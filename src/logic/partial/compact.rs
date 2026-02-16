//! Compact binary representation of parse trees
//!
//! This module provides a highly compressed representation of parse trees that leverages
//! the grammar context to minimize storage. It stores only:
//! - Production alternative indices
//! - Consumed segments counts
//! - Terminal values and bindings
//! - Terminal completion status (Complete vs Partial)
//!
//! NonTerminal names are NOT stored, we assume its parsed with grammar data
//!
//! This is kinda useless, forgot why i wrote it

use crate::logic::grammar::{Grammar, Production};
use crate::logic::partial::structure::{Node, NonTerminal, Terminal};
use std::io;

/// Compact binary representation of a parse tree
/// Format is optimized for size, not human readability
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct CompactTree {
    /// Binary encoded tree data
    data: Vec<u8>,
}

impl CompactTree {
    /// Encode a NonTerminal into compact binary format
    pub fn encode(nt: &NonTerminal, grammar: &Grammar) -> Result<Self, String> {
        let mut data = Vec::new();
        encode_nonterminal(nt, grammar, &mut data).map_err(|e| format!("Encoding error: {}", e))?;
        Ok(CompactTree { data })
    }

    /// Decode back to a NonTerminal given the grammar context
    pub fn decode(&self, grammar: &Grammar, start_symbol: &str) -> Result<NonTerminal, String> {
        let mut cursor = &self.data[..];
        decode_nonterminal(&mut cursor, grammar, start_symbol)
            .map_err(|e| format!("Decoding error: {}", e))
    }

    /// Get the raw binary data
    pub fn data(&self) -> &[u8] {
        &self.data
    }

    /// Get size in bytes
    pub fn size(&self) -> usize {
        self.data.len()
    }

    /// Create from raw binary data
    pub fn from_data(data: Vec<u8>) -> Self {
        CompactTree { data }
    }
}

// ============================================================================
// Encoding
// ============================================================================

fn encode_nonterminal(nt: &NonTerminal, grammar: &Grammar, buf: &mut Vec<u8>) -> io::Result<()> {
    // Encode: alternative_index (varint)
    write_varint(buf, nt.alternative_index as u64)?;

    // Encode: consumed_segments (varint)
    write_varint(buf, nt.consumed_segments as u64)?;

    // Encode: binding (optional string)
    encode_optional_string(&nt.binding, buf)?;

    // Encode: number of children (varint)
    write_varint(buf, nt.children.len() as u64)?;

    // Encode: each child
    for child in &nt.children {
        encode_node(child, grammar, buf)?;
    }

    Ok(())
}

fn encode_node(node: &Node, grammar: &Grammar, buf: &mut Vec<u8>) -> io::Result<()> {
    match node {
        Node::NonTerminal(nt) => {
            // Tag: 0 = NonTerminal
            buf.push(0);
            encode_nonterminal(nt, grammar, buf)?;
        }
        Node::Terminal(term) => {
            // Tag: 1 = Terminal
            buf.push(1);
            encode_terminal(term, buf)?;
        }
    }
    Ok(())
}

fn encode_terminal(term: &Terminal, buf: &mut Vec<u8>) -> io::Result<()> {
    match term {
        Terminal::Complete {
            value,
            binding,
            extension,
        } => {
            // Tag: 0 = Complete
            buf.push(0);

            // Value (string)
            encode_string(value, buf)?;

            // Binding (optional string)
            encode_optional_string(binding, buf)?;

            // Extension (optional regex pattern)
            if let Some(ext) = extension {
                buf.push(1); // has extension
                encode_string(&ext.to_pattern(), buf)?;
            } else {
                buf.push(0); // no extension
            }
        }
        Terminal::Partial {
            value,
            binding,
            remainder,
        } => {
            // Tag: 1 = Partial
            buf.push(1);

            // Value (string)
            encode_string(value, buf)?;

            // Binding (optional string)
            encode_optional_string(binding, buf)?;

            // Remainder (optional regex pattern)
            if let Some(rem) = remainder {
                buf.push(1); // has remainder
                encode_string(&rem.to_pattern(), buf)?;
            } else {
                buf.push(0); // no remainder
            }
        }
    }
    Ok(())
}

fn encode_string(s: &str, buf: &mut Vec<u8>) -> io::Result<()> {
    let bytes = s.as_bytes();
    write_varint(buf, bytes.len() as u64)?;
    buf.extend_from_slice(bytes);
    Ok(())
}

fn encode_optional_string(opt: &Option<String>, buf: &mut Vec<u8>) -> io::Result<()> {
    match opt {
        Some(s) => {
            buf.push(1); // has value
            encode_string(s, buf)?;
        }
        None => {
            buf.push(0); // no value
        }
    }
    Ok(())
}

/// Write a variable-length integer (LEB128 encoding)
fn write_varint(buf: &mut Vec<u8>, mut value: u64) -> io::Result<()> {
    loop {
        let mut byte = (value & 0x7F) as u8;
        value >>= 7;
        if value != 0 {
            byte |= 0x80; // More bytes follow
        }
        buf.push(byte);
        if value == 0 {
            break;
        }
    }
    Ok(())
}

// ============================================================================
// Decoding
// ============================================================================

fn decode_nonterminal(
    cursor: &mut &[u8],
    grammar: &Grammar,
    nt_name: &str,
) -> io::Result<NonTerminal> {
    // Get the production for this nonterminal
    let productions = grammar.productions.get(nt_name).ok_or_else(|| {
        io::Error::new(
            io::ErrorKind::InvalidData,
            format!("Unknown nonterminal: {}", nt_name),
        )
    })?;

    // Decode: alternative_index
    let alternative_index = read_varint(cursor)? as usize;

    if alternative_index >= productions.len() {
        return Err(io::Error::new(
            io::ErrorKind::InvalidData,
            format!(
                "Invalid alternative index {} for nonterminal {}",
                alternative_index, nt_name
            ),
        ));
    }

    let production = productions[alternative_index].clone();

    // Decode: consumed_segments
    let consumed_segments = read_varint(cursor)? as usize;

    // Decode: binding
    let binding = decode_optional_string(cursor)?;

    // Decode: number of children
    let num_children = read_varint(cursor)? as usize;

    // Decode: each child
    let mut children = Vec::with_capacity(num_children);
    let mut symbol_idx = 0;

    for _ in 0..num_children {
        let child = decode_node(cursor, grammar, &production, &mut symbol_idx)?;
        children.push(child);
    }

    Ok(NonTerminal {
        name: nt_name.to_string(),
        production,
        alternative_index,
        children,
        binding,
        consumed_segments,
    })
}

fn decode_node(
    cursor: &mut &[u8],
    grammar: &Grammar,
    production: &Production,
    symbol_idx: &mut usize,
) -> io::Result<Node> {
    // Read tag
    let tag = read_byte(cursor)?;

    match tag {
        0 => {
            // NonTerminal - need to determine its name from the production
            if *symbol_idx >= production.rhs.len() {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidData,
                    "Symbol index out of bounds",
                ));
            }

            let symbol = &production.rhs[*symbol_idx];
            *symbol_idx += 1;

            // Symbol must be a nonterminal reference
            let nt_name = if let Some(name) = symbol.as_nonterminal() {
                name
            } else {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidData,
                    "Expected nonterminal symbol",
                ));
            };

            let nt = decode_nonterminal(cursor, grammar, nt_name)?;
            Ok(Node::NonTerminal(nt))
        }
        1 => {
            // Terminal
            *symbol_idx += 1; // Skip past the terminal symbol in production
            let term = decode_terminal(cursor)?;
            Ok(Node::Terminal(term))
        }
        _ => Err(io::Error::new(
            io::ErrorKind::InvalidData,
            format!("Invalid node tag: {}", tag),
        )),
    }
}

fn decode_terminal(cursor: &mut &[u8]) -> io::Result<Terminal> {
    let tag = read_byte(cursor)?;

    match tag {
        0 => {
            // Complete terminal
            let value = decode_string(cursor)?;
            let binding = decode_optional_string(cursor)?;

            let extension = if read_byte(cursor)? == 1 {
                let pattern = decode_string(cursor)?;
                Some(crate::regex::Regex::new(&pattern).map_err(|e| {
                    io::Error::new(
                        io::ErrorKind::InvalidData,
                        format!("Invalid regex pattern: {:?}", e),
                    )
                })?)
            } else {
                None
            };

            Ok(Terminal::Complete {
                value,
                binding,
                extension,
            })
        }
        1 => {
            // Partial terminal
            let value = decode_string(cursor)?;
            let binding = decode_optional_string(cursor)?;

            let remainder = if read_byte(cursor)? == 1 {
                let pattern = decode_string(cursor)?;
                Some(crate::regex::Regex::new(&pattern).map_err(|e| {
                    io::Error::new(
                        io::ErrorKind::InvalidData,
                        format!("Invalid regex pattern: {:?}", e),
                    )
                })?)
            } else {
                None
            };

            Ok(Terminal::Partial {
                value,
                binding,
                remainder,
            })
        }
        _ => Err(io::Error::new(
            io::ErrorKind::InvalidData,
            format!("Invalid terminal tag: {}", tag),
        )),
    }
}

fn decode_string(cursor: &mut &[u8]) -> io::Result<String> {
    let len = read_varint(cursor)? as usize;

    if cursor.len() < len {
        return Err(io::Error::new(
            io::ErrorKind::UnexpectedEof,
            "Not enough bytes for string",
        ));
    }

    let bytes = &cursor[..len];
    *cursor = &cursor[len..];

    String::from_utf8(bytes.to_vec())
        .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, format!("Invalid UTF-8: {}", e)))
}

fn decode_optional_string(cursor: &mut &[u8]) -> io::Result<Option<String>> {
    let has_value = read_byte(cursor)?;
    match has_value {
        0 => Ok(None),
        1 => Ok(Some(decode_string(cursor)?)),
        _ => Err(io::Error::new(
            io::ErrorKind::InvalidData,
            "Invalid optional string tag",
        )),
    }
}

fn read_byte(cursor: &mut &[u8]) -> io::Result<u8> {
    if cursor.is_empty() {
        return Err(io::Error::new(
            io::ErrorKind::UnexpectedEof,
            "Unexpected end of data",
        ));
    }
    let byte = cursor[0];
    *cursor = &cursor[1..];
    Ok(byte)
}

/// Read a variable-length integer (LEB128 decoding)
fn read_varint(cursor: &mut &[u8]) -> io::Result<u64> {
    let mut result: u64 = 0;
    let mut shift: u32 = 0;

    loop {
        if shift >= 64 {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                "Varint too long",
            ));
        }

        let byte = read_byte(cursor)?;
        result |= ((byte & 0x7F) as u64) << shift;

        if (byte & 0x80) == 0 {
            break;
        }

        shift += 7;
    }

    Ok(result)
}

// ============================================================================
// Helper for grammar symbols
// ============================================================================

impl crate::logic::grammar::Symbol {
    fn as_nonterminal(&self) -> Option<&str> {
        use crate::logic::grammar::Symbol;
        match self {
            Symbol::Nonterminal { name, .. } => Some(name),
            _ => None,
        }
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    use crate::logic::grammar::Grammar;
    use crate::logic::partial::parse::Parser;

    #[test]
    fn test_compact_roundtrip_simple() {
        let grammar_str = r#"
        start ::= "hello" "world"
        "#;

        let grammar = Grammar::load(grammar_str).unwrap();
        let mut parser = Parser::new(grammar.clone());
        let result = parser.parse("helloworld").unwrap();

        assert!(result.is_complete());
        let tree = &result.roots[0];

        // Encode
        let compact = CompactTree::encode(tree, &grammar).unwrap();

        println!("Original size: {} bytes", tree.serialize().len());
        println!("Compact size: {} bytes", compact.size());

        // Decode
        let decoded = compact.decode(&grammar, "start").unwrap();

        // Compare
        assert_eq!(tree.name, decoded.name);
        assert_eq!(tree.alternative_index, decoded.alternative_index);
        assert_eq!(tree.children.len(), decoded.children.len());
    }

    #[test]
    fn test_compact_stlc_variable() {
        let grammar_str = include_str!("../../../examples/stlc.auf");
        let grammar = Grammar::load(grammar_str).unwrap();

        let mut parser = Parser::new(grammar.clone());
        let result = parser.parse("x").unwrap();

        assert!(result.is_complete());
        let tree = &result.roots[0];

        // Encode
        let compact = CompactTree::encode(tree, &grammar).unwrap();

        println!("\nSTLC variable 'x':");
        println!("Serialized size: {} bytes", tree.serialize().len());
        println!("Compact size: {} bytes", compact.size());
        println!(
            "Compression ratio: {:.2}%",
            (compact.size() as f64 / tree.serialize().len() as f64) * 100.0
        );

        // Decode
        let decoded = compact.decode(&grammar, "Expression").unwrap();

        // Compare structurally
        assert_eq!(tree.name, decoded.name);
        assert_eq!(tree.alternative_index, decoded.alternative_index);
    }

    #[test]
    fn test_compact_stlc_lambda() {
        let grammar_str = include_str!("../../../examples/stlc.auf");
        let grammar = Grammar::load(grammar_str).unwrap();

        let mut parser = Parser::new(grammar.clone());
        let result = parser.parse("λx:A.x").unwrap();

        assert!(result.is_complete());
        let tree = &result.roots[0];

        // Encode
        let compact = CompactTree::encode(tree, &grammar).unwrap();

        println!("\nSTLC lambda 'λx:A.x':");
        println!("Serialized size: {} bytes", tree.serialize().len());
        println!("Compact size: {} bytes", compact.size());
        println!(
            "Compression ratio: {:.2}%",
            (compact.size() as f64 / tree.serialize().len() as f64) * 100.0
        );

        // Decode
        let decoded = compact.decode(&grammar, "Expression").unwrap();

        // Compare structurally
        assert_eq!(tree.name, decoded.name);
        assert_eq!(tree.alternative_index, decoded.alternative_index);
    }

    #[test]
    fn test_compact_stlc_application() {
        let grammar_str = include_str!("../../../examples/stlc.auf");
        let grammar = Grammar::load(grammar_str).unwrap();

        let mut parser = Parser::new(grammar.clone());
        let result = parser.parse("f x y").unwrap();

        assert!(result.is_complete());
        let tree = &result.roots[0];

        // Encode
        let compact = CompactTree::encode(tree, &grammar).unwrap();
        println!("Compact encoded size: {} bytes", compact.size());

        println!("\nSTLC application 'f x y':");
        println!("Serialized size: {} bytes", tree.serialize().len());
        println!("Compact size: {} bytes", compact.size());
        println!(
            "Compression ratio: {:.2}%",
            (compact.size() as f64 / tree.serialize().len() as f64) * 100.0
        );

        // Decode
        let decoded = compact.decode(&grammar, "Expression").unwrap();

        // Compare structurally
        assert_eq!(tree.name, decoded.name);
        assert_eq!(tree.alternative_index, decoded.alternative_index);
    }

    #[test]
    fn test_varint_encoding() {
        let test_values = vec![0u64, 1, 127, 128, 255, 256, 16383, 16384, u64::MAX];

        for value in test_values {
            let mut buf = Vec::new();
            write_varint(&mut buf, value).unwrap();

            let mut cursor = &buf[..];
            let decoded = read_varint(&mut cursor).unwrap();

            assert_eq!(value, decoded, "Varint roundtrip failed for {}", value);
        }
    }

    #[test]
    fn test_string_encoding() {
        let test_strings = vec!["", "hello", "λx.x", "日本語"];

        for s in test_strings {
            let mut buf = Vec::new();
            encode_string(s, &mut buf).unwrap();
            println!("Encoded string '{}' into {} bytes", s, buf.len());

            let mut cursor = &buf[..];
            let decoded = decode_string(&mut cursor).unwrap();

            assert_eq!(s, decoded);
        }
    }
}
