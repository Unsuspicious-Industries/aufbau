use crate::logic::ast::{ASTNode, NonTerminal, Terminal};
use crate::logic::grammar::Grammar;
// ---------------- S-expression parsing helpers ----------------

#[derive(Debug, Clone, PartialEq)]
pub enum SExpr {
    Atom(String),
    Str(String),
    List(Vec<SExpr>),
}

pub fn strip_headers(input: &str) -> &str {
    let mut offset = 0usize;
    for line in input.lines() {
        let trimmed = line.trim_start();
        if trimmed.starts_with(';') || trimmed.is_empty() {
            offset += line.len();
            offset += 1; // account for newline
            continue;
        } else {
            return &input[offset..];
        }
    }
    input
}

pub fn parse_sexpr(input: &str) -> Result<SExpr, String> {
    enum Tok<'a> {
        LParen,
        RParen,
        Atom(&'a str),
        Str(String),
    }

    fn tokenize(s: &str) -> Result<Vec<Tok<'_>>, String> {
        let mut toks = Vec::new();
        let mut chars = s.char_indices().peekable();

        while let Some((byte_idx, ch)) = chars.next() {
            match ch {
                c if c.is_whitespace() => {
                    continue;
                }
                ';' => {
                    // comment to end of line
                    while let Some((_, ch)) = chars.peek() {
                        if *ch == '\n' {
                            break;
                        }
                        chars.next();
                    }
                }
                '(' => {
                    toks.push(Tok::LParen);
                }
                ')' => {
                    toks.push(Tok::RParen);
                }
                '"' => {
                    let mut buf = String::new();
                    let mut escaped = false;
                    while let Some((_, ch)) = chars.next() {
                        if escaped {
                            match ch {
                                '"' => buf.push('"'),
                                '\\' => buf.push('\\'),
                                'n' => buf.push('\n'),
                                't' => buf.push('\t'),
                                other => buf.push(other),
                            }
                            escaped = false;
                        } else if ch == '\\' {
                            escaped = true;
                        } else if ch == '"' {
                            break;
                        } else {
                            buf.push(ch);
                        }
                    }
                    toks.push(Tok::Str(buf));
                }
                _ => {
                    let start_byte = byte_idx;
                    let mut end_byte = byte_idx + ch.len_utf8();

                    while let Some((byte_idx, ch)) = chars.peek() {
                        if ch.is_whitespace() || *ch == '(' || *ch == ')' || *ch == '"' {
                            break;
                        }
                        end_byte = *byte_idx + ch.len_utf8();
                        chars.next();
                    }

                    let atom = &s[start_byte..end_byte];
                    toks.push(Tok::Atom(atom));
                }
            }
        }
        Ok(toks)
    }

    fn parse_list<'a>(toks: &[Tok<'a>], pos: &mut usize) -> Result<SExpr, String> {
        let mut items = Vec::new();
        while *pos < toks.len() {
            match toks[*pos] {
                Tok::RParen => {
                    *pos += 1;
                    break;
                }
                Tok::LParen => {
                    *pos += 1;
                    items.push(parse_list(toks, pos)?);
                }
                Tok::Atom(a) => {
                    *pos += 1;
                    items.push(SExpr::Atom(a.to_string()));
                }
                Tok::Str(ref s) => {
                    *pos += 1;
                    items.push(SExpr::Str(s.clone()));
                }
            }
        }
        Ok(SExpr::List(items))
    }

    let toks = tokenize(input)?;
    let mut pos = 0;
    match toks.get(pos) {
        Some(Tok::LParen) => {
            pos += 1;
            parse_list(&toks, &mut pos)
        }
        _ => Err("Expected '('".into()),
    }
}

pub fn sexpr_to_ast(sexpr: &SExpr, grammar: &Grammar) -> Result<ASTNode, String> {
    match sexpr {
        SExpr::List(items) if !items.is_empty() => {
            match &items[0] {
                SExpr::Atom(tag) if tag == "T" => {
                    if items.len() < 2 {
                        return Err("(T ...) requires a value".into());
                    }
                    let value = match &items[1] {
                        SExpr::Str(s) => s.clone(),
                        SExpr::Atom(a) => a.clone(),
                        _ => return Err("terminal value must be atom or string".into()),
                    };
                    let mut binding: Option<String> = None;
                    for extra in &items[2..] {
                        if let SExpr::List(pair) = extra {
                            if pair.len() == 2 {
                                if let SExpr::Atom(k) = &pair[0] {
                                    match k.as_str() {
                                        "b" => {
                                            binding = Some(sexpr_atom_or_str(&pair[1])?);
                                        }
                                        _ => {}
                                    }
                                }
                            }
                        }
                    }
                    Ok(ASTNode::Terminal(Terminal {
                        value,
                        span: None,
                        binding,
                    }))
                }
                SExpr::Atom(tag) if tag == "N" => {
                    if items.len() < 2 {
                        return Err("(N ...) requires a name".into());
                    }
                    let name = sexpr_atom(&items[1])?;
                    let mut binding: Option<String> = None;
                    let mut children: Vec<ASTNode> = Vec::new();
                    for extra in &items[2..] {
                        match extra {
                            SExpr::List(pair) if pair.len() == 2 => {
                                if let SExpr::Atom(k) = &pair[0] {
                                    match k.as_str() {
                                        "b" => {
                                            binding = Some(sexpr_atom_or_str(&pair[1])?);
                                        }
                                        "rule" => {
                                            // Rules will be resolved during parsing/binding phase
                                            let _rname = sexpr_atom_or_str(&pair[1])?;
                                        }
                                        _ => {
                                            // This is not a known metadata key, treat as child node
                                            children.push(sexpr_to_ast(extra, grammar)?);
                                        }
                                    }
                                } else {
                                    // First element is not an atom, treat as child node
                                    children.push(sexpr_to_ast(extra, grammar)?);
                                }
                            }
                            other => {
                                children.push(sexpr_to_ast(other, grammar)?);
                            }
                        }
                    }
                    Ok(ASTNode::Nonterminal(NonTerminal {
                        value: name,
                        span: None,
                        children,
                        binding,
                        bound_typing_rule: None,
                    }))
                }
                other => Err(format!("Unknown node tag: {:?}", other)),
            }
        }
        _ => Err("Invalid S-expression; expected list".into()),
    }
}

pub fn sexpr_atom(s: &SExpr) -> Result<String, String> {
    match s {
        SExpr::Atom(a) => Ok(a.clone()),
        _ => Err("expected atom".into()),
    }
}

pub fn sexpr_atom_or_str(s: &SExpr) -> Result<String, String> {
    match s {
        SExpr::Atom(a) => Ok(a.clone()),
        SExpr::Str(t) => Ok(t.clone()),
        _ => Err("expected atom or string".into()),
    }
}
