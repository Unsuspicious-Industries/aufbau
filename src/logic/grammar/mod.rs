pub mod utils;
pub mod load;
pub mod save;

use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use regex::Regex;

#[derive(Debug, Clone)]
pub enum Symbol {
    Litteral(String),
    Expression(String),
    Regex(Regex),
    Single { value: Box<Symbol>, binding: Option<String>, repetition: Option<RepetitionKind> },
    Group  { symbols: Vec<Symbol>, repetition: Option<RepetitionKind> },
}

impl Eq for Symbol {}

impl PartialEq for Symbol {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Symbol::Litteral(a), Symbol::Litteral(b)) => a == b,
            (Symbol::Expression(a), Symbol::Expression(b)) => a == b,
            (Symbol::Regex(a), Symbol::Regex(b)) => a.as_str() == b.as_str(),
            (Symbol::Single { value: va, binding: ba, repetition: ra }, Symbol::Single { value: vb, binding: bb, repetition: rb }) => va == vb && ba == bb && ra == rb,
            (Symbol::Group { symbols: sa, repetition: ra }, Symbol::Group { symbols: sb, repetition: rb }) => sa == sb && ra == rb,
            _ => false,
        }
    }
}

impl Hash for Symbol {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Symbol::Litteral(s) => {
                0u8.hash(state);
                s.hash(state);
            }
            Symbol::Expression(s) => {
                1u8.hash(state);
                s.hash(state);
            }
            Symbol::Regex(r) => {
                2u8.hash(state);
                r.as_str().hash(state);
            }
            Symbol::Single { value, binding, repetition } => {
                3u8.hash(state);
                value.hash(state);
                binding.hash(state);
                repetition.hash(state);
            }
            Symbol::Group { symbols, repetition } => {
                4u8.hash(state);
                symbols.hash(state);
                repetition.hash(state);
            }
        }
    }
}

#[derive(Debug, Clone, Eq, Hash, PartialEq)]
pub enum RepetitionKind {
    ZeroOrMore,     // *
    OneOrMore,      // +
    ZeroOrOne,      // ?
}

impl Symbol {
    pub fn new(value: String) -> Self { 
        debug_trace!("grammar", "Creating symbol from value: {}", value);
        if value.starts_with('\'') && value.ends_with('\'') {
            Self::Litteral(value[1..value.len()-1].to_string())
        } else if value.starts_with('/') && value.ends_with('/') {
            Self::Regex(Regex::new(&value[1..value.len()-1]).unwrap_or_else(|_| Regex::new("").unwrap()))
        } else {
            Self::Expression(value)
        }
    }
    pub fn with_binding(value: String, binding: String) -> Self { 
        Self::Single { 
            value: Box::new(Self::new(value)), 
            binding: Some(binding), 
            repetition: None 
        } 
    }
    pub fn with_repetition(value: String, repetition: RepetitionKind) -> Self { 
        Self::Single { 
            value: Box::new(Self::new(value)), 
            binding: None, 
            repetition: Some(repetition) 
        } 
    }
    pub fn with_binding_and_repetition(value: String, binding: String, repetition: RepetitionKind) -> Self { 
        Self::Single { 
            value: Box::new(Self::new(value)), 
            binding: Some(binding), 
            repetition: Some(repetition) 
        } 
    }
    pub fn group(symbols: Vec<Symbol>, repetition: Option<RepetitionKind>) -> Self { Self::Group { symbols, repetition } }

    pub fn value(&self) -> String { 
        match self { 
            Symbol::Litteral(value) => value.clone(),
            Symbol::Expression(value) => value.clone(),
            Symbol::Regex(regex) => format!("/{}/", regex.as_str()),
            Symbol::Single { value, .. } => value.value(),
            Symbol::Group { symbols, .. } => {
                let values: Vec<String> = symbols.iter().map(|s| s.value()).collect();
                format!("({})", values.join(" "))
            }
        } 
    }
    pub fn binding(&self) -> Option<&String> { 
        match self { 
            Symbol::Single { binding, .. } => binding.as_ref(), 
            _ => None 
        } 
    }
    pub fn repetition(&self) -> Option<&RepetitionKind> { 
        match self { 
            Symbol::Single { repetition, .. } => repetition.as_ref(), 
            Symbol::Group { repetition, .. } => repetition.as_ref(),
            _ => None
        } 
    }
    pub fn group_symbols(&self) -> Option<&[Symbol]> { match self { Symbol::Group { symbols, .. } => Some(symbols.as_slice()), _ => None } }
    pub fn group_symbols_mut(&mut self) -> Option<&mut Vec<Symbol>> { match self { Symbol::Group { symbols, .. } => Some(symbols), _ => None } }
    pub fn is_group(&self) -> bool { matches!(self, Symbol::Group { .. }) }
    pub fn has_binding(&self) -> bool { self.binding().is_some() }
    pub fn is_litteral(&self) -> bool { matches!(self, Symbol::Litteral(_)) }
    pub fn is_regex(&self) -> bool { matches!(self, Symbol::Regex(_)) }
    pub fn is_terminal(&self) -> bool { self.is_litteral() || self.is_regex() }
    pub fn is_nonterminal(&self) -> bool { !self.is_terminal() }
}

/// Convenience alias for non-terminal symbols.
pub type Nonterminal = String;
/// A single production rule `left ::= right₀ right₁ …`.
#[derive(Debug, Clone, PartialEq)]
pub struct Production {
    pub rule: Option<String>,
    pub rhs: Vec<Symbol>,
}

use crate::debug_trace;
use crate::logic::typing::TypingRule;

/// A complete grammar consisting of context-free productions and
/// inference-style typing rules.
#[derive(Debug, Clone, Default, PartialEq)]
pub struct Grammar {
    pub productions: HashMap<Nonterminal, Vec<Production>>,
    pub typing_rules: HashMap<String, TypingRule>, // name -> rule
    pub special_tokens: Vec<String>,
    // Optional explicit start nonterminal for parsing
    pub start: Option<Nonterminal>,
    // Preserve declaration order of productions as they appear in the spec
    pub production_order: Vec<Nonterminal>,
}

impl Grammar {
    /// Create an empty grammar
    pub fn new() -> Self {
        Self::default()
    }

    /// Add a special token to the grammar if not already present.
    pub fn add_special_token(&mut self, token: String) {
        if !self.special_tokens.contains(&token) {
            self.special_tokens.push(token);
        }
    }

    /// Add a typing rule to the grammar.
    pub fn add_typing_rule(&mut self, rule: TypingRule) {
        self.typing_rules.insert(rule.name.clone(), rule);
    }

    /// Set the start nonterminal.
    pub fn set_start<S: Into<Nonterminal>>(&mut self, start: S) {
        self.start = Some(start.into());
    }

    /// Get the start nonterminal if available.
    pub fn start_nonterminal(&self) -> Option<&Nonterminal> {
        self.start.as_ref()
    }
}

#[cfg(test)]
pub(crate) mod tests {
    use super::*;
    use crate::logic::typing;  // Needed for tests
    use crate::logic::parser::Parser;
    use crate::set_debug_level;

    pub const STLC_SPEC: &str = r#"

    // Identifier (supports Unicode)
    Identifier ::= /[\p{L}][\p{L}\p{N}_τ₁₂₃₄₅₆₇₈₉₀]*/

    // Variables with var typing rule
    Variable(var) ::= Identifier[x]

    // Type names (supports Unicode type variables like τ₁, τ₂)
    TypeName ::= Identifier

    // Base types (parentheses are literals, hence quoted)
    BaseType ::= TypeName | '(' Type ')'

    // Function types (right-associative)
    Type ::= BaseType[τ₁] '->' Type[τ₂] | BaseType[τ]

    // Typed parameter
    TypedParam ::= Variable[x] ':' Type[τ]

    // Lambda abstraction (dot is a literal)
    Lambda(lambda) ::= 'λ' TypedParam '.' Term[e]
    

    // Base terms (cannot be applications; parentheses are literal tokens)
    BaseTerm ::= Variable | Lambda | '(' Term ')'

    // Applications (left-associative via iteration)
    Application(app) ::= BaseTerm[f] BaseTerm[e]

    // Terms
    Term ::= Application[e] | BaseTerm[e]

    // Typing Rules
    x ∈ Γ
    ----------- (var)
    Γ(x)

    Γ[x:τ₁] ⊢ e : τ₂
    --------------------------- (lambda)
    τ₁ → τ₂

    Γ ⊢ f : τ₁ → τ₂, Γ ⊢ e : τ₁
    -------------------------------- (app)
    τ₂
    "#;

    #[test]
    fn parses_unified_stlc_grammar() {
        let grammar = Grammar::load(STLC_SPEC).expect("Parser failed");

        // Check productions
        assert!(grammar.productions.contains_key("Variable"));
        assert!(grammar.productions.contains_key("Lambda"));
        let lambda_prods = grammar.productions.get("Lambda").unwrap();
        assert_eq!(lambda_prods.len(), 1);
        assert_eq!(lambda_prods[0].rule, Some("lambda".to_string()));
        
        let typed_param_prod = grammar.productions.get("TypedParam").unwrap();
        let var_symbol = typed_param_prod[0].rhs.iter().find(|s| s.value() == "Variable").unwrap();
        assert_eq!(var_symbol.binding().cloned(), Some("x".to_string()));

        // Check typing rules
        assert_eq!(grammar.typing_rules.len(), 3);
        assert!(grammar.typing_rules.contains_key("var"));
        assert!(grammar.typing_rules.contains_key("lambda"));
        assert!(grammar.typing_rules.contains_key("app"));
        
        let var_rule = grammar.typing_rules.get("var").unwrap();
        match &var_rule.conclusion.kind {
            typing::rule::ConclusionKind::ContextLookup(ctx, v) => {
                assert_eq!(ctx, "Γ");
                assert_eq!(v, "x");
            }
            _ => panic!("Expected context lookup conclusion for var rule"),
        }
        assert_eq!(var_rule.premises.len(), 1);
        match &var_rule.premises[0] {
            typing::Premise { setting: None, judgment: Some(typing::TypingJudgment::Membership(var, ctx)) } => {
                assert_eq!(var, "x");
                assert_eq!(ctx, "Γ");
            }
            _ => panic!("Expected membership judgment for var rule"),
        }
        
        let lambda_rule = grammar.typing_rules.get("lambda").unwrap();
        match &lambda_rule.conclusion.kind {
            typing::rule::ConclusionKind::Type(ty) => assert_eq!(format!("{}", ty), "τ₁ → τ₂"),
            _ => panic!("Expected type conclusion for lambda rule"),
        }
        assert_eq!(lambda_rule.premises.len(), 1);
        match &lambda_rule.premises[0] {
            typing::Premise { setting, judgment: Some(typing::TypingJudgment::Ascription((term, ty))) } => {
                let setting = setting.as_ref().unwrap();
                assert_eq!(setting.name, "Γ");
                assert_eq!(setting.extensions.len(), 1);
                assert_eq!(setting.extensions[0].0, "x");
                assert_eq!(format!("{}", setting.extensions[0].1), "τ₁");
                assert_eq!(term, "e");
                assert_eq!(format!("{}", ty), "τ₂");
            }
            _ => panic!("Expected ascription judgment for lambda rule"),
        }
        
        let app_rule = grammar.typing_rules.get("app").unwrap();
        match &app_rule.conclusion.kind {
            typing::rule::ConclusionKind::Type(ty) => assert_eq!(format!("{}", ty), "τ₂"),
            _ => panic!("Expected type conclusion for app rule"),
        }
        assert_eq!(app_rule.premises.len(), 2);
        match &app_rule.premises[0] {
            typing::Premise { setting, judgment: Some(typing::TypingJudgment::Ascription((term, ty))) } => {
                assert!(setting.is_none() || setting.as_ref().unwrap().extensions.is_empty());
                assert_eq!(term, "f");
                assert_eq!(format!("{}", ty), "τ₁ → τ₂");
            }
            _ => panic!("Expected ascription judgment for app rule premise 0"),
        }
        match &app_rule.premises[1] {
            typing::Premise { setting, judgment: Some(typing::TypingJudgment::Ascription((term, ty))) } => {
                assert!(setting.is_none() || setting.as_ref().unwrap().extensions.is_empty());
                assert_eq!(term, "e");
                assert_eq!(format!("{}", ty), "τ₁");
            }
            _ => panic!("Expected ascription judgment for app rule premise 1"),
        }

        // Start symbol should be the last production LHS (convention)
        assert_eq!(grammar.start_nonterminal().cloned(), Some("Term".to_string()));
        assert_eq!(grammar.production_order.last(), Some(&"Term".to_string()));
    }

    #[test]
    fn roundtrip_write_and_parse() {
        let g1 = Grammar::load(STLC_SPEC).expect("parse");
        let spec = g1.to_spec_string();
        let g2 = Grammar::load(&spec).expect("re-parse");

        assert_eq!(g1.productions.len(), g2.productions.len());
        assert_eq!(g1.typing_rules.len(), g2.typing_rules.len());
        
        for key in g1.productions.keys() {
            assert!(g2.productions.contains_key(key));
            assert_eq!(g1.productions[key], g2.productions[key]);
        }
        for key in g1.typing_rules.keys() {
            assert!(g2.typing_rules.contains_key(key));
            assert_eq!(g1.typing_rules[key], g2.typing_rules[key]);
        }
        let mut t1 = g1.special_tokens.clone();
        let mut t2 = g2.special_tokens.clone();
        t1.sort();
        t2.sort();
        assert_eq!(t1, t2);
        assert_eq!(g1.start_nonterminal(), g2.start_nonterminal());
    }

    #[test]
    fn test_parse_repetition_suffix() {
        use crate::logic::grammar::utils::parse_repetition_suffix;
        
        let (b,r)=parse_repetition_suffix("Stmt*");
        assert_eq!(b,"Stmt");
        assert_eq!(r,Some(RepetitionKind::ZeroOrMore));

        let (b,r)=parse_repetition_suffix("Term+");
        assert_eq!(b,"Term");
        assert_eq!(r,Some(RepetitionKind::OneOrMore));

        let (b,r)=parse_repetition_suffix("Expr?");
        assert_eq!(b,"Expr");
        assert_eq!(r,Some(RepetitionKind::ZeroOrOne));

        let (b,r)=parse_repetition_suffix("Variable");
        assert_eq!(b,"Variable");
        assert_eq!(r,None);
    }

    #[test]
    fn test_parse_rhs_with_repetition() {
        use crate::logic::grammar::utils::parse_rhs;
        
        let result = parse_rhs("Stmt*").unwrap();
        assert_eq!(result[0][0].value(), "Stmt");
        assert_eq!(result[0][0].repetition(), Some(&RepetitionKind::ZeroOrMore));

        let result = parse_rhs("'{' Stmt* '}'").unwrap();
        assert_eq!(result[0][0].value(), "{");
        assert_eq!(result[0][1].value(), "Stmt");
        assert_eq!(result[0][1].repetition(), Some(&RepetitionKind::ZeroOrMore));
        assert_eq!(result[0][2].value(), "}");
    }

    #[test]
    fn test_parse_rhs_with_binding_and_repetition() {
        use crate::logic::grammar::utils::parse_rhs;
        
        let result = parse_rhs("'{' Stmt[s]* '}'").unwrap();
        assert_eq!(result[0][1].value(), "Stmt");
        assert_eq!(result[0][1].binding().cloned(), Some("s".into()));
        assert_eq!(result[0][1].repetition(), Some(&RepetitionKind::ZeroOrMore));
    }

    const REPETITION_GRAMMAR: &str = r#"
    Stmt ::= 'print' Expr ';'
    Expr ::= Number | Variable
    Number ::= /[0-9]+/
    Variable ::= /[a-zA-Z][a-zA-Z0-9]*/
    Block ::= '{' Stmt* '}'
    "#;

    #[test]
    fn test_grammar_with_repetition_loads() {
        let g = Grammar::load(REPETITION_GRAMMAR).unwrap();
        let block = g.productions.get("Block").unwrap();
        assert_eq!(block[0].rhs[1].value(), "Stmt");
        assert_eq!(block[0].rhs[1].repetition(), Some(&RepetitionKind::ZeroOrMore));
    }

    #[test]
    fn test_parse_empty_block() {
        crate::logic::debug::set_debug_level(
            crate::logic::debug::DebugLevel::Trace
        );
        let g = Grammar::load(REPETITION_GRAMMAR).unwrap();
        let mut p = Parser::new(g);
        let ast = p.parse("{ }").unwrap();
        if let crate::logic::ast::ASTNode::Nonterminal(nt)= ast.clone() {
            assert_eq!(nt.value, "Block");
            assert_eq!(nt.children.len(),2);
        } else {
            panic!("Expected nonterminal")
        }
        println!("Parsed empty block successfully: {}", ast.pretty());
    }

    #[test]
    fn test_parse_block_with_statements() {
        
        let g = Grammar::load(REPETITION_GRAMMAR).unwrap();
        let mut p = Parser::new(g);
        let ast = p.parse("{ print x ; print y ; }").unwrap();
        if let crate::logic::ast::ASTNode::Nonterminal(nt)=ast.clone() {
            assert_eq!(nt.value, "Block");
            assert!(nt.children.len()>2);
        } else {
            panic!("Expected nonterminal")
        }
        println!("Parsed empty block successfully: {}", ast.pretty());
    }

    #[test]
    fn test_parse_all_repetition_operators() {
        use crate::logic::grammar::utils::parse_rhs;
        
        let r = parse_rhs("Term* Expr+ Stmt?").unwrap();
        assert_eq!(r[0][0].repetition(), Some(&RepetitionKind::ZeroOrMore));
        assert_eq!(r[0][1].repetition(), Some(&RepetitionKind::OneOrMore));
        assert_eq!(r[0][2].repetition(), Some(&RepetitionKind::ZeroOrOne));
    }

    #[test]
    fn test_grouped_repetition_parsing_inline() {
        let spec = r#"
        ArgList ::= (Arg (',' Arg)*)?
        Arg ::= Identifier
        Identifier ::= /[a-zA-Z_][a-zA-Z0-9_]*/
        "#;
        let g = Grammar::load(spec).unwrap();
        assert!(g.productions.contains_key("ArgList"));
        assert!(!g.productions.keys().any(|k| k.starts_with("__group")));
        let rhs=&g.productions.get("ArgList").unwrap()[0].rhs;
        assert_eq!(rhs.len(),1);
        assert!(rhs[0].is_group());
        assert_eq!(rhs[0].repetition(), Some(&RepetitionKind::ZeroOrOne));
        let inner = rhs[0].group_symbols().unwrap();
        assert_eq!(inner.len(),2);
        assert_eq!(inner[0].value(), "Arg");
        assert_eq!(inner[1].repetition(), Some(&RepetitionKind::ZeroOrMore));
    }

    #[test]
    fn test_grouped_repetition_runtime_inline() {
        use crate::{set_debug_level,set_debug_input,debug_info};
        use crate::logic::debug::DebugLevel;
        let spec = r#"
        Identifier ::= /[a-zA-Z_][a-zA-Z0-9_]*/
        Arg ::= Identifier
        ArgList ::= (Arg (',' Arg)*)?
        "#;
        let g = Grammar::load(spec).unwrap();
        assert_eq!(g.start_nonterminal().cloned(), Some("ArgList".into()));
        set_debug_level(DebugLevel::None);
        let run = |gr:&Grammar, input:&str| {
            set_debug_input(Some(input.into()));
            let mut p = Parser::new(gr.clone());
            match p.parse(input){
                Ok(ast)=> if let Some(nt)=ast.as_nonterminal(){ debug_info!("test","Parsed '{}': {}", input, nt.value)},
                Err(e)=> panic!("Failed to parse '{}': {}", input, e)
            };
        };
        run(&g,"x");
        run(&g,"x, y");
        run(&g,"x, y, z");
    }
}
