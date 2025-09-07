pub mod utils;
pub mod load;
pub mod save;

use std::collections::HashMap;

#[derive(Debug, Clone, Eq, Hash, PartialEq)]
pub struct Symbol {
    pub value: String,
    pub binding: Option<String>,
    pub repetition: Option<RepetitionKind>,
}

#[derive(Debug, Clone, Eq, Hash, PartialEq)]
pub enum RepetitionKind {
    ZeroOrMore,     // *
    OneOrMore,      // +
    ZeroOrOne,      // ?
}

impl Symbol {
    pub fn new(value: String) -> Self {
        Symbol { value, binding: None, repetition: None }
    }
    
    pub fn with_binding(value: String, binding: String) -> Self {
        Symbol { value, binding: Some(binding), repetition: None }
    }
    
    pub fn with_repetition(value: String, repetition: RepetitionKind) -> Self {
        Symbol { value, binding: None, repetition: Some(repetition) }
    }
    
    pub fn with_binding_and_repetition(value: String, binding: String, repetition: RepetitionKind) -> Self {
        Symbol { value, binding: Some(binding), repetition: Some(repetition) }
    }
}

/// Convenience alias for non-terminal symbols.
pub type Nonterminal = String;
/// Convenience alias for terminal symbols.
pub type Terminal = String;
/// A single production rule `left ::= right₀ right₁ …`.
#[derive(Debug, Clone, PartialEq)]
pub struct Production {
    pub rule: Option<String>,
    pub rhs: Vec<Symbol>,
}

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

    pub const STLC_SPEC: &str = r#"

    // Identifier (supports Unicode)
    Identifier ::= /[\p{L}][\p{L}\p{N}_τ₁₂₃₄₅₆₇₈₉₀]*/

    // Variables with var typing rule
    Variable(var) ::= Identifier[x]

    // Type names (supports Unicode type variables like τ₁, τ₂)
    TypeName ::= Identifier

    // Base types
    BaseType ::= TypeName | '(' Type ')'

    // Function types (right-associative)
    Type ::= BaseType[τ₁] '->' Type[τ₂] | BaseType[τ]

    // Typed parameter
    TypedParam ::= Variable[x] ':' Type[τ]

    // Lambda abstraction
    Lambda(lambda) ::= 'λ' TypedParam '.' Term[e]
    

    // Base terms (cannot be applications)
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
        let var_symbol = typed_param_prod[0].rhs.iter().find(|s| s.value == "Variable").unwrap();
        assert_eq!(var_symbol.binding, Some("x".to_string()));

        // Check typing rules
        assert_eq!(grammar.typing_rules.len(), 3);
        assert!(grammar.typing_rules.contains_key("var"));
        assert!(grammar.typing_rules.contains_key("lambda"));
        assert!(grammar.typing_rules.contains_key("app"));
        
        let var_rule = grammar.typing_rules.get("var").unwrap();
        assert_eq!(var_rule.conclusion, typing::Conclusion::ContextLookup("Γ".to_string(), "x".to_string()));
        assert_eq!(var_rule.premises.len(), 1);
        match &var_rule.premises[0] {
            typing::Premise { setting: None, judgment: Some(typing::TypingJudgment::Membership(var, ctx)) } => {
                assert_eq!(var, "x");
                assert_eq!(ctx, "Γ");
            }
            _ => panic!("Expected membership judgment for var rule"),
        }
        
        let lambda_rule = grammar.typing_rules.get("lambda").unwrap();
        assert_eq!(lambda_rule.conclusion, typing::Conclusion::Type(typing::Type::parse("τ₁ → τ₂").unwrap()));
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
        assert_eq!(app_rule.conclusion, typing::Conclusion::Type(typing::Type::parse("τ₂").unwrap()));
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
        let grammar1 = Grammar::load(STLC_SPEC).expect("parse");
        let spec = grammar1.to_spec_string();
        // Removed debug print - use unified debug system if needed
        let grammar2 = Grammar::load(&spec).expect("re-parse");

        // Compare essential parts instead of direct equality (HashMap ordering can differ)
        assert_eq!(grammar1.productions.len(), grammar2.productions.len());
        assert_eq!(grammar1.typing_rules.len(), grammar2.typing_rules.len());
        
        // Check that all production keys exist in both
        for key in grammar1.productions.keys() {
            assert!(grammar2.productions.contains_key(key), "Missing production key: {}", key);
            assert_eq!(grammar1.productions[key], grammar2.productions[key], "Production mismatch for key: {}", key);
        }
        
        // Check that all typing rule keys exist in both
        for key in grammar1.typing_rules.keys() {
            assert!(grammar2.typing_rules.contains_key(key), "Missing typing rule key: {}", key);
            assert_eq!(grammar1.typing_rules[key], grammar2.typing_rules[key], "Typing rule mismatch for key: {}", key);
        }
        
        // Special tokens should be the same (order doesn't matter)
        let mut tokens1 = grammar1.special_tokens.clone();
        let mut tokens2 = grammar2.special_tokens.clone();
        tokens1.sort();
        tokens2.sort();
        assert_eq!(tokens1, tokens2);

        // Start symbol preserved through roundtrip
        assert_eq!(grammar1.start_nonterminal(), grammar2.start_nonterminal());
    }

    #[test]
    fn test_parse_repetition_suffix() {
        use crate::logic::grammar::utils::parse_repetition_suffix;
        
        // Test parsing repetition operators
        let (base, rep) = parse_repetition_suffix("Stmt*");
        assert_eq!(base, "Stmt");
        assert_eq!(rep, Some(RepetitionKind::ZeroOrMore));

        let (base, rep) = parse_repetition_suffix("Term+");
        assert_eq!(base, "Term");
        assert_eq!(rep, Some(RepetitionKind::OneOrMore));

        let (base, rep) = parse_repetition_suffix("Expr?");
        assert_eq!(base, "Expr");
        assert_eq!(rep, Some(RepetitionKind::ZeroOrOne));

        let (base, rep) = parse_repetition_suffix("Variable");
        assert_eq!(base, "Variable");
        assert_eq!(rep, None);
    }

    #[test]
    fn test_parse_rhs_with_repetition() {
        use crate::logic::grammar::utils::parse_rhs;
        
        // Test parsing RHS with repetition operators
        let result = parse_rhs("Stmt*").expect("Failed to parse RHS");
        assert_eq!(result.len(), 1);
        assert_eq!(result[0].len(), 1);
        assert_eq!(result[0][0].value, "Stmt");
        assert_eq!(result[0][0].repetition, Some(RepetitionKind::ZeroOrMore));

        let result = parse_rhs("'{' Stmt* '}'").expect("Failed to parse RHS");
        assert_eq!(result.len(), 1);
        assert_eq!(result[0].len(), 3);
        assert_eq!(result[0][0].value, "{");
        assert_eq!(result[0][1].value, "Stmt");
        assert_eq!(result[0][1].repetition, Some(RepetitionKind::ZeroOrMore));
        assert_eq!(result[0][2].value, "}");
    }

    #[test]
    fn test_parse_rhs_with_binding_and_repetition() {
        use crate::logic::grammar::utils::parse_rhs;
        
        // Test parsing with both binding and repetition
        let result = parse_rhs("'{' Stmt[s]* '}'").expect("Failed to parse RHS");
        assert_eq!(result.len(), 1);
        assert_eq!(result[0].len(), 3);
        assert_eq!(result[0][1].value, "Stmt");
        assert_eq!(result[0][1].binding, Some("s".to_string()));
        assert_eq!(result[0][1].repetition, Some(RepetitionKind::ZeroOrMore));
    }

    const REPETITION_GRAMMAR: &str = r#"
    // Grammar with repetition for testing
    Stmt ::= 'print' Expr ';'
    Expr ::= Number | Variable
    Number ::= /[0-9]+/
    Variable ::= /[a-zA-Z][a-zA-Z0-9]*/
    Block ::= '{' Stmt* '}'
    "#;

    #[test]
    fn test_grammar_with_repetition_loads() {
        let grammar = Grammar::load(REPETITION_GRAMMAR);
        assert!(grammar.is_ok(), "Grammar should load successfully: {:?}", grammar.err());
        
        let grammar = grammar.unwrap();
        
        // Check that Block production has correct repetition
        let block_productions = grammar.productions.get("Block").unwrap();
        assert_eq!(block_productions.len(), 1);
        assert_eq!(block_productions[0].rhs.len(), 3);
        assert_eq!(block_productions[0].rhs[1].value, "Stmt");
        assert_eq!(block_productions[0].rhs[1].repetition, Some(RepetitionKind::ZeroOrMore));
    }

    #[test] 
    fn test_parse_empty_block() {
        use crate::logic::parser::Parser;
        
        let grammar = Grammar::load(REPETITION_GRAMMAR).expect("Failed to load grammar");
        let mut parser = Parser::new(grammar);
        
        // Test parsing empty block "{}"
        let ast = parser.parse("{ }");
        assert!(ast.is_ok(), "Should parse empty block: {:?}", ast.err());
        
        let ast = ast.unwrap();
        if let crate::logic::ast::ASTNode::Nonterminal(nt) = ast {
            assert_eq!(nt.value, "Block");
            assert_eq!(nt.children.len(), 2); // Only '{' and '}', no statements
        } else {
            panic!("Expected nonterminal node");
        }
    }

    #[test]
    fn test_parse_block_with_statements() {
        use crate::logic::parser::Parser;
        
        let grammar = Grammar::load(REPETITION_GRAMMAR).expect("Failed to load grammar");
        let mut parser = Parser::new(grammar);
        
        // Test parsing block with statements "{ print x ; print y ; }"
        let ast = parser.parse("{ print x ; print y ; }");
        assert!(ast.is_ok(), "Should parse block with statements: {:?}", ast.err());
        
        let ast = ast.unwrap();
        if let crate::logic::ast::ASTNode::Nonterminal(nt) = ast {
            assert_eq!(nt.value, "Block");
            assert!(nt.children.len() > 2); // '{', statements, '}'
        } else {
            panic!("Expected nonterminal node");
        }
    }

    #[test]
    fn test_parse_all_repetition_operators() {
        use crate::logic::grammar::utils::parse_rhs;
        
        // Test all three repetition operators
        let result = parse_rhs("Term* Expr+ Stmt?").expect("Failed to parse RHS");
        assert_eq!(result.len(), 1);
        assert_eq!(result[0].len(), 3);
        
        assert_eq!(result[0][0].value, "Term");
        assert_eq!(result[0][0].repetition, Some(RepetitionKind::ZeroOrMore));
        
        assert_eq!(result[0][1].value, "Expr");
        assert_eq!(result[0][1].repetition, Some(RepetitionKind::OneOrMore));
        
        assert_eq!(result[0][2].value, "Stmt");
        assert_eq!(result[0][2].repetition, Some(RepetitionKind::ZeroOrOne));
    }
}
