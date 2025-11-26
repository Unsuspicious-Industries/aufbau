#[cfg(test)]
mod tests {
    #[test]
    fn test_empty_input_partial_ast() {
        use crate::logic::grammar::Grammar;
        use crate::logic::partial::parse::Parser;
        use crate::logic::typing::evaluate_typing;

        let grammar_spec = r#"
        Identifier ::= /[a-zA-Z][a-zA-Z0-9]*/
        Variable(dec) ::= Identifier[x]
        Abstraction(abs) ::= 'λ' Identifier[x] ':' Type '.' Expression[e]

        AtomicExpression ::= Variable | '(' Expression ')'
        Application(app) ::= AtomicExpression[e₁] AtomicExpression[e₂]
        BaseType ::= Identifier[τ]
        AtomicType ::= BaseType | '(' Type ')'

        FunctionType ::= AtomicType[τ₁] '->' Type[τ₂]
        Type ::= AtomicType | FunctionType

        Expression ::= AtomicExpression | Abstraction | Application

        // Variable lookup rule
        x ∈ Γ
        ----------- (dec)
        Γ(x)

        // Lambda abstraction rule  
        Γ[x:τ₁] ⊢ e : τ₂
        ----------------------- (abs)
        τ₁ → τ₂

        // Function application rule
        Γ ⊢ e₁ : ?A, Γ ⊢ e₂ : ?A → ?B
        -------------------------------- (app)
        ?B
        "#;

        let grammar = Grammar::load(grammar_spec).unwrap();
        let mut parser = Parser::new(grammar.clone());
        let ast = parser.partial("λx:A.(x(λy:B.y)").unwrap();
        println!("Partial AST for empty input: {:#?}", ast);

        let valid = evaluate_typing(ast.roots(), &grammar);
        println!("Evaluate typing: {}", valid);
    }
}
