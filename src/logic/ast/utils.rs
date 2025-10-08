use crate::logic::ast::ASTNode;

/// Deep AST equality check for testing
pub fn ast_eq(a: &ASTNode, b: &ASTNode) -> bool {
    match (a, b) {
        (ASTNode::Terminal(t1), ASTNode::Terminal(t2)) => {
            t1.value == t2.value && t1.binding == t2.binding
        }
        (ASTNode::Nonterminal(nt1), ASTNode::Nonterminal(nt2)) => {
            if nt1.value != nt2.value
                || nt1.binding != nt2.binding
                || nt1.children.len() != nt2.children.len()
            {
                return false;
            }
            for (child_a, child_b) in nt1.children.iter().zip(nt2.children.iter()) {
                if !ast_eq(child_a, child_b) {
                    return false;
                }
            }
            true
        }
        // Different variants are not equal
        _ => false,
    }
}
