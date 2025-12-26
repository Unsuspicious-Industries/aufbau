//! C-Like Language Parseability Tests
//!
//! Fast tests for C-like language that verify all prefixes
//! are parseable without doing full completion search.

use super::*;

pub fn clike_grammar() -> Grammar {
    load_example_grammar("clike")
}

#[test]
fn valid_expressions() {
    let grammar = clike_grammar();
    let cases = vec![
        // === Variable declarations with initializer ===
        ParseTestCase::valid("int init 0", "int x = 0;"),
        ParseTestCase::valid("int init 5", "int x = 5;"),
        ParseTestCase::valid("int init 42", "int x = 42;"),
        ParseTestCase::valid("int init 100", "int y = 100;"),
        ParseTestCase::valid("float init", "float x = 3;"),
        ParseTestCase::valid("char init", "char c = 0;"),
        ParseTestCase::valid("bool init true", "bool b = true;"),
        ParseTestCase::valid("bool init false", "bool b = false;"),
        
        // === Variable declarations without initializer ===
        ParseTestCase::valid("int no init", "int x;"),
        ParseTestCase::valid("float no init", "float y;"),
        ParseTestCase::valid("char no init", "char c;"),
        ParseTestCase::valid("bool no init", "bool b;"),
        ParseTestCase::valid("void no init", "void v;"),
        
        // === Empty function definitions ===
        ParseTestCase::valid("void main empty", "void main() {}"),
        ParseTestCase::valid("int main empty", "int main() {}"),
        ParseTestCase::valid("float func empty", "float compute() {}"),
        ParseTestCase::valid("bool func empty", "bool check() {}"),
        ParseTestCase::valid("char func empty", "char getChar() {}"),
        
        // === Functions with return ===
        ParseTestCase::valid("return 0", "int main() {return 0;}"),
        ParseTestCase::valid("return 1", "int main() {return 1;}"),
        ParseTestCase::valid("return 42", "int main() {return 42;}"),
        ParseTestCase::valid("return true", "bool main() {return true;}"),
        ParseTestCase::valid("return false", "bool main() {return false;}"),
        
        // === Functions with parameters ===
        ParseTestCase::valid("one int param", "int add(int x) {}"),
        ParseTestCase::valid("two int params", "int add(int x, int y) {}"),
        ParseTestCase::valid("three int params", "int foo(int a, int b, int c) {}"),
        ParseTestCase::valid("mixed params", "float bar(int x, float y) {}"),
        ParseTestCase::valid("all types params", "int f(int a, float b, char c, bool d) {}"),
        
        // === Arithmetic expressions ===
        ParseTestCase::valid("add", "int x = 1 + 2;"),
        ParseTestCase::valid("sub", "int x = 5 - 3;"),
        ParseTestCase::valid("mul", "int x = 2 * 3;"),
        ParseTestCase::valid("div", "int x = 10 / 2;"),
        ParseTestCase::valid("mod", "int x = 10 % 3;"),
        ParseTestCase::valid("add larger", "int x = 100 + 200;"),
        ParseTestCase::valid("complex arith", "int x = 1 + 2 + 3;"),
        
        // === Comparison expressions ===
        ParseTestCase::valid("eq", "bool x = 1 == 1;"),
        ParseTestCase::valid("neq", "bool x = 1 != 2;"),
        ParseTestCase::valid("lt", "bool x = 1 < 2;"),
        ParseTestCase::valid("gt", "bool x = 2 > 1;"),
        ParseTestCase::valid("lte", "bool x = 1 <= 2;"),
        ParseTestCase::valid("gte", "bool x = 2 >= 1;"),
        
        // === Parenthesized expressions ===
        ParseTestCase::valid("paren literal", "int x = (5);"),
        ParseTestCase::valid("paren add", "int x = (1 + 2);"),
        ParseTestCase::valid("double paren", "int x = ((5));"),
        ParseTestCase::valid("paren nested", "int x = ((1 + 2));"),
        ParseTestCase::valid("paren complex", "int x = (1 + (2 + 3));"),
        
        // === If statements ===
        ParseTestCase::valid("if true", "int main() {if (true) {}}"),
        ParseTestCase::valid("if false", "int main() {if (false) {}}"),
        ParseTestCase::valid("if comparison lt", "int main() {if (1 < 2) {}}"),
        ParseTestCase::valid("if comparison gt", "int main() {if (2 > 1) {}}"),
        ParseTestCase::valid("if comparison eq", "int main() {if (1 == 1) {}}"),
        ParseTestCase::valid("if with else", "int main() {if (true) {} else {}}"),
        ParseTestCase::valid("if else with bodies", "int main() {if (true) {int x = 1;} else {int y = 2;}}"),
        
        // === While loops ===
        ParseTestCase::valid("while true", "int main() {while (true) {}}"),
        ParseTestCase::valid("while false", "int main() {while (false) {}}"),
        ParseTestCase::valid("while comparison", "int main() {while (1 < 10) {}}"),
        ParseTestCase::valid("while with body", "int main() {while (true) {int x = 1;}}"),
        
        // === For loops ===
        ParseTestCase::valid("for basic", "int main() {for (int i = 0; i < 10; i = i + 1) {}}"),
        ParseTestCase::valid("for j var", "int main() {for (int j = 0; j < 5; j = j + 1) {}}"),
        ParseTestCase::valid("for k var", "int main() {for (int k = 1; k < 100; k = k + 1) {}}"),
        ParseTestCase::valid("for with body", "int main() {for (int i = 0; i < 10; i = i + 1) {int x = i;}}"),
        ParseTestCase::valid("for decrement", "int main() {for (int i = 10; i > 0; i = i - 1) {}}"),
        
        // === Nested blocks ===
        ParseTestCase::valid("nested block 1", "int main() {{}}"),
        ParseTestCase::valid("nested block 2", "int main() {{{}}}"),
        ParseTestCase::valid("nested block 3", "int main() {{{{}}}}"),
        ParseTestCase::valid("block with stmt", "int main() {{int x = 1;}}"),
        
        // === Multiple statements ===
        ParseTestCase::valid("two decls", "int main() {int x = 1; int y = 2;}"),
        ParseTestCase::valid("three decls", "int main() {int x = 1; int y = 2; int z = 3;}"),
        ParseTestCase::valid("four decls", "int main() {int a = 1; int b = 2; int c = 3; int d = 4;}"),
        ParseTestCase::valid("decl and return", "int main() {int x = 5; return x;}"),
        ParseTestCase::valid("decls and return", "int main() {int x = 1; int y = 2; return x + y;}"),
        
        // === Assignments ===
        ParseTestCase::valid("assign simple", "int main() {int x; x = 5;}"),
        ParseTestCase::valid("assign expr", "int main() {int x; x = 1 + 2;}"),
        ParseTestCase::valid("assign twice", "int main() {int x; x = 1; x = 2;}"),
        ParseTestCase::valid("assign in for", "int main() {for (int i = 0; i < 10; i = i + 1) {}}"),
        
        // === Function calls ===
        ParseTestCase::valid("call no args", "int main() {foo();}"),
        ParseTestCase::valid("call one arg", "int main() {foo(1);}"),
        ParseTestCase::valid("call two args", "int main() {foo(1, 2);}"),
        ParseTestCase::valid("call three args", "int main() {foo(1, 2, 3);}"),
        ParseTestCase::valid("call four args", "int main() {foo(1, 2, 3, 4);}"),
        ParseTestCase::valid("call in decl", "int main() {int x = foo();}"),
        ParseTestCase::valid("call with args in decl", "int main() {int x = add(1, 2);}"),
        ParseTestCase::valid("call in expr", "int main() {int x = 1 + foo();}"),
        
        // === Programs with multiple functions ===
        ParseTestCase::valid("two funcs", "int foo() {} int main() {}"),
        ParseTestCase::valid("three funcs", "int a() {} int b() {} int main() {}"),
        ParseTestCase::valid("helper main", "int add(int x, int y) {return x + y;} int main() {return 0;}"),
        ParseTestCase::valid("void and int", "void helper() {} int main() {return 0;}"),
        
        // === Global variables ===
        ParseTestCase::valid("global before func", "int x = 5; int main() {}"),
        ParseTestCase::valid("globals before func", "int x = 5; int y = 10; int main() {}"),
        ParseTestCase::valid("global no init before func", "int x; int main() {}"),
        ParseTestCase::valid("mixed globals", "int x = 1; float y; int main() {}"),
        
        // === Nested control flow ===
        ParseTestCase::valid("nested if", "int main() {if (true) {if (true) {}}}"),
        ParseTestCase::valid("nested while", "int main() {while (true) {while (true) {}}}"),
        ParseTestCase::valid("if in while", "int main() {while (true) {if (true) {}}}"),
        ParseTestCase::valid("while in if", "int main() {if (true) {while (true) {}}}"),
        ParseTestCase::valid("for in if", "int main() {if (true) {for (int i = 0; i < 10; i = i + 1) {}}}"),
        ParseTestCase::valid("if in for", "int main() {for (int i = 0; i < 10; i = i + 1) {if (true) {}}}"),
        
        // === Complex programs ===
        ParseTestCase::valid("factorial structure", "int factorial(int n) {if (n < 2) {return 1;} return n;}"),
        ParseTestCase::valid("sum loop", "int main() {int sum = 0; for (int i = 0; i < 10; i = i + 1) {sum = sum + i;} return sum;}"),
        ParseTestCase::valid("conditional assign", "int main() {int x = 0; if (true) {x = 1;} else {x = 2;} return x;}"),
    ];

    println!("\n=== C-like Valid Expressions ({} cases) ===", cases.len());
    let res = run_parse_batch(&grammar, &cases);
    assert_eq!(res.failed, 0, "{}", res.format_failures());
    println!("All {} tests passed in {:?} (avg: {:?})", cases.len(), res.total_duration, res.avg_duration);
}

#[test]
fn invalid_expressions() {
    let grammar = clike_grammar();
    let cases = vec![

        // === Missing semicolons ===
        ParseTestCase::invalid("missing semi decl", "int x = 5 int y"),
        ParseTestCase::invalid("missing semi return", "int main() {return 0 }"),
        ParseTestCase::invalid("missing semi assign", "int main() {int x; x = 5 }"),
        ParseTestCase::invalid("missing semi between", "int x = 1 int y = 2;"),
            
        // === Missing/extra braces ===.
        // We keep only cases that are truly structurally impossible.
        ParseTestCase::invalid("missing open brace func", "int main() }"),
        ParseTestCase::invalid("extra close brace", "int main() {}}"),
        ParseTestCase::invalid("missing if brace", "int main() {if (true) }"),
        ParseTestCase::invalid("missing while brace", "int main() {while (true) }"),
        
        // === Missing parentheses ===
        ParseTestCase::invalid("missing if open paren", "int main() {if true) {}}"),
        ParseTestCase::invalid("missing if close paren", "int main() {if (true {}}"),
        ParseTestCase::invalid("missing while parens", "int main() {while true {}}"),
        ParseTestCase::invalid("missing func parens", "int main {}"),
        ParseTestCase::invalid("missing for parens", "int main() {for int i = 0; i < 10; i = i + 1 {}}"),
        ParseTestCase::invalid("missing call parens", "int main() {foo;}"), // Weird pass
        
        // === Wrong token order ===
        ParseTestCase::invalid("type after var", "x int = 5;"),
        ParseTestCase::invalid("op first", "+ 5;"),
        ParseTestCase::invalid("semi first", "; int x = 5;"),
        ParseTestCase::invalid("close paren first", ") int x;"),
        ParseTestCase::invalid("close brace first", "} int main() {}"),
        ParseTestCase::invalid("eq before var", "= 5 int x;"),
        
        // === Double operators/tokens ===
        ParseTestCase::invalid("double equals init", "int x == 5;"),
        ParseTestCase::invalid("double semi", "int x = 5;;"),
        ParseTestCase::invalid("double plus", "int x = 1 ++ 2;"),
        ParseTestCase::invalid("double type", "int int x = 5;"),
        ParseTestCase::invalid("double var", "int x x = 5;"),
        
        // === Invalid statements ===
        ParseTestCase::invalid("naked expr", "5;"),
        ParseTestCase::invalid("naked op", "+;"),
        ParseTestCase::invalid("empty stmt", ";"),
        ParseTestCase::invalid("type alone", "int;"),
        
        // === For loop errors ===
        ParseTestCase::invalid("for missing init", "int main() {for (; i < 10; i = i + 1) {}}"),
        ParseTestCase::invalid("for missing semi 1", "int main() {for (int i = 0 i < 10; i = i + 1) {}}"),
        ParseTestCase::invalid("for missing semi 2", "int main() {for (int i = 0; i < 10 i = i + 1) {}}"),
        ParseTestCase::invalid("for double semi", "int main() {for (int i = 0;; i < 10; i = i + 1) {}}"),
        
        // === Function errors ===
        ParseTestCase::invalid("func no return type", "main() {}"),
        ParseTestCase::invalid("func no name", "int () {}"),
        ParseTestCase::invalid("func double name", "int main main() {}"),
        ParseTestCase::invalid("param no type", "int foo(x) {}"),
        ParseTestCase::invalid("param no name", "int foo(int) {}"),
        
        // === Type errors: unbound variables ===
        ParseTestCase::type_error("unbound in init", "int x = y;"),
        ParseTestCase::type_error("unbound in return", "int main() {return x;}"), // should type_error
        ParseTestCase::type_error("unbound in condition", "int main() {if (x) {}}"), // should type error
        ParseTestCase::type_error("unbound in while", "int main() {while (x) {}}"),
        ParseTestCase::type_error("unbound in expr", "int main() {int x = y + 1;}"),
        ParseTestCase::type_error("unbound in assign", "int main() {int x; x = y;}"),
        ParseTestCase::type_error("unbound in call", "int main() {foo(x);}"),
        ParseTestCase::type_error("unbound rhs", "int main() {int x; x = z;}"),
        ParseTestCase::type_error("unbound both", "int main() {x = y;}"),
        
        // === Type errors: scope issues ===
        ParseTestCase::type_error("var before decl", "int main() {x = 5; int x;}"),
        ParseTestCase::type_error("use after block", "int main() {{int x = 1;} return x;}"),
        ParseTestCase::type_error("for var escapes", "int main() {for (int i = 0; i < 10; i = i + 1) {} return i;}"),
        
        // === Type errors: type mismatches in initialization ===
        ParseTestCase::type_error("int init with bool", "int x = true;"),
        ParseTestCase::type_error("int init with false", "int x = false;"),
        ParseTestCase::type_error("bool init with int", "bool x = 5;"),
        ParseTestCase::type_error("bool init with zero", "bool x = 0;"),
        ParseTestCase::type_error("float init with bool", "float x = true;"),
        ParseTestCase::type_error("char init with bool", "char x = false;"),
        
        // === Type errors: arithmetic type mismatches ===
        ParseTestCase::type_error("add int and bool", "int x = 1 + true;"),
        ParseTestCase::type_error("add bool and int", "bool x = true + 1;"),
        ParseTestCase::type_error("sub int and bool", "int x = 5 - false;"),
        ParseTestCase::type_error("mul mixed types", "int x = 1 * true;"),
        ParseTestCase::type_error("div mixed types", "int x = 10 / false;"),
        
        // === Type errors: comparison type mismatches ===
        ParseTestCase::type_error("compare int to bool", "bool x = 1 == true;"),
        ParseTestCase::type_error("compare bool to int", "bool x = false == 0;"),
        ParseTestCase::type_error("lt int and bool", "bool x = 1 < true;"),
        ParseTestCase::type_error("gt bool and int", "bool x = true > 0;"),
        
        // === Type errors: condition type mismatches ===
        ParseTestCase::type_error("if with int cond", "int main() {if (1) {}}"),
        ParseTestCase::type_error("if with zero cond", "int main() {if (0) {}}"),
        ParseTestCase::type_error("if with literal cond", "int main() {if (42) {}}"),
        ParseTestCase::type_error("while with int cond", "int main() {while (1) {}}"),
        ParseTestCase::type_error("while with zero cond", "int main() {while (0) {}}"),
        ParseTestCase::type_error("while with expr cond", "int main() {while (1 + 1) {}}"),
        
        // === Type errors: assignment type mismatches ===
        ParseTestCase::type_error("assign bool to int", "int main() {int x; x = true;}"),
        ParseTestCase::type_error("assign int to bool", "int main() {bool x; x = 5;}"),
        ParseTestCase::type_error("assign expr wrong type", "int main() {bool x; x = 1 + 2;}"),
        ParseTestCase::type_error("assign cmp to int", "int main() {int x; x = 1 < 2;}"),
        
        // === Type errors: for loop condition (expects int, not bool per spec) ===
        ParseTestCase::type_error("for bool cond", "int main() {for (int i = 0; true; i = i + 1) {}}"),
        ParseTestCase::type_error("for false cond", "int main() {for (int i = 0; false; i = i + 1) {}}"),
        ParseTestCase::type_error("for cmp cond", "int main() {for (int i = 0; i == 0; i = i + 1) {}}"),
        
        // === Type errors: subtle scoping ===
        ParseTestCase::type_error("shadow type mismatch", "int main() {int x = 1; {bool x = true;} x = false;}"),
        ParseTestCase::type_error("inner block var escape", "int main() {if (true) {int y = 1;} y = 2;}"),
        ParseTestCase::type_error("else block var escape", "int main() {if (true) {} else {int z = 1;} z = 2;}"),
        
        // === Invalid expressions ===
        ParseTestCase::invalid("incomplete add", "int x = 1 +;"),
        ParseTestCase::invalid("incomplete sub", "int x = 1 -;"),
        ParseTestCase::invalid("incomplete mul", "int x = 1 *;"),
        ParseTestCase::invalid("incomplete div", "int x = 1 /;"),
        ParseTestCase::invalid("incomplete cmp", "int x = 1 <;"),
        ParseTestCase::invalid("op op", "int x = 1 + +;"),
        ParseTestCase::invalid("empty parens", "int x = ();"),
        ParseTestCase::invalid("unmatched paren expr", "int x = (1;"),
        
        // === Keyword misuse ===
        ParseTestCase::invalid("return outside func", "return 0;"),
        ParseTestCase::invalid("if outside func", "if (true) {}"),
        ParseTestCase::invalid("while outside func", "while (true) {}"),
        ParseTestCase::invalid("for outside func", "for (int i = 0; i < 10; i = i + 1) {}"),
    ];

    println!("\n=== C-like Invalid Expressions ({} cases) ===", cases.len());
    let res = run_parse_batch(&grammar, &cases);
    assert_eq!(res.failed, 0, "{}", res.format_failures());
    println!("All {} tests passed in {:?} (avg: {:?})", cases.len(), res.total_duration, res.avg_duration);
}
