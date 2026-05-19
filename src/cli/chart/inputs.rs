//! Canonical input generators shared by the chart CLI subcommand and
//! the Criterion benchmarks.

use aufbau::domains::typing::{Context, Type};

/// `f x0 x1 … x{n-1}` with a well-typed context for n-argument application.
pub fn stlc_chain(n: usize) -> (String, Context) {
    let type_names: Vec<_> = (0..=n).map(|i| format!("T{i}")).collect();
    let mut f_ty = type_names[n].clone();
    for i in (0..n).rev() {
        f_ty = format!("{}->{}", type_names[i], f_ty);
    }
    let mut ctx = Context::new();
    ctx.add("f".to_string(), Type::parse_raw(&f_ty).unwrap());
    for i in 0..n {
        ctx.add(format!("x{i}"), Type::parse_raw(&type_names[i]).unwrap());
    }
    let input = std::iter::once("f".to_string())
        .chain((0..n).map(|i| format!("x{i}")))
        .collect::<Vec<_>>()
        .join(" ");
    (input, ctx)
}

/// `{ let v0: Int = 1; … let v{n-1}: Int = n; }` with n declarations.
pub fn imp_block(n: usize) -> String {
    let mut parts = vec!["{".to_string()];
    for i in 0..n {
        parts.extend([
            "let".to_string(),
            format!("v{i}"),
            ":".to_string(),
            "Int".to_string(),
            "=".to_string(),
            (i + 1).to_string(),
            ";".to_string(),
        ]);
    }
    parts.push("}".to_string());
    parts.join(" ")
}

/// `1 + 2 + … + n` — flat left-associative sum.
pub fn arith_flat(n: usize) -> String {
    (1..=n.max(1))
        .map(|i| i.to_string())
        .collect::<Vec<_>>()
        .join(" + ")
}

/// `((…1…))` — expression wrapped in n layers of parentheses.
pub fn arith_nested(n: usize) -> String {
    format!("{}{}{}", "(".repeat(n), 1, ")".repeat(n))
}
