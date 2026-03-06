#[W] Conclusion

The conclusion determines the rule's result type and any modifications to the context $\Gamma$.

Source: [`src/logic/typing/eval.rs`](../../src/logic/typing/eval.rs), [`src/logic/typing/rule.rs`](../../src/logic/typing/rule.rs)

## Conclusion Kinds

>D Conclusion Kinds
A conclusion has one of the following forms:

| Form | Syntax | Result |
| :--- | :--- | :--- |
| **Type** | $\tau$ | Return type $\tau$ |
| **Lookup** | $\Gamma(x)$ | Type of $x$ in $\Gamma$ |
| **Transform** | $\Gamma \to \Gamma[x:\tau] \vdash \tau'$ | Extend $\Gamma$ with $x:\tau$, return type $\tau'$ |
<

Checking mode (void) is initiated by the [Check premise](./premises.md) $\Gamma \triangleright e$, not by a conclusion. All conclusions must return a type.

### Type Conclusion

>D Type Conclusion
A conclusion $\tau$ evaluates directly to type $\tau$. Any meta-variables in $\tau$ must be bound by premises; unbound meta-variables yield an indeterminate type.
<

### Context Lookup

>D Context Lookup
A conclusion $\Gamma(x)$ resolves the binding $x$ in context $\Gamma$. 

*   If $x$ is not fully parsed: $\text{Partial}$.
*   If $x$ is not in $\Gamma$: $\text{Invalid}$.
*   Otherwise: The type associated with $x$.
<

### Context Transform

>D Context Transform
A conclusion $\Gamma \to \Gamma[x:\tau] \vdash \tau'$ extends context $\Gamma$ for sibling nodes.

1.  Evaluate $x$ and $\tau$ to concrete values.
2.  $\Gamma' = \Gamma \cup \\{x:\tau\\}$.
3.  The result type is $\tau'$.
4.  The output context is $\Gamma'$.
<
