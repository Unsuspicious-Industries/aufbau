# Partial parsing system

The partial parsing system is a generalized parser that tries all *valid* and *possibly valid* ways to parse a given input with a given grammar. 

Is takes a grammar input and code input, and produces what we call a **Partial AST** which is a *forest* of possible ASTs, that can contains either fully valid and parsed children or *completeable* states. It also has typing information, gathered by binding the grammar specified typing rules to the parsed nodes. 

### Fails and mismatches

The partial parser needs to ensure what we define as **correctness by completion** meaning that all of the nodes constituting a partial tree could become a valid complete AST node by adding a finite number of token. This is expressible through the Partial Production structure and the Partial symbol. A **Partial Node** is define as a node that corresponds to some non-zero amount of tokens in the input, but isnt complete by **lack of tokens**. It is defined by a produciton that is not fully matched, with a cursor indicating how many symbols have been matched. In some cases, we might have to stop mid-token. This is why we use the **Partial Symbol** which is a symbol that can match a prefix of a token.

Mismatches occur when the parser encounters a token that doesnt correpsond to the symbols in a production. Mismatches should trhow errors, and mismatched trees should not be considered valid or completeable, and npt appear in the **Partial AST**. Errors should propagate up, an * AST branch* that is partial on a msimatch is an invalid branch, and should be cut until the closest upper valid point (with valid partial parralels)