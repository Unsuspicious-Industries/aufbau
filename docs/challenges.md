# Implementation challenges and notes

This document outlines some of the challenges faced during the implementation of the P7 system, along with notes on how they were addressed. It serves as a reference for future development and maintenance.

## Partial parsing and completeability

In order to provide constraints for generation tasks, we develop an approach based on **completability**. A string $s$ is said to be completable in a given language if there exists a string $s'$ such that the concatenation $s s'$ is a valid string in that language. 

Our goal is to check for completeability, but also provide all possible $s'$ that would make $s s'$ valid. Several approaches can be considered, from naive a algorithmic implentation that leverages a **partial parser** integrating a **typing core** defining a set of valid type operation implementable in grammars, to higher order logical systems that could allow for a provable implementation of the constraint mecanism. 

### Partial parser 

In this codebase I've started implementing a partial parsing system, inspired from **early parsing** that collects all possible valid partial trees over a given string $s$, and then uses the typing core to filter out invalid branches based on defined type rules. 

Currently, we define a specific grammar format adapted to this approach, with a combination of ENBF-like production with *bindings* and typing rules. 

```
Identifier ::= /[a-z][a-zA-Z0-9]*/
Variable(dec) ::= Identifier[x]
Abstraction(abs) ::= 'λ' Identifier[x] ':' Type[τ] '.' Expression[e]

AtomicExpression ::= Variable | '(' Expression ')'
Application(app) ::= AtomicExpression[e1] AtomicExpression[e2]+
BaseType ::= 'Int' | 'Bool'
AtomicType ::= BaseType | '(' Type ')'

FunctionType ::= AtomicType[τ1] '→' Type[τ2]
Type ::= AtomicType | FunctionType

Expression ::= AtomicExpression | Abstraction | Application

// Variable lookup rule
x ∈ Γ
----------- (dec)
Γ(x)

// Lambda abstraction rule  
Γ[x:τ1] ⊢ e : τ2
----------------------- (abs)
τ1 → τ2

// Function application rule
Γ ⊢ e1 : τ1 → τ2,   Γ ⊢ e2 : τ1
-------------------------------- (app)
τ2
```



As a note, this can present other challenges to handle arbitrary vocabularies as presented in [this blog post](https://unsuspicious.org/blog/completing-regex).


## Multiple argument functions, tuple types and repetitions

## Inference time type constraints