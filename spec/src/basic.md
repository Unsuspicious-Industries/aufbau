# Basic Concepts and Notations

First, we are going to establish some simple concepts of programming language theory, so that we can refer to them later in the specification.

>D Alphabet 
An **alphabet**, often noted sigma $\Sigma$ is a finite set of symbols. 
<

We are going to use shorthand notation for ranges of symbols in alphabets. For example `a-z` denotes all lowercase letters from `a` to `z`, and `0-9` denotes all digits from `0` to `9`.

>E Alphabet Example
For example, the alphabet for arithmetic expressions could be defined as:
$$\Sigma_\text{Arithmetic} = \\{0\text{-}9, +, -, *, /\\}$$
<

An alphabet can be subjected to various operations to create different kinds of sets. We usually call those sets **languages**. The **Kleene star** operation, denoted $X^*$, creates the set of all possible strings that can be formed by concatenating zero or more elements from $X$. It's similar to the power set operation, but instead of creating subsets, it creates strings.

>D Kleene Closure
The **Kleene closure** of an alphabet $\Sigma$, denoted $\Sigma^\ast$, is the set of all possible strings that can be formed by concatenating elements of $\Sigma$.
<

>E Kleene Closure Example
For example, the Kleene closure of the alphabet $\Sigma = {a,b}^\ast$ is :
$$\Sigma^\ast = \\{\epsilon, a, b, aa, ab, ba, bb, aaa, aab, aba, abb, baa, bab, bba, bbb, \ldots\\}$$
<

A language is defined as a set of **strings** over an alphabet. A string is a finite sequence of symbols from the alphabet. A string in a language is called a **word**.

>L Language Closure Lemma
For all languages $L$ over an alphabet $\Sigma$, 
$$L \subset \Sigma^\ast$$
<

The **concatenation** operation is written as $s \cdot t$ or the shorthand $st$ for strings $s, t \in \Sigma^\ast$. It is defined as the string obtained by appending $t$ to $s$.

>R Notation
The **empty input** is denoted by $\epsilon$. 
<


## Definitions and notations

Let $L$ be a **formal language** over an alphabet $\Sigma$.

An **expression** $s \in \Sigma^\ast$ is said to be **completable** in $L$ if there exists some $s' \in \Sigma^\ast$ such that $ss' \in L$.


>D Completability Set
For any expression $s \in \Sigma^\ast$, the **completability set** is:
$$\mathcal{C}_L(s) = \{a \in \Sigma : \exists s' \in \Sigma^\ast : sas' \in L\}$$
<

The completability set is central to our formalism. We can use it to state an important equivalence:

>T Completability Equivalence
$ \forall s \in \Sigma^\ast$, 

$s \text{ completable in } L \iff \mathcal{C}_L(s) \neq \emptyset$
<

Completability of $s$ in $L$ thus reduces to checking whether its completability set is non-empty. 

>R Nullable Completability
As an interesting edge case, if $s \in L$, then $s$ is **complete** in $L$ but also **completable** in $L$ and its **completability set** is the *nullable* set:
$$
s\in L \iff \mathcal{C}_L(s) = \\{\epsilon\\}
$$
<

## Structure of this Specification

We will start by defining the theoretical foundations to our approach, and then mix it up with algorithmic and implementation details and actual references to the codebase.
