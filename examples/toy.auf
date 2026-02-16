// Toy — meaningless but typed fun

Word ::= 'beep' | 'boop' | 'blorp'
TypeName ::= 'Fizz' | 'Buzz'
Laugh ::= 'ha' | 'ho' | 'hee'
Chorus ::= Laugh | Laugh Chorus

TypedValue(tv) ::= Word[w] ':' TypeName[τ]
Paren ::= '(' Expression ')'
Echo(echo) ::= Expression[e] 'x' Chorus[c]
Concat(cat) ::= Expression[l] '+' Expression[r]
Expression ::= TypedValue | Paren | Echo | Concat

// Typing Rules
----------- (tv)
τ

Γ ⊢ e : ?A
----------- (echo)
?A

Γ ⊢ l : ?A, Γ ⊢ r : ?A
-------------------- (cat)
?A
