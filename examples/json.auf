// minimal json-like

// Whitespace (optional)
WS ::= /[ \t\n\r]*/

// Tokens
LBrace ::= '{'
RBrace ::= '}'
LBracket ::= '['
RBracket ::= ']'
Colon ::= ':'
Comma ::= ','

// Literals (simplified but robust)
Number ::= /-?(0|[1-9][0-9]*)(\.[0-9]+)?([eE][+-]?[0-9]+)?/
StrChar ::= /[a-zA-Z0-9 _-]*/
String ::= '"' StrChar '"'
Boolean ::= 'true' | 'false'
Null ::= 'null'

// Values
Value ::= WS JsonValue WS
JsonValue ::= String | Number | Boolean | Null | Object | Array

// Arrays
Array ::= LBracket WS Elements WS RBracket
Elements ::= JsonValue | JsonValue WS Comma WS Elements

// Objects
Object ::= LBrace WS Members WS RBrace
Members ::= Pair | Pair WS Comma WS Members
Pair ::= String WS Colon WS JsonValue
