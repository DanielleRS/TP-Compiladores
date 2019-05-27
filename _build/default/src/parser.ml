
type token = 
  | WHILE
  | VAR
  | TYPE
  | TO
  | TIMES
  | THEN
  | STRING of (string)
  | SEMI
  | RPAREN
  | RBRACK
  | RBRACE
  | PLUS
  | OR
  | OF
  | NIL
  | NEQ
  | MINUS
  | LT
  | LPAREN
  | LET
  | LE
  | LBRACK
  | LBRACE
  | INT of (int)
  | IN
  | IF
  | ID of (string)
  | GT
  | GE
  | FUNCTION
  | FOR
  | EQ
  | EOF
  | END
  | ELSE
  | DOT
  | DO
  | DIVIDE
  | COMMA
  | COLON
  | BREAK
  | ASSIGN
  | ARRAY
  | AND
