// parser.mly

%token <int> 	INT
%token <string> ID
%token EOF
%token FOR
%token WHILE
%token BREAK
%token LET
%token IN
%token NIL
%token TO
%token END
%token FUNCTION
%token VAR
%token TYPE
%token ARRAY
%token IF
%token THEN
%token ELSE
%token DO
%token OF
%token LPAREN
%token RPAREN
%token LBRACK
%token RBRACK
%token LBRACE
%token RBRACE
%token DOT
%token COLON
%token COMMA
%token SEMI
%token PLUS
%token MINUS
%token TIMES
%token DIVIDE
%token EQ
%token NEQ
%token LT
%token LE
%token GT
%token GE
%token AND
%token OR
%token ASSIGN
%token <string> STRING

%%
