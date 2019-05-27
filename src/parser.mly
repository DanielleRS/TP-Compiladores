// parser.mly

%token <int> 	INT
%token <string> ID
%token <string> STRING
%token FOR WHILE BREAK LET IN NIL TO END
%token LPAREN RPAREN LBRACK RBRACK LBRACE RBRACE
%token DOT COLON COMMA SEMI
%token PLUS MINUS TIMES DIVIDE
%token EQ NEQ LT LE GT GE
%token AND OR
%token IF THEN ELSE
%token FUNCTION
%token VAR
%token TYPE
%token ARRAY
%token DO
%token OF
%token ASSIGN
%token EOF

%%
