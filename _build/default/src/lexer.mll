{
  module L = Lexing

  type token = [%import: Parser.token] [@@deriving show]

  let illegal_character loc char =
    Error.error loc "illegal character '%c'" char

  let error_string loc =
    Error.error loc "error string"

  let error_comment loc=
    Error.error loc "error comment"

  let set_filename lexbuf fname =
    lexbuf.L.lex_curr_p <-  
      { lexbuf.L.lex_curr_p with L.pos_fname = fname }
}

let spaces = [' ' '\t'] +
let litint = [ '0' - '9'] +

let alphanumeric = ['a'-'z' 'A'-'Z']+
let id = alphanumeric + (alphanumeric|litint|'_')*


rule token = parse
   | spaces        { token lexbuf }
   | '\n'          { L.new_line lexbuf; token lexbuf }
   | "/*"          { comment_lexer 0 lexbuf}
   | litint as lxm { INT (int_of_string lxm) }
   | id as lxm { ID lxm }
   | '"' { string_lexer (Buffer.create 64) lexbuf }   
   | "for"         { FOR }
   | "while"       { WHILE }
   | "break"       { BREAK }
   | "let"         { LET }
   | "in"          { IN }
   | "nil"         { NIL }
   | "to"          { TO }
   | "end"         { END }
   | "function"    { FUNCTION }
   | "var"         { VAR }
   | "type"        { TYPE }
   | "array"       { ARRAY }
   | "if"          { IF }
   | "then"        { THEN }
   | "else"        { ELSE }
   | "do"          { DO }
   | "of"          { OF }
   | '('           { LPAREN }
   | ')'           { RPAREN }
   | '['           { LBRACK }
   | ']'           { RBRACK }
   | '{'           { LBRACE }
   | '}'           { RBRACE }
   | '.'           { DOT }
   | ':'           { COLON }
   | ','           { COMMA }
   | ';'           { SEMI }
   | '+'           { PLUS }
   | '-'           { MINUS }
   | '*'           { TIMES }
   | '/'           { DIVIDE }
   | '='           { EQ }
   | "<>"          { NEQ }
   | '<'           { LT }
   | "<="          { LE }
   | '>'           { GT }
   | ">="          { GE }
   | '&'           { AND }
   | '|'           { OR }
   | ":="          { ASSIGN }
   | eof           { EOF }
   | _             { illegal_character (Location.curr_loc lexbuf) (L.lexeme_char lexbuf 0) }
   
   and string_lexer buf =
      parse
      | '"'       { STRING (Buffer.contents buf) }
      | '\\' '/'  { Buffer.add_char buf '/'; string_lexer buf lexbuf }
      | '\\' '\\' { Buffer.add_char buf '\\'; string_lexer buf lexbuf }
      | '\\' 'b'  { Buffer.add_char buf '\b'; string_lexer buf lexbuf }
      | '\\' 'f'  { Buffer.add_char buf '\012'; string_lexer buf lexbuf }
      | '\\' 'n'  { Buffer.add_char buf '\n'; string_lexer buf lexbuf }
      | '\\' 'r'  { Buffer.add_char buf '\r'; string_lexer buf lexbuf }
      | '\\' 't'  { Buffer.add_char buf '\t'; string_lexer buf lexbuf }
      | [^ '"' '\\']+ { Buffer.add_string buf (Lexing.lexeme lexbuf); string_lexer buf lexbuf }
      | _ { illegal_character (Location.curr_loc lexbuf) (L.lexeme_char lexbuf 0) }
      | eof { error_comment (Location.curr_loc lexbuf) }

   and comment_lexer size =
      parse
      | "/*" { comment_lexer (size+1) lexbuf }
      | "*/" { if size = 0 then token lexbuf else comment_lexer (size-1) lexbuf }
      | eof  { error_comment (Location.curr_loc lexbuf) }
      | _ { comment_lexer size lexbuf }