{
  module L = Lexing

  type token = [%import: Parser.token] [@@deriving show]

  let illegal_character loc char =
    Error.error loc "illegal character '%c'" char

  let err_string loc =
    Error.error loc "Error string"

  let err_comment loc=
    Error.error loc "Error comment"

  let set_filename lexbuf fname =
    lexbuf.L.lex_curr_p <-  
      { lexbuf.L.lex_curr_p with L.pos_fname = fname }
}

let spaces = [' ' '\t'] +
let litint = [ '0' - '9'] +

let alpha = ['a'-'z' 'A'-'Z']+
let id = alpha + (alpha|litint|'_')*


rule token = parse
   | spaces        { token lexbuf }
   | '\n'          { L.new_line lexbuf; token lexbuf }
   | "/*"          { comment 0 lexbuf}
   | litint as lxm { INT (int_of_string lxm) }
   | id as lxm { ID lxm }
   | '"' { lex_string (Buffer.create 64) lexbuf }   
   | "for"         { FOR }
   | "while"       { WHILE }
   | "break"       { BREAK }
   | "let"         { LET }
   | "in"          { IN }
   | "nil"         { NIL }
   | "to"          { TO }
   | "end"         { END }
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
   | "if"          { IF }
   | "then"        { THEN }
   | "else"        { ELSE }
   | "function"    { FUNCTION }
   | "var"         { VAR }
   | "type"        { TYPE }
   | "array"       { ARRAY }
   | "do"          { DO }
   | "of"          { OF }
   | ":="          { ASSIGN }
   | eof           { EOF }
   | _             { illegal_character (Location.curr_loc lexbuf) (L.lexeme_char lexbuf 0) }
   
   and lex_string buf =
      parse
      | '"'       { STRING (Buffer.contents buf) }
      | '\\' '/'  { Buffer.add_char buf '/'; lex_string buf lexbuf }
      | '\\' '\\' { Buffer.add_char buf '\\'; lex_string buf lexbuf }
      | '\\' 'b'  { Buffer.add_char buf '\b'; lex_string buf lexbuf }
      | '\\' 'f'  { Buffer.add_char buf '\012'; lex_string buf lexbuf }
      | '\\' 'n'  { Buffer.add_char buf '\n'; lex_string buf lexbuf }
      | '\\' 'r'  { Buffer.add_char buf '\r'; lex_string buf lexbuf }
      | '\\' 't'  { Buffer.add_char buf '\t'; lex_string buf lexbuf }
      | [^ '"' '\\']+ { Buffer.add_string buf (Lexing.lexeme lexbuf); lex_string buf lexbuf }
      | _ { illegal_character (Location.curr_loc lexbuf) (L.lexeme_char lexbuf 0) }
      | eof { err_comment (Location.curr_loc lexbuf) }

   and comment size =
      parse
      | "/*" { comment (size + 1) lexbuf }
      | "*/" { if size = 0 then token lexbuf else comment (size - 1) lexbuf }
      | eof  { err_comment (Location.curr_loc lexbuf) }
      | _ { comment size lexbuf }
