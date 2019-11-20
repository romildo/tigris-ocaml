{
  module L = Lexing

  type token = [%import: Parser.token] [@@deriving show]

  let illegal_character loc char =
    Error.error loc "illegal character '%c'" char

  let unterminated_comment loc =
    Error.error loc "unterminated comment"

  let unterminated_string loc =
    Error.error loc "unterminated string"

  let illegal_escape loc sequence =
    Error.error loc "illegal escape sequence '%s' in string literal" sequence

  let set_filename lexbuf fname =
    lexbuf.L.lex_curr_p <- { lexbuf.L.lex_curr_p with L.pos_fname = fname }

  (* position where string literal started *)
  let string_position = ref L.dummy_pos

  (* string buffer to accumulate characters when scanning string literals *)
  let string_buffer = Buffer.create 16

  (* helper function to update new line counting while scanning string literals *)
  let str_incr_linenum str lexbuf =
    String.iter (function '\n' -> L.new_line lexbuf | _ -> ()) str

  (* helper function to add a char to sthe string buffer *)
  let str_add_char char =
    Buffer.add_char string_buffer char
}

let spaces = [' ' '\t']+
let alpha = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let litint = digit+
let exponent = ['e' 'E'] ['+' '-']? digit+
let litreal = (digit+ '.' digit* | digit* '.' digit+) exponent? | digit+ exponent
let id = alpha (alpha | digit | '_')*

rule token = parse
  | spaces         { token lexbuf }
  | '\n'           { L.new_line lexbuf; token lexbuf }
  | "#" [^ '\n']*  { token lexbuf }
  | "{#"           { comment [lexbuf.L.lex_start_p] lexbuf }
  | litint as lxm  { INTEGER (int_of_string lxm) }
  | litreal as lxm { REAL (float_of_string lxm) }
  | "true"         { LOGIC true }
  | "false"        { LOGIC false }
  | '"'            { string_position := lexbuf.L.lex_start_p; string lexbuf }
  | "break"        { BREAK }
  | "do"           { DO }
  | "else"         { ELSE }
  | "end"          { END }
  | "if"           { IF }
  | "in"           { IN }
  | "let"          { LET }
  | "then"         { THEN }
  | "var"          { VAR }
  | "function"     { FUNCTION }
  | "type"         { TYPE }
  | "while"        { WHILE }
  | ":="           { ASSIGN }
  | '|'            { OR }
  | '&'            { AND }
  | '='            { EQ }
  | "<>"           { NE }
  | '<'            { LT }
  | "<="           { LE }
  | '>'            { GT }
  | ">="           { GE }
  | '+'            { PLUS }
  | '-'            { MINUS }
  | '*'            { TIMES }
  | '/'            { DIV }
  | '%'            { MOD }
  | '^'            { POW }
  | '('            { LPAREN }
  | ')'            { RPAREN }
  | ':'            { COLON }
  | ','            { COMMA }
  | ';'            { SEMI }
  | id as lxm      { ID (Symbol.symbol lxm) }
  | eof            { EOF }
  | _              { illegal_character (Location.curr_loc lexbuf) (L.lexeme_char lexbuf 0) }

and comment level = parse
  | "#}" { match level with
           | [_] -> token lexbuf
           | _::level' -> comment level' lexbuf
           | [] -> Error.fatal "bug in comment scanner"
         }
  | "{#" { comment (lexbuf.L.lex_start_p :: level) lexbuf }
  | '\n' { L.new_line lexbuf;
           comment level lexbuf
         }
  | _    { comment level lexbuf }
  | eof  { unterminated_comment (List.hd level, lexbuf.L.lex_start_p)
         }

and string = parse
| '"'                           { lexbuf.L.lex_start_p <- !string_position;
                                  let text = Buffer.contents string_buffer in
                                  Buffer.clear string_buffer;
                                  STRING text }
| "\\b"                         { str_add_char '\b'; string lexbuf }
| "\\n"                         { str_add_char '\n'; string lexbuf }
| "\\r"                         { str_add_char '\r'; string lexbuf }
| "\\t"                         { str_add_char '\t'; string lexbuf }
| "\\\""                        { str_add_char '"'; string lexbuf }
| "\\\\"                        { str_add_char '\\'; string lexbuf }
| "\\^" (['@' 'A'-'Z'] as x)    { str_add_char (Char.chr (Char.code x - Char.code '@')); string lexbuf }
| "\\^" (['a'-'z'] as x)        { str_add_char (Char.chr (Char.code x - Char.code 'a' + 1)); string lexbuf }
| "\\" (digit digit digit as x) { str_add_char (Char.chr (int_of_string x)); string lexbuf }
| "\\" _ as x                   { illegal_escape (lexbuf.L.lex_start_p, lexbuf.L.lex_curr_p) x;
                                  string lexbuf }
| [^ '\\' '"']+ as lxm          { str_incr_linenum lxm lexbuf;
                                  Buffer.add_string string_buffer lxm;
                                  string lexbuf }
| eof                           { unterminated_string (!string_position, lexbuf.L.lex_start_p);
                                  token lexbuf }
