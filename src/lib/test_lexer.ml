(* expect tests for the scanner *)

module L = Lexing

let scan_string s =
  let lexbuf = L.from_string s in
  let rec go () =
    let tok = Lexer.token lexbuf in
    Format.printf
      "%a %s\n%!"
      Location.pp_location (Location.curr_loc lexbuf)
      (Lexer.show_token tok);
    match tok with
    | Parser.EOF -> ()
    | _ -> go ()
  in
  try go ()
  with
  | Error.Error (loc, msg) ->
     Format.printf "%a error: %s\n" Location.pp_location loc msg

let%expect_test _ =
  (* white spaces and comments *)
  scan_string "";
  [%expect{| :1.0-1.0 Parser.EOF |}];

  scan_string "   \t\t \n     \n  ";
  [%expect{| :3.2-3.2 Parser.EOF |}];

  scan_string "# a line comment";
  [%expect{| :1.16-1.16 Parser.EOF |}];

  scan_string "{# a block comment #}";
  [%expect{| :1.21-1.21 Parser.EOF |}];

  scan_string "{# a {# nested {# block #} comment #} here #}";
  [%expect{| :1.45-1.45 Parser.EOF |}];

  scan_string "{# an unterminated block comment";
  [%expect{| :1.0-1.32 error: unterminated comment |}];

  scan_string "{# an unterminated {# nested block comment #} here";
  [%expect{| :1.0-1.50 error: unterminated comment |}];

  (* logic literals *)
  scan_string "true false";
  [%expect{|
    :1.0-1.4 (Parser.LOGIC true)
    :1.5-1.10 (Parser.LOGIC false)
    :1.10-1.10 Parser.EOF |}];

  (* integer literals *)
  scan_string "0 139015 007 -4324 +2019";
  [%expect{|
           :1.0-1.1 (Parser.INTEGER 0)
           :1.2-1.8 (Parser.INTEGER 139015)
           :1.9-1.12 (Parser.INTEGER 7)
           :1.13-1.14 Parser.MINUS
           :1.14-1.18 (Parser.INTEGER 4324)
           :1.19-1.20 Parser.PLUS
           :1.20-1.24 (Parser.INTEGER 2019)
           :1.24-1.24 Parser.EOF |}];

  (* real literals *)
  scan_string "123.4567 105. .34 1.e234 .23E+5 765e-180 -1.2 +2.1";
  [%expect{|
    :1.0-1.8 (Parser.REAL 123.4567)
    :1.9-1.13 (Parser.REAL 105.)
    :1.14-1.17 (Parser.REAL 0.34)
    :1.18-1.24 (Parser.REAL 1e+234)
    :1.25-1.31 (Parser.REAL 23000.)
    :1.32-1.40 (Parser.REAL 7.65e-178)
    :1.41-1.42 Parser.MINUS
    :1.42-1.45 (Parser.REAL 1.2)
    :1.46-1.47 Parser.PLUS
    :1.47-1.50 (Parser.REAL 2.1)
    :1.50-1.50 Parser.EOF |}];

  (* string literals *)
  scan_string {tigris|"a string literal" "another one"|tigris};
  [%expect{|
    :1.0-1.18 (Parser.STRING "a string literal")
    :1.19-1.32 (Parser.STRING "another one")
    :1.32-1.32 Parser.EOF |}];

  scan_string {|"first line
                 second line
                 third line"|};
  [%expect{|
    :1.0-3.1 (Parser.STRING
       "first line\n                 second line\n                 third line")
    :3.1-3.1 Parser.EOF |}];

  scan_string {|"with\tescape sequences\n\"HERE\\\b!\rTHERE\^c\^C\^@\0678"|};
  [%expect{|
    :1.0-1.58 (Parser.STRING "with\tescape sequences\n\"HERE\\\b!\rTHERE\003\003\000C8")
    :1.58-1.58 Parser.EOF |}];

  scan_string {|"illegal escape \K."|};
  [%expect{| :1.16-1.18 error: illegal escape sequence '\K' in string literal |}];

  scan_string {|"illegal control character \^9."|};
  [%expect{| :1.27-1.29 error: illegal escape sequence '\^' in string literal |}];

  scan_string {|"unterminated string literal|};
  [%expect{| :1.0-1.28 error: unterminated string |}];

  (* keywords *)
  scan_string "var if then else while do break let in end";
  [%expect{|
    :1.0-1.3 Parser.VAR
    :1.4-1.6 Parser.IF
    :1.7-1.11 Parser.THEN
    :1.12-1.16 Parser.ELSE
    :1.17-1.22 Parser.WHILE
    :1.23-1.25 Parser.DO
    :1.26-1.31 Parser.BREAK
    :1.32-1.35 Parser.LET
    :1.36-1.38 Parser.IN
    :1.39-1.42 Parser.END
    :1.42-1.42 Parser.EOF |}];

  (* identifiers *)
  scan_string "height alpha_301_coord";
  [%expect{|
    :1.0-1.6 (Parser.ID "height")
    :1.7-1.22 (Parser.ID "alpha_301_coord")
    :1.22-1.22 Parser.EOF |}];

  scan_string "first-name";
  [%expect{|
    :1.0-1.5 (Parser.ID "first")
    :1.5-1.6 Parser.MINUS
    :1.6-1.10 (Parser.ID "name")
    :1.10-1.10 Parser.EOF |}];

  scan_string "__weight";
  [%expect{| :1.0-1.1 error: illegal character '_' |}];

  scan_string "34rua";
  [%expect{|
    :1.0-1.2 (Parser.INTEGER 34)
    :1.2-1.5 (Parser.ID "rua")
    :1.5-1.5 Parser.EOF |}];

  (* operators *)
  scan_string "+ - * / % ^ = <> > >= < <= & | :=";
  [%expect{|
    :1.0-1.1 Parser.PLUS
    :1.2-1.3 Parser.MINUS
    :1.4-1.5 Parser.TIMES
    :1.6-1.7 Parser.DIV
    :1.8-1.9 Parser.MOD
    :1.10-1.11 Parser.POW
    :1.12-1.13 Parser.EQ
    :1.14-1.16 Parser.NE
    :1.17-1.18 Parser.GT
    :1.19-1.21 Parser.GE
    :1.22-1.23 Parser.LT
    :1.24-1.26 Parser.LE
    :1.27-1.28 Parser.AND
    :1.29-1.30 Parser.OR
    :1.31-1.33 Parser.ASSIGN
    :1.33-1.33 Parser.EOF |}];

  (* punctuation *)
  scan_string "( ) , ; :";
  [%expect{|
    :1.0-1.1 Parser.LPAREN
    :1.2-1.3 Parser.RPAREN
    :1.4-1.5 Parser.COMMA
    :1.6-1.7 Parser.SEMI
    :1.8-1.9 Parser.COLON
    :1.9-1.9 Parser.EOF |}];

  (* illegal character *)
  scan_string "$";
  [%expect{| :1.0-1.1 error: illegal character '$' |}];
