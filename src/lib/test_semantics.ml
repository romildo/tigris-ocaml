(* Test syntax and semantics analysers *)

module L = Lexing

let check str =
  let lexbuf = L.from_string str in
  try
    let ast = Parser.program Lexer.token lexbuf in
    (* Format.printf "%s\n%!" (Tree.string_of_tree (Tree.map Absyntotree.node_txt (Absyntotree.tree_of_lexp ast))); *)
    Format.printf "%s\n%!" (Box.string_of_box (Tree.box_of_tree (Tree.map Absyntotree.node_txt (Absyntotree.tree_of_lexp ast))));
    let t = Semantic.type_check ast in
    Format.printf "%s\n%!" (Types.show_ty t)
  with
  | Parser.Error ->
     Format.printf "%a error: syntax\n%!" Location.pp_position lexbuf.L.lex_curr_p
  | Error.Error (loc, msg) ->
     Format.printf "%a error: %s%!" Location.pp_location loc msg

let%expect_test _ =
  (* literals *)
  check "false";
  [%expect{|
    ╭─────────────╮
    │BoolExp false│
    ╰─────────────╯
    BOOL |}];

  check "345";
  [%expect{|
    ╭──────────╮
    │IntExp 345│
    ╰──────────╯
    INT |}];

  check {|"text"|};
  [%expect{|
    :1.6 error: syntax |}];

  (* looping *)
  check "while true do 45";
  [%expect{|
            ╭────────╮
            │WhileExp│
            ╰────┬───╯
           ╭─────┴──────╮
    ╭──────┴─────╮ ╭────┴────╮
    │BoolExp true│ │IntExp 45│
    ╰────────────╯ ╰─────────╯
    UNIT |}];

  check "while 2019 do true";
  [%expect{|
             ╭────────╮
             │WhileExp│
             ╰────┬───╯
          ╭───────┴──────╮
    ╭─────┴─────╮ ╭──────┴─────╮
    │IntExp 2019│ │BoolExp true│
    ╰───────────╯ ╰────────────╯
    :1.6-1.10 error: type mismatch: expected BOOL, found INT |}];

  check "height";
  [%expect{|
         ╭──────╮
         │VarExp│
         ╰───┬──╯
    ╭────────┴───────╮
    │SimpleVar height│
    ╰────────────────╯
    :1.0-1.6 error: undefined variable height |}];

  check "let var height := 56 in height";
  [%expect{|
                     ╭──────╮
                     │LetExp│
                     ╰───┬──╯
               ╭─────────┴───────────╮
            ╭──┴──╮              ╭───┴──╮
            │Decls│              │VarExp│
            ╰──┬──╯              ╰───┬──╯
           ╭───┴──╮         ╭────────┴───────╮
           │VarDec│         │SimpleVar height│
           ╰───┬──╯         ╰────────────────╯
        ╭─────┬┴─────╮
    ╭───┴──╮ ╭┴ ╭────┴────╮
    │height│ ││ │IntExp 56│
    ╰──────╯ ╰╯ ╰─────────╯
    INT |}];

  check "let var height : int := 56 in height";
  [%expect{|
                      ╭──────╮
                      │LetExp│
                      ╰───┬──╯
                 ╭────────┴─────────────╮
             ╭───┴─╮                ╭───┴──╮
             │Decls│                │VarExp│
             ╰───┬─╯                ╰───┬──╯
             ╭───┴──╮          ╭────────┴───────╮
             │VarDec│          │SimpleVar height│
             ╰───┬──╯          ╰────────────────╯
        ╭──────┬─┴──────╮
    ╭───┴──╮ ╭─┴─╮ ╭────┴────╮
    │height│ │int│ │IntExp 56│
    ╰──────╯ ╰───╯ ╰─────────╯
    INT |}];

  check "let var height : bool := 56 in height";
  [%expect{|
                       ╭──────╮
                       │LetExp│
                       ╰───┬──╯
                 ╭─────────┴─────────────╮
              ╭──┴──╮                ╭───┴──╮
              │Decls│                │VarExp│
              ╰──┬──╯                ╰───┬──╯
             ╭───┴──╮           ╭────────┴───────╮
             │VarDec│           │SimpleVar height│
             ╰───┬──╯           ╰────────────────╯
        ╭───────┬┴───────╮
    ╭───┴──╮ ╭──┴─╮ ╭────┴────╮
    │height│ │bool│ │IntExp 56│
    ╰──────╯ ╰────╯ ╰─────────╯
    :1.25-1.27 error: type mismatch: expected BOOL, found INT |}];

  check "let var height : t3 := 56 in height";
  [%expect{|
                      ╭──────╮
                      │LetExp│
                      ╰───┬──╯
                ╭─────────┴────────────╮
             ╭──┴──╮               ╭───┴──╮
             │Decls│               │VarExp│
             ╰──┬──╯               ╰───┬──╯
            ╭───┴──╮          ╭────────┴───────╮
            │VarDec│          │SimpleVar height│
            ╰───┬──╯          ╰────────────────╯
        ╭──────┬┴──────╮
    ╭───┴──╮ ╭─┴╮ ╭────┴────╮
    │height│ │t3│ │IntExp 56│
    ╰──────╯ ╰──╯ ╰─────────╯
    :1.17-1.19 error: undefined type t3 |}];
