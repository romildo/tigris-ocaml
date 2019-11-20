(* Test syntax and semantics analysers *)

module L = Lexing

let check str =
  let lexbuf = L.from_string str in
  try
    let ast = Parser.program Lexer.token lexbuf in
    Format.printf "%s\n%!" (Tree.string_of_tree (Tree.map Absyntotree.node_txt (Absyntotree.tree_of_lexp ast)));
    (* Format.printf "%s\n%!" (Box.string_of_box (Tree.box_of_tree (Tree.map Absyntotree.node_txt (Absyntotree.tree_of_lexp ast)))); *)
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
    BoolExp false
    BOOL |}];

  check "345";
  [%expect{|
    IntExp 345
    INT |}];

  check {|"text"|};
  [%expect{|
    StringExp text
    STRING |}];

  (* looping *)
  check "while true do 45";
  [%expect{|
    WhileExp
    ├─ BoolExp true
    └─ IntExp 45
    UNIT |}];

  check "while 2019 do true";
  [%expect{|
    WhileExp
    ├─ IntExp 2019
    └─ BoolExp true
    :1.6-1.10 error: type mismatch: expected BOOL, found INT |}];

  check "height";
  [%expect{|
    VarExp
    └─ SimpleVar height
    :1.0-1.6 error: undefined variable height |}];

  check "let var height = 56 in height";
  [%expect{|
    LetExp
    ├─ Decls
    │  └─ VarDec
    │     ├─ height
    │     ├─
    │     └─ IntExp 56
    └─ VarExp
       └─ SimpleVar height
    INT |}];

  check "let var height : int = 56 in height";
  [%expect{|
    LetExp
    ├─ Decls
    │  └─ VarDec
    │     ├─ height
    │     ├─ int
    │     └─ IntExp 56
    └─ VarExp
       └─ SimpleVar height
    INT |}];

  check "let var height : bool = 56 in height";
  [%expect{|
    LetExp
    ├─ Decls
    │  └─ VarDec
    │     ├─ height
    │     ├─ bool
    │     └─ IntExp 56
    └─ VarExp
       └─ SimpleVar height
    :1.24-1.26 error: type mismatch: expected BOOL, found INT |}];

  check "let var height : t3 = 56 in height";
  [%expect{|
    LetExp
    ├─ Decls
    │  └─ VarDec
    │     ├─ height
    │     ├─ t3
    │     └─ IntExp 56
    └─ VarExp
       └─ SimpleVar height
    :1.17-1.19 error: undefined type t3 |}];

  check "printint(56)";
  [%expect{|
    CallExp
    ├─ printint
    └─ Args
       └─ IntExp 56
    UNIT |}];

  check "printint(true)";
  [%expect{|
    CallExp
    ├─ printint
    └─ Args
       └─ BoolExp true
    :1.9-1.13 error: type mismatch: expected INT, found BOOL |}];

  check "printint(56, 57, 58)";
  [%expect{|
    CallExp
    ├─ printint
    └─ Args
       ├─ IntExp 56
       ├─ IntExp 57
       └─ IntExp 58
    :1.0-1.20 error: too much arguments |}];

  check "printint(56, true + 1)";
  [%expect{|
    CallExp
    ├─ printint
    └─ Args
       ├─ IntExp 56
       └─ OpExp +
          ├─ BoolExp true
          └─ IntExp 1
    :1.0-1.22 error: too much arguments |}];

  check "print_integer(56)";
  [%expect{|
    CallExp
    ├─ print_integer
    └─ Args
       └─ IntExp 56
    :1.0-1.17 error: undefined function print_integer |}];

  check "let var g = 8 in g(0)";
  [%expect{|
    LetExp
    ├─ Decls
    │  └─ VarDec
    │     ├─ g
    │     ├─
    │     └─ IntExp 8
    └─ CallExp
       ├─ g
       └─ Args
          └─ IntExp 0
    :1.17-1.21 error: g is not a function |}];

  check "let function test() = printint(56) in test()";
  [%expect{|
    LetExp
    ├─ Decls
    │  └─ MutualFunDecs
    │     └─ Function: test
    │        ├─ Params
    │        ├─ FunctionDec
    │        └─ CallExp
    │           ├─ printint
    │           └─ Args
    │              └─ IntExp 56
    └─ CallExp
       ├─ test
       └─ Args
    UNIT |}];

  check "let function max(x:int, y:int):int = if x >= y then x else y in max(9,7)";
  [%expect{|
    LetExp
    ├─ Decls
    │  └─ MutualFunDecs
    │     └─ Function: max
    │        ├─ Params
    │        │  ├─ x
    │        │  │  └─ int
    │        │  └─ y
    │        │     └─ int
    │        ├─ int
    │        └─ IfExp
    │           ├─ OpExp >=
    │           │  ├─ VarExp
    │           │  │  └─ SimpleVar x
    │           │  └─ VarExp
    │           │     └─ SimpleVar y
    │           ├─ VarExp
    │           │  └─ SimpleVar x
    │           └─ VarExp
    │              └─ SimpleVar y
    └─ CallExp
       ├─ max
       └─ Args
          ├─ IntExp 9
          └─ IntExp 7
    INT |}];

  check "let function max(x:int, y:int):int = if x >= y then x else y in max(true, 8)";
  [%expect{|
    LetExp
    ├─ Decls
    │  └─ MutualFunDecs
    │     └─ Function: max
    │        ├─ Params
    │        │  ├─ x
    │        │  │  └─ int
    │        │  └─ y
    │        │     └─ int
    │        ├─ int
    │        └─ IfExp
    │           ├─ OpExp >=
    │           │  ├─ VarExp
    │           │  │  └─ SimpleVar x
    │           │  └─ VarExp
    │           │     └─ SimpleVar y
    │           ├─ VarExp
    │           │  └─ SimpleVar x
    │           └─ VarExp
    │              └─ SimpleVar y
    └─ CallExp
       ├─ max
       └─ Args
          ├─ BoolExp true
          └─ IntExp 8
    :1.68-1.72 error: type mismatch: expected INT, found BOOL |}];

  check "let function f(x:int, y:int):int = x > y in max(9,7)";
  [%expect{|
    LetExp
    ├─ Decls
    │  └─ MutualFunDecs
    │     └─ Function: f
    │        ├─ Params
    │        │  ├─ x
    │        │  │  └─ int
    │        │  └─ y
    │        │     └─ int
    │        ├─ int
    │        └─ OpExp >
    │           ├─ VarExp
    │           │  └─ SimpleVar x
    │           └─ VarExp
    │              └─ SimpleVar y
    └─ CallExp
       ├─ max
       └─ Args
          ├─ IntExp 9
          └─ IntExp 7
    :1.35-1.40 error: type mismatch: expected INT, found BOOL |}];

  check "let function f(x:int, y:int):bool = x > 0 | y in f(9,7)";
  [%expect{|
    LetExp
    ├─ Decls
    │  └─ MutualFunDecs
    │     └─ Function: f
    │        ├─ Params
    │        │  ├─ x
    │        │  │  └─ int
    │        │  └─ y
    │        │     └─ int
    │        ├─ bool
    │        └─ OpExp |
    │           ├─ OpExp >
    │           │  ├─ VarExp
    │           │  │  └─ SimpleVar x
    │           │  └─ IntExp 0
    │           └─ VarExp
    │              └─ SimpleVar y
    └─ CallExp
       ├─ f
       └─ Args
          ├─ IntExp 9
          └─ IntExp 7
    :1.44-1.45 error: type mismatch: expected BOOL, found INT |}];

  check "let function f(x:int, y:int, x:string):int = x+y in f(9,7)";
  [%expect{|
    LetExp
    ├─ Decls
    │  └─ MutualFunDecs
    │     └─ Function: f
    │        ├─ Params
    │        │  ├─ x
    │        │  │  └─ int
    │        │  ├─ y
    │        │  │  └─ int
    │        │  └─ x
    │        │     └─ string
    │        ├─ int
    │        └─ OpExp +
    │           ├─ VarExp
    │           │  └─ SimpleVar x
    │           └─ VarExp
    │              └─ SimpleVar y
    └─ CallExp
       ├─ f
       └─ Args
          ├─ IntExp 9
          └─ IntExp 7
    :1.29-1.37 error: duplicate parameter name x |}];

  check {|let
            function par(n:int):bool = if n = 0 then true else impar(n-1)
            function impar(n:int):bool = if n = 0 then false else par(n-1)
          in
            par(18)
         |};
  [%expect{|
    LetExp
    ├─ Decls
    │  └─ MutualFunDecs
    │     ├─ Function: par
    │     │  ├─ Params
    │     │  │  └─ n
    │     │  │     └─ int
    │     │  ├─ bool
    │     │  └─ IfExp
    │     │     ├─ OpExp =
    │     │     │  ├─ VarExp
    │     │     │  │  └─ SimpleVar n
    │     │     │  └─ IntExp 0
    │     │     ├─ BoolExp true
    │     │     └─ CallExp
    │     │        ├─ impar
    │     │        └─ Args
    │     │           └─ OpExp -
    │     │              ├─ VarExp
    │     │              │  └─ SimpleVar n
    │     │              └─ IntExp 1
    │     └─ Function: impar
    │        ├─ Params
    │        │  └─ n
    │        │     └─ int
    │        ├─ bool
    │        └─ IfExp
    │           ├─ OpExp =
    │           │  ├─ VarExp
    │           │  │  └─ SimpleVar n
    │           │  └─ IntExp 0
    │           ├─ BoolExp false
    │           └─ CallExp
    │              ├─ par
    │              └─ Args
    │                 └─ OpExp -
    │                    ├─ VarExp
    │                    │  └─ SimpleVar n
    │                    └─ IntExp 1
    └─ CallExp
       ├─ par
       └─ Args
          └─ IntExp 18
    BOOL |}];

  check {|let
            function par(n:int):bool = if n = 0 then true else impar(n-1)
            var alpha = 18
            function impar(n:int):bool = if n = 0 then false else par(n-1)
          in
            par(alpha)
         |};
  [%expect{|
    LetExp
    ├─ Decls
    │  ├─ MutualFunDecs
    │  │  └─ Function: par
    │  │     ├─ Params
    │  │     │  └─ n
    │  │     │     └─ int
    │  │     ├─ bool
    │  │     └─ IfExp
    │  │        ├─ OpExp =
    │  │        │  ├─ VarExp
    │  │        │  │  └─ SimpleVar n
    │  │        │  └─ IntExp 0
    │  │        ├─ BoolExp true
    │  │        └─ CallExp
    │  │           ├─ impar
    │  │           └─ Args
    │  │              └─ OpExp -
    │  │                 ├─ VarExp
    │  │                 │  └─ SimpleVar n
    │  │                 └─ IntExp 1
    │  ├─ VarDec
    │  │  ├─ alpha
    │  │  ├─
    │  │  └─ IntExp 18
    │  └─ MutualFunDecs
    │     └─ Function: impar
    │        ├─ Params
    │        │  └─ n
    │        │     └─ int
    │        ├─ bool
    │        └─ IfExp
    │           ├─ OpExp =
    │           │  ├─ VarExp
    │           │  │  └─ SimpleVar n
    │           │  └─ IntExp 0
    │           ├─ BoolExp false
    │           └─ CallExp
    │              ├─ par
    │              └─ Args
    │                 └─ OpExp -
    │                    ├─ VarExp
    │                    │  └─ SimpleVar n
    │                    └─ IntExp 1
    └─ CallExp
       ├─ par
       └─ Args
          └─ VarExp
             └─ SimpleVar alpha
    :2.63-2.73 error: undefined function impar |}];
