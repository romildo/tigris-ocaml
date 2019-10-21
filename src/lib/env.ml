(* env.ml *)

module S = Symbol
module Ty = Types

type entry =
  | VarEntry of Ty.ty
  | FunEntry of Ty.ty list * Ty.ty
  [@@deriving show]

let standard_types =
  [ ("bool",   Ty.BOOL  )
  ; ("int",    Ty.INT   )
  ; ("real",   Ty.REAL  )
  ; ("string", Ty.STRING)
  ]

let standard_functions =
  [ "printbool", [Ty.BOOL], Ty.UNIT
  ; "printint",  [Ty.INT ], Ty.UNIT
  ; "printreal", [Ty.REAL], Ty.UNIT
  ; "exit",      [Ty.INT ], Ty.UNIT
  (* TODO: complete with the other standard functions *)
  ]

let base_tenv =
  List.fold_left
    (fun env (name, t) ->
      S.enter (S.symbol name) t env)
    S.empty
    standard_types

let base_venv =
  List.fold_left
    (fun env (name, formals, result) ->
      S.enter
        (S.symbol name)
        (FunEntry (formals, result))
        env)
    S.empty
    standard_functions
