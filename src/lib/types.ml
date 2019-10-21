(* types.ml *)

type ty =
  | UNIT
  | BOOL
  | INT
  | REAL
  | STRING
  [@@deriving show]

let rec coerceable a b =
  match a, b with
  | UNIT                      , UNIT                       -> true
  | BOOL                      , BOOL                       -> true
  | INT                       , INT                        -> true
  | REAL                      , REAL                       -> true
  | STRING                    , STRING                     -> true
  | _                                                      -> false


let name = Symbol.name
let map = List.map
let mkt = Tree.mkt
let intersperse = Util.intersperse

let rec show_ty = function
  | UNIT                      -> "UNIT"
  | BOOL                      -> "BOOL"
  | INT                       -> "INT"
  | REAL                      -> "REAL"
  | STRING                    -> "STRING"

let rec tree_of_ty = function
  | UNIT                      -> mkt "UNIT" []
  | BOOL                      -> mkt "BOOL" []
  | INT                       -> mkt "INT" []
  | REAL                      -> mkt "REAL" []
  | STRING                    -> mkt "STRING" []
