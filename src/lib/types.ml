(* types.ml *)

type ty =
  | UNIT
  | BOOL
  | INT
  | REAL
  | STRING
  | NAME of Symbol.symbol * ty option ref
  [@@deriving show]

let rec actual_ty = function
  | NAME (_,{contents=Some t}) -> actual_ty t
  | t                          -> t

let rec coerceable a b =
  match a, b with
  | UNIT                      , UNIT                       -> true
  | BOOL                      , BOOL                       -> true
  | INT                       , INT                        -> true
  | INT                       , REAL                       -> true
  | REAL                      , REAL                       -> true
  | STRING                    , STRING                     -> true
  | NAME (_,{contents=Some t}), b                          -> coerceable t b
  | a                         , NAME (_,{contents=Some t}) -> coerceable a t
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
  | NAME (n,{contents=mt})    -> "NAME " ^ name n

let rec tree_of_ty = function
  | UNIT                      -> mkt "UNIT" []
  | BOOL                      -> mkt "BOOL" []
  | INT                       -> mkt "INT" []
  | REAL                      -> mkt "REAL" []
  | STRING                    -> mkt "STRING" []
  | NAME (n,{contents=mt})    -> mkt ("NAME " ^ name n) []
