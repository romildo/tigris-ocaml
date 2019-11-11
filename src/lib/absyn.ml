(* absyn.ml *)

open Location

module S = Symbol

type operator =
  | PlusOp | MinusOp | TimesOp | DivOp | ModOp | PowOp
  | EqOp | NeOp
  | LtOp | LeOp | GtOp | GeOp
  | OrOp | AndOp
  [@@deriving show]

type parameter = S.symbol * S.symbol
  [@@deriving show]

type exp =
  | BoolExp   of bool
  | IntExp    of int
  | RealExp   of float
  | StringExp of string
  | VarExp    of lvar
  | AssignExp of lvar * lexp
  | CallExp   of S.symbol * lexp list
  | OpExp     of operator * lexp * lexp
  | IfExp     of lexp * lexp * lexp option
  | WhileExp  of lexp * lexp
  | BreakExp
  | SeqExp    of lexp list
  | LetExp    of ldec list * lexp
  [@@deriving show]

and var =
  | SimpleVar of S.symbol
  [@@deriving show]

and dec =
  | VarDec             of vardec
  | MutualFunctionDecs of fundec loc list
  | MutualTypeDecs     of typedec loc list
  [@@deriving show]

and vardec = S.symbol * lsymbol option * lexp
  [@@deriving show]

and fundec = S.symbol * parameter loc list * lsymbol option * lexp
  [@@deriving show]

and typedec = S.symbol * ltypecons
  [@@deriving show]

and typecons =
  | NameCons of S.symbol
  [@@deriving show]

and lexp = exp loc (* expression with a location *)

and lvar = var loc  (* variable with a location *)

and ldec = dec loc (* declaration with a location *)

and lfundec = fundec loc  (* function declaration with a location *)

and ltypecons = typecons loc (* type constructor with a location *)

and lsymbol = S.symbol loc (* symbol with a location *)
