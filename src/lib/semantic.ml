(* semantic.ml *)

module A = Absyn
module S = Symbol
module E = Env
module T = Types


let type_mismatch loc expected found =
  Error.error loc "type mismatch: expected %s, found %s" (T.show_ty expected) (T.show_ty found)

let loc = Location.loc

let coerceable = T.coerceable

let coerce ty1 ty2 pos =
  if not (coerceable ty1 ty2) then
    type_mismatch pos ty2 ty1

let check_bool ty pos = coerce ty T.BOOL pos

let check_int ty pos = coerce ty T.INT pos

let check_real ty pos = coerce ty T.REAL pos

let check_string ty pos = coerce ty T.STRING pos

let check_unit ty pos = coerce ty T.UNIT pos


let rec check_exp ((tenv, venv, in_loop) as env) (pos, exp) =
  match exp with

  (* TODO: remaining expression *)

  | _ ->
     Error.fatal "unimplemented"


let type_check program =
  check_exp (E.base_tenv, E.base_venv, false) program
