(* semantic.ml *)

module A = Absyn
module S = Symbol
module E = Env
module T = Types


let type_mismatch loc expected found =
  Error.error loc "type mismatch: expected %s, found %s" (T.show_ty expected) (T.show_ty found)

let undefined loc kind id =
  Error.error loc "undefined %s %s" kind (S.name id)

let misdefined loc kind id =
  Error.error loc "%s is not a %s" (S.name id) kind


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


let look env kind id pos =
  match S.look id env with
  | Some x -> x
  | None -> undefined pos kind id

let tylook tenv id pos =
  look tenv "type" id pos

let varlook venv id pos =
  match look venv "variable" id pos with
  | E.VarEntry t -> t
  | E.FunEntry _ -> misdefined pos "variable" id


let rec check_exp ((tenv, venv, in_loop) as env) (pos, exp) =
  match exp with
  | A.BoolExp _ -> T.BOOL

  | A.IntExp _ -> T.INT

  | A.WhileExp (test, body) ->
     check_bool (check_exp env test) (loc test);
     ignore (check_exp (tenv, venv, true) body);
     T.UNIT

  | A.LetExp (decs, body) ->
     let env' = List.fold_left check_dec env decs in
     check_exp env' body

  | A.VarExp var -> check_var env var

  (* TODO: remaining expression *)

  | _ ->
     Error.fatal "unimplemented"


and check_dec ((tenv, venv, in_loop) as env) (pos, dec) =
  match dec with
  | A.VarDec (name,type_opt,init) ->
     let tinit = check_exp env init in
     let tvar =
       match type_opt with
       | Some (pos,tname) -> let t = tylook tenv tname pos in
                             coerce tinit t (loc init);
                             t
       | None -> tinit
     in
     let venv' = S.enter name (E.VarEntry tvar) venv in
     (tenv,venv',in_loop)

  | _ ->
     Error.fatal "unimplemented"

and check_var ((tenv,venv,in_loop) as env) (pos,var) =
  match var with
  | A.SimpleVar id -> varlook venv id pos

  | _ ->
     Error.fatal "unimplemented"


let type_check program =
  check_exp (E.base_tenv, E.base_venv, false) program
