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

let check_arithmetic t u pos1 pos2 =
  if coerceable t T.INT then
    if coerceable u T.INT then
      T.INT
    else if coerceable u T.REAL then
      T.REAL
    else
      Error.error pos2 "type mismatch: int or real expected"
  else if coerceable t T.REAL then
    if coerceable u T.INT then
      T.REAL
    else if coerceable u T.REAL then
      T.REAL
    else
      Error.error pos2 "type mismatch: int or real expected"
  else
    Error.error pos1 "type mismatch: int or real expected"

let check_relational t u pos1 pos2 =
  if (coerceable t T.INT
      || coerceable t T.REAL
      || coerceable t T.STRING
      || coerceable t T.BOOL) then
    if coerceable t u || coerceable u t then
      T.BOOL
    else
      type_mismatch pos2 t u
  else
    Error.error pos1 "type mismatch: int, real, string or boolean expected"

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

let funlook venv id pos =
  match look venv "function" id pos with
  | E.VarEntry _ -> misdefined pos "function" id
  | E.FunEntry (params, result) -> (params, result)

let rec check_exp ((tenv, venv, in_loop) as env) (pos, exp) =
  match exp with
  | A.BoolExp _ -> T.BOOL

  | A.IntExp _ -> T.INT

  | A.RealExp _ -> T.REAL

  | A.StringExp _ -> T.STRING

  | A.WhileExp (test, body) ->
     check_bool (check_exp env test) (loc test);
     ignore (check_exp (tenv, venv, true) body);
     T.UNIT

  | A.LetExp (decs, body) ->
     let env' = List.fold_left check_dec env decs in
     check_exp env' body

  | A.VarExp var -> check_var env var

  | A.AssignExp (var, exp) ->
     let tvar = check_var env var in
     let texp = check_exp env exp in
     coerce texp tvar (loc exp);
     T.UNIT

  | A.OpExp (op, l, r) ->
    let tl = check_exp env l in
    let tr = check_exp env r in
    begin match op with
    | A.PlusOp | A.MinusOp | A.TimesOp | A.DivOp | A.ModOp | A.PowOp ->
       check_arithmetic tl tr (loc l) (loc r)
    | A.LtOp | A.LeOp | A.GtOp | A.GeOp ->
       check_relational tl tr (loc l) (loc r)
    | A.AndOp | A.OrOp ->
       check_bool tl (loc l);
       check_bool tr (loc r);
       T.BOOL
    | A.EqOp | A.NeOp ->
       if not (coerceable tl tr || coerceable tr tl) then
         type_mismatch pos tl tr;
       T.BOOL
    end

  | A.SeqExp exps ->
     let rec check_seq seq =
       match seq with
       | []        -> T.UNIT
       | [exp]     -> check_exp env exp
       | exp::rest -> ignore (check_exp env exp); check_seq rest
     in
     check_seq exps

  | A.BreakExp ->
     if not in_loop then
       Error.error pos "break cannot appear outside a loop";
     T.UNIT

  | A.IfExp (test, e1, e2optional) ->
     let ttest = check_exp env test in
     let t1 = check_exp env e1 in
     check_bool ttest (loc test);
     begin match e2optional with
     | None -> check_unit t1 (loc e1); T.UNIT
     | Some e2 ->
        let t2 = check_exp env e2 in
        if coerceable t1 t2 then
          t2
        else if coerceable t2 t1 then
          t1
        else
          type_mismatch (loc e2) t1 t2
     end

  | A.CallExp (f, args) ->
     let (params, result) = funlook venv f pos in
     let rec check_args params args =
       match params, args with
       | p::params_rest , a::args_rest ->
          coerce (check_exp env a) p (loc a);
          check_args params_rest args_rest
       | [] , [] -> result
       | [] , _ -> Error.error pos "too much arguments"
       | _ , [] -> Error.error pos "too few arguments"
     in
     check_args params args

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

  | A.MutualFunctionDecs fdecs ->
     let venv' = List.fold_left (check_signature tenv) venv fdecs in
     List.iter (check_body tenv venv') fdecs;
     (tenv, venv', in_loop)

  | A.MutualTypeDecs tdecs ->
     let add_new_type tenv (_, (tname, _)) =
       S.enter tname (T.NAME (tname, ref None)) tenv in
     let fix_new_type tenv (tloc, (tname, tcons)) =
       match tylook tenv tname tloc with
       | T.NAME (_, cell) -> cell := Some (check_tcons tenv tcons)
       | _ -> Error.fatal "fix_type"
     in
     let tenv' = List.fold_left add_new_type tenv tdecs in
     List.iter (fix_new_type tenv') tdecs;
     (tenv', venv, in_loop)

  | _ ->
     Error.fatal "unimplemented"

and check_var ((tenv,venv,in_loop) as env) (pos,var) =
  match var with
  | A.SimpleVar id -> varlook venv id pos

  | _ ->
     Error.fatal "unimplemented"

and check_tcons tenv (pos, tcons) =
  match tcons with
  | A.NameCons tname -> tylook tenv tname pos

  | _ -> Error.fatal "unimplemented"

and check_signature tenv venv (_, (name, params, result_opt, _)) =
  (* check duplicate parameter names *)
  ignore
    (List.fold_left
       (fun all (ploc,(pname,_)) ->
         if List.mem pname all then
           Error.error ploc "duplicate parameter name %s" (S.name pname)
         else
           (pname::all))
       []
       params);
  (* calculate type of parameters *)
  let tparams = List.map (fun (ploc,(_,ptype)) -> tylook tenv ptype ploc) params
  (* calculate type of result *)
  and tresult =
    match result_opt with
    | Some (pos,result) -> tylook tenv result pos
    | None -> T.UNIT
  in
  (* add function name into symbol table *)
  S.enter name (E.FunEntry (tparams, tresult)) venv

and check_body tenv venv (pos, (name, params, _, body)) =
  let (formals, result) = funlook venv name pos in
  let venv' =
    List.fold_left2
      (fun venv (_,(name,_)) t -> S.enter name (E.VarEntry t) venv)
      venv
      params
      formals
  in
  let tbody = check_exp (tenv, venv', false) body in
  coerce tbody result (loc body)

let type_check program =
  check_exp (E.base_tenv, E.base_venv, false) program
