(** Copyright 2021-2023, Georgy Sichkar *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Errors
open Env_types.Common_env
open Env_types.Type_check_env
open Monads.Type_check_Monad

let to_atype = function
  | Null -> None
  | VString _ -> Some (TNullable TString)
  | VInt _ -> Some (TNot_Nullable TInt)
  | VChar _ -> Some (TNot_Nullable TChar)
  | VBool _ -> Some (TNot_Nullable TBool)
;;

let to_var_type = function
  | Some x -> Some (TVar x)
  | None -> None
;;

let to_type_v = function
  | Some x -> Some (Value_sig x)
  | None -> None
;;

let to_type_m = function
  | Void -> Result.error "Not assignable type"
  | TReturn x -> Result.ok (Value_sig (TVar x))
;;

let to_type_m_opt = function
  | Void -> return None
  | TReturn x -> return (Some (Value_sig (TVar x)))
;;

let to_assign_t = function
  | Value_sig (TVar a_tp) -> Result.ok a_tp
  | Method_sig { m_modif = _; m_type; _ } ->
    (match m_type with
     | Void -> Result.error "Non-assignable type"
     | TReturn a_tp -> Result.ok a_tp)
  | Constructor_sig { con_modif = _; con_id; _ } -> Result.ok (TNullable (TClass con_id))
  | Fild_sig { f_modif = _; f_type = TVar tp; _ } -> Result.ok tp
;;

let opt_to_assign_t = function
  | None -> Result.ok None
  | Some a ->
    (match to_assign_t a with
     | Result.Ok a_tp -> Result.Ok (Some a_tp)
     | Result.Error x -> Result.error x)
;;

let compare a b f =
  let a_tp_res = opt_to_assign_t a in
  let b_tp_res = opt_to_assign_t b in
  match a_tp_res, b_tp_res with
  | Result.Error _, Result.Error _ | _, Result.Error _ | Result.Error _, _ ->
    Result.error "Non-comparable types"
  | Result.Ok a_tp_opt, Result.Ok b_tp_opt -> f (a_tp_opt, b_tp_opt)
;;

let compare_t_env_opt f a b =
  match f a b with
  | Result.Ok None -> return None
  | Result.Ok (Some x) -> return (Some (Value_sig (TVar x)))
  | Result.Error msg -> fail (Other_error msg)
;;

let ( =!= ) a b =
  let helper (a_tp_opt, b_tp_opt) =
    match a_tp_opt, b_tp_opt with
    | Some a_tp, Some b_tp when equal_assignable_type a_tp b_tp -> Result.ok a_tp_opt
    | Some (TNullable _), None -> Result.ok a_tp_opt
    | None, Some (TNullable _) -> Result.ok b_tp_opt
    | None, None -> Result.ok None
    | _, _ -> Result.error "Types are not equal"
  in
  compare a b helper
;;

let eq_t_env_opt a b = compare_t_env_opt ( =!= ) a b

let ( =!> ) a b =
  match a =!= b with
  | Result.Ok a_tp -> Result.Ok a_tp
  | Result.Error _ ->
    let helper (a_tp_opt, b_tp_opt) =
      match a_tp_opt, b_tp_opt with
      | Some (TNullable (TBase a_tp)), Some (TNot_Nullable b_tp)
        when equal_base_type a_tp b_tp -> Result.ok a_tp_opt
      | _, _ -> Result.error "Types are not equal."
    in
    compare a b helper
;;

let greater_t_env_opt a b = compare_t_env_opt ( =!> ) a b
let params_check f prms = map_left f prms

let v_decl_to_type_v = function
  | Var_decl (tp, _) -> Value_sig tp
;;

let is_success_list lst =
  let f acc el =
    match acc, el with
    | true, Result.Ok _ -> true
    | _, _ -> false
  in
  List.fold_left f true lst
;;

let map2_opt f l1 l2 =
  let len1 = List.length l1 in
  let len2 = List.length l2 in
  match Int.equal len1 len2 with
  | true -> Result.ok (List.map2 f l1 l2)
  | false -> Result.error "len(list1) != len(list2)"
;;

let check_method_ tp params args =
  let compare_prms = map2_opt ( =!> ) in
  let to_env elem = Some (v_decl_to_type_v elem) in
  let args_env = List.map to_env params in
  let result =
    args
    >>| fun prms_env ->
    match compare_prms args_env prms_env with
    | Result.Ok ans -> is_success_list ans
    | Result.Error _ -> false
  in
  result
  >>= function
  | true -> return (Some tp)
  | false -> fail Type_mismatch
;;

let check_invoke sign env_args =
  match sign with
  | Some (Method_sig { m_modif = _; m_type; m_id = _; m_params = Params params }) ->
    let tp = to_type_m m_type in
    (match tp with
     | Result.Error _ -> fail Type_mismatch
     | Result.Ok tp -> check_method_ tp params env_args)
  | Some (Constructor_sig { con_modif = _; con_id; con_params; base_args }) ->
    let tp = Value_sig (TVar (TNullable (TClass con_id))) in
    (match con_params, base_args with
     | Params params, None -> check_method_ tp params env_args
     | _ -> fail (Other_error "Inheritance with constructors is not supported"))
  | _ -> fail Type_mismatch
;;

let get_class_decl = function
  | Exception_ctx exc -> exc
  | Class_ctx cl -> cl
;;

let get_sign id el =
  let is_fild s id =
    match s with
    | { f_modif = _; f_type = _; f_id } when equal_ident f_id id -> Some (Fild_sig s)
    | _ -> None
  in
  let is_method s id =
    match s with
    | { m_modif = _; m_type = _; m_id; _ } when equal_ident m_id id -> Some (Method_sig s)
    | _ -> None
  in
  let is_constructor s id =
    match s with
    | { con_modif = _; con_id; _ } when equal_ident con_id id -> Some (Constructor_sig s)
    | _ -> None
  in
  match el with
  | Fild (sign, _) -> is_fild sign id
  | Method (sign, _) -> is_method sign id
  | Constructor (sign, _) -> is_constructor sign id
  | Main _ -> None
;;

let get_class_member_sign cl_d el_id =
  let f id acc el =
    match acc, el with
    | None, el -> get_sign id el
    | ret, _ -> ret
  in
  List.fold_left (f el_id) None cl_d.cl_mems
;;

let find_global id = read_global_el (Code_ident id) >>| fun x -> get_class_decl x

let is_public sign =
  match sign with
  | Fild_sig { f_modif = Some (FAccess MPublic); _ }
  | Method_sig { m_modif = Some (MAccess MPublic); _ }
  | Constructor_sig { con_modif = Some MPublic; _ } -> return sign
  | _ -> fail (Access_error "Attempt to get a private class member")
;;

let rec find_class_member expr cl_decl =
  let find_mem id =
    match get_class_member_sign cl_decl id with
    | Some x -> return x
    | None -> fail (Other_error "Parsing error in EPoint_access")
  in
  let get_next_decl_id mem_id =
    find_mem mem_id
    >>= is_public
    >>= function
    | Fild_sig { f_modif = _; f_type = TVar (TNullable (TClass id)); _ } -> return id
    | Method_sig { m_modif = _; m_type = TReturn (TNullable (TClass id)); _ } -> return id
    | Constructor_sig { con_modif = _; con_id; _ } -> return con_id
    | _ -> fail (Access_error "Magic case")
  in
  match expr with
  | EIdentifier id -> find_mem id >>= is_public
  | EPoint_access (EIdentifier id, expr1) ->
    get_next_decl_id id >>= find_global >>= find_class_member expr1
  | _ -> fail (Other_error "Parsing error in EPoint_access")
;;

let check_point_acc e1 e2 =
  let helper =
    match e1 with
    | EIdentifier id ->
      let local_find = read_local_el id in
      let global_find id_ = read_global_el (Code_ident id_) >>| get_class_decl in
      let get_class = function
        | Fild_sig { f_modif = _; f_type = TVar (TNullable (TClass id_)); _ } ->
          return id_
        | Value_sig (TVar (TNullable (TClass id_))) -> return id_
        | _ -> fail (Other_error "Error during parsing in Epoint_access")
      in
      local_find >>= get_class >>= global_find >>= find_class_member e2 >>= is_public
    | _ -> fail (Other_error "Error during parsing in Epoint_access")
  in
  helper >>| fun x -> Some x
;;

let env_bool = Value_sig (TVar (TNot_Nullable TBool))
let env_bool_null = Value_sig (TVar (TNullable (TBase TBool)))
let env_int = Value_sig (TVar (TNot_Nullable TInt))
let env_int_null = Value_sig (TVar (TNullable (TBase TInt)))
let env_char = Value_sig (TVar (TNot_Nullable TChar))
let env_char_null = Value_sig (TVar (TNullable (TBase TChar)))
let env_string = Value_sig (TVar (TNullable TString))

let operands_eq oper1 oper2 =
  match oper1, oper2 with
  | None, None | None, _ | _, None ->
    fail (Other_error "Using keyword Null with math operations")
  | _ -> eq_t_env_opt oper1 oper2
;;

let base_type_eq2 tp0 tp1 tp2 = tp0 >>= eq_t_env_opt tp1 <|> (tp0 >>= eq_t_env_opt tp2)

let check_bin_op op (env1, env2) =
  let is_operands_eq = operands_eq env1 env2 in
  let base_type_eq2 = base_type_eq2 is_operands_eq in
  (* _ *)
  let int_op = base_type_eq2 (Some env_int) (Some env_int_null) in
  let int_bool_op = int_op *> return (Some env_bool) in
  let bool_op = base_type_eq2 (Some env_bool) (Some env_bool_null) in
  (*  *)
  let compare_op = eq_t_env_opt env1 env2 in
  (* TODO: Тут разрешил проверку на равенство классов, придумать, как обрабатывать в интерпритаторе *)
  match op with
  | Asterisk | Plus | Minus | Division | Mod -> int_op
  | Less | LessOrEqual | More | MoreOrEqual -> int_bool_op
  | And | Or -> bool_op
  | Equal | NotEqual -> compare_op *> return (Some env_bool)
  | Assign -> greater_t_env_opt env1 env2
;;

let check_un_op op env1 =
  match op with
  | UMinus -> base_type_eq2 env1 (Some env_int) (Some env_int_null)
  | UNot -> base_type_eq2 env1 (Some env_bool) (Some env_bool_null)
  | New ->
    env1
    >>= (function
    | Some (Constructor_sig { con_modif = _; con_id; _ }) ->
      return (Some (Value_sig (TVar (TNullable (TClass con_id)))))
    | Some (Value_sig (TVar (TNullable (TClass id)))) ->
      read_global_el (Code_ident id)
      *> return (Some (Value_sig (TVar (TNullable (TClass id)))))
    | _ -> fail Type_mismatch)
;;

let check_expr exp =
  let get_local_ident id = read_local_el id >>| fun id -> Some id in
  let rec helper = function
    | EConst x ->
      (* None means "there is null" *)
      return (to_type_v @@ to_var_type @@ to_atype x)
    | EIdentifier x -> get_local_ident x
    | EMethod_invoke (e2, Args args) ->
      let is_Eident = function
        | EIdentifier x -> get_local_ident x
        | _ -> fail (Other_error "Just for skip")
      in
      let is_EPoint_acc = function
        | EPoint_access (e1, e2) -> check_point_acc e1 e2
        | _ -> fail Type_mismatch
      in
      is_Eident e2
      <|> is_EPoint_acc e2
      >>= fun x ->
      let env_val_prms = params_check helper args in
      check_invoke x env_val_prms
    | EPoint_access (e1, e2) ->
      let is_EPoint_acc = function
        | Some (Fild_sig _) -> return ()
        | _ -> fail Not_find_ident
      in
      check_point_acc e1 e2 >>= fun res -> is_EPoint_acc res *> return res
    | EBin_op (op, e1, e2) ->
      lift2 (fun x y -> x, y) (helper e1) (helper e2) >>= check_bin_op op
    | EUn_op (op, e1) -> check_un_op op (helper e1)
  in
  helper exp
;;

let local_scope mnd =
  read
  >>= fun old_ctx ->
  mnd
  >>= fun res ->
  read_main_ctx >>= fun id -> save old_ctx *> save_main_ctx id *> return res
;;

let var_exp_check e env_tp =
  let r_tp x = Some x in
  (check_expr e >>= fun e_tp -> greater_t_env_opt (r_tp env_tp) e_tp) *> return TP_Ok
;;

let return_check e_opt =
  read_scope_tp
  >>= fun tp ->
  match e_opt, tp with
  | None, Some Void -> return TP_Ok
  | Some e, Some (TReturn r) -> var_exp_check e (Value_sig (TVar r))
  | _ -> fail Type_mismatch
;;

let is_exception_ con_id =
  read_global_el (Code_ident con_id)
  >>= function
  | Exception_ctx _ -> return TP_Ok
  | _ -> fail (Other_error "throw can be used only with exceptions")
;;

let throw_check env_obj =
  let get_class_id =
    match env_obj with
    | Some (Constructor_sig { con_modif = _; con_id; _ }) -> return con_id
    | Some (Fild_sig { f_modif = _; f_type = TVar (TNullable (TClass id)) }) -> return id
    | Some (Value_sig (TVar (TNullable (TClass id)))) -> return id
    | _ -> fail (Other_error "throw can be used only with exceprions")
  in
  get_class_id >>= is_exception_
;;

let add_local id tp =
  let is_exist = read_local_el_opt id in
  is_exist
  >>= function
  | None -> save_local_el id tp
  | Some _ -> fail (Double_definition_of id)
;;

let add_global id tp =
  let is_exist = read_global_el_opt id in
  is_exist
  >>= function
  | None -> save_global_el id tp
  | Some _ ->
    let (Code_ident id) = id in
    fail (Double_definition_of id)
;;

let add_local_decl id tp = add_local id tp *> return TP_Ok
let cond_check_ e = base_type_eq2 (check_expr e) (Some env_bool) (Some env_bool_null)

let if_else_check e st st_opt st_check =
  let f _ _ _ = TP_Ok in
  let exp_res = cond_check_ e in
  let st_res = st_check st in
  let st_opt_res =
    match st_opt with
    | None -> return TP_Ok
    | Some st1 -> st_check st1
  in
  lift3 f exp_res st_res st_opt_res
;;

let un_opt_ mnd x =
  match x with
  | Some x -> mnd x *> return TP_Ok
  | None -> return TP_Ok
;;

let for_check init_opt e1_opt e2_opt s h =
  let f _ _ _ _ = TP_Ok in
  let init_opt_res = un_opt_ h init_opt in
  let e1_opt_res = un_opt_ cond_check_ e1_opt in
  let e2_opt_res = un_opt_ check_expr e2_opt in
  let s_res = h s in
  lift4 f init_opt_res e1_opt_res e2_opt_res s_res
;;

let catch_s_check_ h = function
  | None -> return TP_Ok
  | Some (edecl_opt, s) ->
    (match edecl_opt with
     | None -> h s
     | Some (CIdent exc_id, _) -> is_exception_ exc_id
     | Some (CDecl (Var_decl (tp, id)), e_opt) ->
       let tp = Value_sig tp in
       let add = add_local_decl id tp in
       let is_exc_ =
         match tp with
         | Value_sig (TVar (TNullable (TClass exc_id))) -> is_exception_ exc_id
         | _ -> fail (Other_error "Parsing error in catch statement")
       in
       (match e_opt with
        | Some e -> is_exc_ *> add *> cond_check_ e *> return TP_Ok
        | None -> is_exc_ *> add *> return TP_Ok))
;;

let try_catch_fin_check try_s catch_s finally_s h =
  let f _ _ _ = TP_Ok in
  let try_s_res = local_scope (h try_s) in
  let catch_s_res = local_scope (catch_s_check_ h catch_s) in
  let fin_s_res = local_scope (un_opt_ h finally_s) in
  match catch_s, finally_s with
  | None, None -> fail (Other_error "After try shold be catch or finally")
  | _ -> lift3 f try_s_res catch_s_res fin_s_res
;;

let check_statement stat =
  let rec helper = function
    | SExpr e -> check_expr e *> return TP_Ok
    | Steps stp ->
      let f acc = helper acc *> return () in
      let check_steps = iter_left f stp *> return TP_Ok in
      local_scope check_steps
    | SReturn e_opt -> return_check e_opt
    | SBreak -> return TP_Ok
    | SThrow e -> check_expr e >>= throw_check
    | SDecl (Var_decl (tp, id), e_opt) ->
      let tp = Value_sig tp in
      let add = add_local_decl id tp in
      (match e_opt with
       | Some e -> add *> var_exp_check e tp
       | None -> add)
    | SIf_else (e, s, st1) -> local_scope @@ if_else_check e s st1 helper
    | SWhile (e, s) -> local_scope @@ (cond_check_ e *> helper s)
    | SFor { f_init_p; f_cond_p; f_iter_p; f_body } ->
      local_scope @@ for_check f_init_p f_cond_p f_iter_p f_body helper
    | STry_catch_fin { try_s; catch_s; finally_s } ->
      try_catch_fin_check try_s catch_s finally_s helper
  in
  helper stat
;;

let add_params params =
  let (Params params) = params in
  let f = function
    | Var_decl (tp, id) -> add_local id (Value_sig tp)
  in
  iter_left f params
;;

let memb_check = function
  | Fild (sign, e_opt) ->
    (match e_opt with
     | Some e -> var_exp_check e (Fild_sig sign)
     | None -> return TP_Ok)
  | Main (m_type, s) -> local_scope @@ (save_scope_tp (Some m_type) *> check_statement s)
  | Method ({ m_modif = _; m_type; m_id = _; m_params }, s) ->
    local_scope
    @@ (save_scope_tp (Some m_type) *> add_params m_params *> check_statement s)
  | Constructor ({ con_modif = _; con_id; con_params; _ }, s) ->
    let cons_tp = TReturn (TNullable (TClass con_id)) in
    local_scope
    @@ (save_scope_tp (Some cons_tp) *> add_params con_params *> check_statement s)
;;

let create_cons_sign con_modif con_id con_params base_args =
  { con_modif; con_id; con_params; base_args }
;;

let default_cons id = create_cons_sign (Some MPublic) id (Params []) None

let class_check cl_decl =
  let { cl_modif; cl_id; parent; cl_mems } = cl_decl in
  let is_main id msg =
    if equal_ident id (Id "Main") then fail (Other_error msg) else return ()
  in
  let is_class_main = is_main cl_id "Main cann't be a class" in
  let add_class cl_decl_update =
    match parent with
    | Some id when equal_ident id Base_lib.exception_name ->
      add_global (Code_ident cl_id) (Exception_ctx cl_decl_update)
    | None -> add_global (Code_ident cl_id) (Class_ctx cl_decl_update)
    | _ ->
      fail
        (Other_error
           "Only rudimentary class inheritance is supported from Exception class")
  in
  let f = function
    | Fild _ | Method _ -> return ()
    | Main _ ->
      read_main_ctx
      >>= (function
      | Some _ -> fail (Double_definition_of (Id "Main"))
      | None -> save_main_ctx (Some (Code_ident cl_id)))
    | Constructor (sign, _) ->
      let { con_modif = _; con_id; _ } = sign in
      (match equal_ident con_id cl_id with
       | true -> add_local con_id (Constructor_sig sign)
       | false -> fail (Other_error "The constructor name must match the class name"))
  in
  (* if Constructor in class haven't been initialized *)
  let mb_con = Constructor_sig (default_cons cl_id) in
  let cons = Constructor (default_cons cl_id, Steps []) in
  let cl_mems_new = cons :: cl_mems in
  let add_constructor_and_main =
    iter_left f cl_mems
    *> (add_local cl_id mb_con
        *> return { cl_modif; cl_id; parent; cl_mems = cl_mems_new }
        <|> return cl_decl)
  in
  let g = function
    | Main _ | Constructor _ -> return ()
    | Fild (sign, _) ->
      let { f_modif = _; f_type = _; f_id } = sign in
      add_local f_id (Fild_sig sign)
    | Method (sign, _) ->
      let { m_modif = _; m_type = _; m_id; _ } = sign in
      add_local m_id (Method_sig sign)
  in
  let add_members = iter_left g cl_mems in
  let helper el = memb_check el *> return () in
  is_class_main *> add_constructor_and_main
  >>= fun x -> add_class x *> (local_scope @@ (add_members *> iter_left helper cl_mems))
;;

let run_with_base_exception =
  let cd = CodeMap.empty in
  match Base_lib.exception_decl with
  | Some second ->
    let hm = CodeMap.add (Code_ident Base_lib.exception_name) (Exception_ctx second) cd in
    continue (return ()) (hm, IdentMap.empty, None, None)
  | _ -> run (fail (Other_error "Base_lib Not connected"))
;;

let type_check_ ast =
  let (Ast ast) = ast in
  let type_checker_ = iter_left class_check ast in
  match run_with_base_exception with
  | (ctx, _, _, _), Result.Ok _ -> continue type_checker_ (ctx, IdentMap.empty, None, None)
  | _, Result.Error info -> run (fail info)
;;

let type_check ast =
  let (Ast ast) = ast in
  let type_checker_ = iter_left class_check ast in
  let lib_ctx =
    match Base_lib.base_lib_decl with
    | Some lib -> type_check_ lib
    | None -> run (fail (Other_error "Base_lib Not connected"))
  in
  match lib_ctx with
  | _, Result.Error info -> run (fail info)
  | (ctx, _, _, _), Result.Ok _ -> continue type_checker_ (ctx, IdentMap.empty, None, None)
;;

let type_check_with_main ast =
  match type_check ast with
  | (ctx, _, _, Some cl_decl), Result.Ok _ -> Result.ok (ctx, cl_decl)
  | _ -> Result.error "The program must have Main"
;;
