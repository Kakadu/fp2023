(** Copyright 2021-2023, Georgy Sichkar *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Errors
open Common_types
open Env_types.Eval_env
open Monads.Eval
open Interpret_converters

let rec get_point_access_value e_id e1 l_env_l =
  let get_id =
    match e_id with
    | EIdentifier id -> return_n id
    | _ ->
      fail
        (Interpret_error
           (Runtime_error "Impossible before point argument in point_access"))
  in
  let helper =
    match e1 with
    | EIdentifier id -> lift2 (fun ad v -> id, ad, v) read_self_ad (read_local_el id)
    | EPoint_access (e1, e2) -> get_point_access_value e1 e2 l_env_l
    | _ ->
      fail
        (Interpret_error (Runtime_error "Impossible after point argument in point_access"))
  in
  get_id
  >>= read_local_el
  >>= function
  | IConst (Init (Instance_v ad)) -> run_in_another_self ad l_env_l helper
  | _ ->
    fail
      (Interpret_error
         (Runtime_error "Point_access is only available with instances of a class"))
;;

let eval_point_access e_id e1 lenv_kernel =
  get_point_access_value e_id e1 lenv_kernel
  >>= fun (_, _, v) ->
  match v with
  | IConst (Init x) -> return_n (to_const @@ to_init x)
  | _ ->
    fail
      (Interpret_error (Runtime_error "Eval of the expression haven't implemented yet"))
;;

let eval_instrs_ f e args l_env_l e_stm e_expr =
  let f_new = f e_stm e_expr l_env_l in
  match e with
  | EIdentifier id ->
    let cur_ad = read_self_ad in
    let mc_decl = read_local_el id in
    cur_ad >>= fun ad -> mc_decl >>= is_env_code >>= f_new ad args
  | EPoint_access (e_id, e1) ->
    let info = get_point_access_value e_id e1 l_env_l in
    info >>= fun (_, ad, decl) -> return_n decl >>= is_env_code >>= f_new ad args
  | _ -> fail (Interpret_error (Runtime_error "Impossible method name"))
;;

let m_eval_ e_stm _ l_env_l ad args = function
  | IMethod (sign, body) ->
    let get_sys_meth_opt =
      read_inst_cl ad >>| fun id -> Base_lib.get_system_method_opt id sign.m_id
    in
    let prms = get_params_id sign.m_params in
    get_sys_meth_opt
    >>= (function
           | Some f -> return_n f
           | None -> return_n @@ e_stm body)
    >>= run_method sign.m_type prms args ad l_env_l
  | IConstructor _ ->
    fail (Interpret_error (Syntax_error "Trying to call a constructor without new"))
;;

let eval_method e l_env_l e_stm e_expr args =
  eval_instrs_ m_eval_ e args l_env_l e_stm e_expr
;;

let c_eval_ e_stm e_expr l_env_l _ args = function
  | IMethod _ ->
    fail (Interpret_error (Syntax_error "'New' can be used only with constructor"))
  | IConstructor (sign, body) ->
    let prms = get_params_id sign.con_params in
    let cl_decl =
      read_global (Code_ident sign.con_id) >>= fun x -> return_n @@ get_class_decl x
    in
    let eval_constructor =
      cl_decl
      >>= fun x ->
      alloc_instance e_expr x
      >>= fun new_ad ->
      let f = e_stm body in
      run_method Void prms args new_ad l_env_l f *> return_n new_ad
    in
    Base_lib.run_sys_constructor_or_normal eval_constructor (Code_ident sign.con_id)
;;

let eval_constructor e l_env_l e_stm e_expr args =
  eval_instrs_ c_eval_ e args l_env_l e_stm e_expr >>= return_n
;;

let eval_un_op un_op res lenv_kernel e_stm e_expr =
  let get_res = e_expr res in
  match un_op with
  | UMinus -> get_res >>= update_int ( ~- )
  | UNot -> get_res >>= update_bool not
  | New ->
    (match res with
     | EMethod_invoke (e, Args args) ->
       let args = map_left e_expr args in
       args
       >>= eval_constructor e lenv_kernel e_stm e_expr
       >>= fun ad -> return_n @@ create_inst ad
     | _ ->
       fail (Interpret_error (Syntax_error "'New' can be used only with constructor")))
;;

let eval_bin_op op e1 e2 e_expr l_env_l =
  let op_ get_ op_t =
    let res = lift2 (fun r1 r2 -> r1, r2) (e_expr e1 >>= get_) (e_expr e2 >>= get_) in
    res >>= fun (r1, r2) -> op_t r1 r2
  in
  (* int *)
  let int_op op_t = op_ get_int op_t in
  let int_f op r1 r2 = return_n (op r1 r2) >>= ret_int in
  (* bool *)
  let bool_f op r1 r2 = return_n (op r1 r2) >>= ret_bool in
  (* --- *)
  let div_f op r1 = function
    | 0 -> fail (Interpret_error Division_by_zero)
    | r2 -> return_n (op r1 r2) >>= ret_int
  in
  let eq_op op_t = op_ (fun x -> is_v x) op_t in
  let eq_f op r1 r2 = return_n (op r1 r2) >>= ret_bool in
  let lazy_ f get_ = e_expr e1 >>= get_ >>= f (e_expr e1 >>= get_) in
  (* --- *)
  match op with
  | Asterisk -> int_op @@ int_f ( * )
  | Plus -> int_op @@ int_f ( + )
  | Minus -> int_op @@ int_f ( - )
  | Mod -> int_op (div_f ( mod ))
  | Division -> int_op (div_f ( / ))
  | And ->
    let op_and f = function
      | false -> return_n false
      | true -> f
    in
    lazy_ op_and get_bool >>= ret_bool
  | Or ->
    let op_or f = function
      | true -> return_n true
      | false -> f
    in
    lazy_ op_or get_bool >>= ret_bool
  | Less -> int_op @@ bool_f ( < )
  | LessOrEqual -> int_op @@ bool_f ( <= )
  | More -> int_op @@ bool_f ( > )
  | MoreOrEqual -> int_op @@ bool_f ( >= )
  | NotEqual -> eq_op @@ eq_f (fun r1 r2 -> not (equal_iconst r1 r2)) (* != *)
  | Equal -> eq_op @@ eq_f equal_iconst (* == *)
  | Assign ->
    let res = e_expr e2 >>= is_assignable in
    res
    >>= is_init_v
    >>= fun x ->
    (match e1 with
     | EIdentifier id -> update_local_el id x
     | EPoint_access (e_id, e1) ->
       get_point_access_value e_id e1 l_env_l
       >>= fun (id, ad, _) -> update_instance_el id ad x
     | _ -> fail (Interpret_error (Syntax_error "Methods cannot be assignable")))
    *> res
;;

let eval_expr eval_stm lenv_kernel expr =
  let rec helper = function
    | EConst x -> to_const @@ to_init @@ val_to_iconst x |> return_n
    | EIdentifier id -> read_local_el id
    | EPoint_access (e_id, e1) -> eval_point_access e_id e1 lenv_kernel
    | EMethod_invoke (e, Args args) ->
      let args = map_left helper args in
      args
      >>= eval_method e lenv_kernel eval_stm None
      >>= (function
       | Some x -> return_n x
       | None ->
         fail
           (Interpret_error
              (Runtime_error "Void methods can't be used with assignable types")))
    | EUn_op (un, e) -> eval_un_op un e lenv_kernel eval_stm helper
    | EBin_op (bin, e1, e2) -> eval_bin_op bin e1 e2 helper lenv_kernel
  in
  helper expr
;;

let eval_stm_expr e_expr e_stm lenv_kernel = function
  | EMethod_invoke (e, Args args) ->
    let args = map_left e_expr args in
    args
    >>= eval_method e lenv_kernel e_stm None
    >>= (function
     | Some _ ->
       fail
         (Interpret_error (Runtime_error "As statement can be used only 'Void' method"))
     | None -> return_n ())
  | EBin_op (op, e1, e2) -> e_expr (EBin_op (op, e1, e2)) *> return_n ()
  | _ ->
    fail (Interpret_error (System_error "Trying to use an expression as a statement"))
;;

let catch_eval ad e_stm l_env_l = function
  | None -> return_n ()
  | Some catch_l ->
    let f acc (cond, body) =
      local
        (match cond, acc with
         | None, None -> e_stm body *> return_n (Some ())
         | Some (c_decl, e_opt), None ->
           let eval_cond =
             match e_opt with
             | None -> return_n true
             | Some e -> eval_expr e_stm l_env_l e >>= get_bool
           in
           let eval_body = function
             | true -> e_stm body *> return_n (Some ())
             | false -> return_n None
           in
           let accept_cond inh_cl_id cl_id =
             equal_ident inh_cl_id cl_id || equal_ident inh_cl_id Base_lib.Exception.name
           in
           let is_inheritance cl_tp cl_id config =
             match cl_tp with
             | TVar (TNullable (TClass inh_cl_id)) when accept_cond inh_cl_id cl_id ->
               config *> eval_cond
             | _ -> return_n false
           in
           read_inst_cl ad
           >>= fun (Code_ident cl_id) ->
           (match c_decl with
            | CDecl (Var_decl (cl_tp, name)) ->
              let update_env = add_local_el name (IConst (Init (Instance_v ad))) in
              is_inheritance cl_tp cl_id update_env >>= eval_body
            | CExn_id inh_cl_id when accept_cond inh_cl_id cl_id ->
              (match e_opt with
               | Some _ ->
                 fail
                   (Interpret_error
                      (Runtime_error
                         "An exception filter can only be written for a declaration in \
                          catch"))
               | None -> e_stm body *> return_n (Some ()))
            | _ ->
              fail (Interpret_error (Runtime_error "Using normal class as an exception")))
         | _ -> return_n None)
    in
    fold_left f None catch_l
    >>= (function
     | Some _ -> return_n ()
     | None -> return_e ad)
;;

let eval_try_catch_fin e_stm l_env_l try_ catch_ fin_ =
  let f_try = e_stm try_ in
  let f_catch ad = catch_eval ad e_stm l_env_l catch_ in
  let f_finally =
    match fin_ with
    | Some stm -> e_stm stm
    | None -> return_n ()
  in
  run_tcf f_try f_catch f_finally
;;

let eval_while e_stm l_env_l cond_e body =
  let f_cond = eval_expr e_stm l_env_l cond_e >>= get_bool >>= return_n in
  let f_body = e_stm body in
  run_loop f_cond f_body
;;

let eval_for e_stm l_env_l init cond iter body =
  let f_cond =
    let eval_cond e = eval_expr e_stm l_env_l e >>= get_bool in
    let eval_iter e = eval_expr e_stm l_env_l e in
    match cond, iter with
    | Some cond_e, Some iter_e ->
      eval_cond cond_e >>= fun res -> eval_iter iter_e *> return_n res
    | Some cond_e, None -> eval_cond cond_e
    | None, Some iter_e -> eval_iter iter_e *> return_n true
    | None, None -> return_n true
  in
  let f_body = e_stm body in
  local
  @@ ((match init with
       | Some stm -> e_stm stm
       | None -> return_n ())
      *> run_loop f_cond f_body)
;;

let eval_statement lenv_kernel stm =
  let rec helper stm_ =
    let eval_expr expr = eval_expr helper lenv_kernel expr in
    match stm_ with
    | SExpr e -> eval_stm_expr eval_expr helper lenv_kernel e
    | SDecl (Var_decl (_, id), e_opt) ->
      (match e_opt with
       | None -> add_local_el id (IConst Not_init)
       | Some e -> eval_expr e >>= is_assignable >>= add_local_el id)
    | SReturn e_opt ->
      (match e_opt with
       | None -> return_r None
       | Some e -> eval_expr e >>= is_assignable >>= fun x -> return_r (Some x))
    | Steps stm_l -> local @@ iter_left helper stm_l
    | SIf_else (e, stm, stm_opt) ->
      local @@ eval_expr e
      >>= get_bool
      >>= (function
       | true -> helper stm
       | false ->
         (match stm_opt with
          | None -> return_n ()
          | Some stm -> helper stm))
    | SBreak -> return_b ()
    | SThrow e -> eval_expr e >>= get_inst >>= return_e
    | STry_catch_fin { try_s; catch_s; finally_s } ->
      eval_try_catch_fin helper lenv_kernel try_s catch_s finally_s
    | SWhile (e, stm) -> eval_while helper lenv_kernel e stm
    | SFor { f_init_p; f_cond_p; f_iter_p; f_body } ->
      eval_for helper lenv_kernel f_init_p f_cond_p f_iter_p f_body
  in
  helper stm
;;

let eval_expr l_env_l = eval_expr (eval_statement l_env_l) l_env_l

let interpret_ genv cl_id =
  let (Code_ident main_cl_id) = cl_id in
  let local_env = Ident_Map.empty in
  let lenv_with_constructors =
    let f cl_id cl_decl = function
      | None -> None
      | Some acc ->
        (match cl_id with
         | Code_ident id when equal_ident id main_cl_id -> Some acc
         | Code_ident id ->
           find_cl_meth id cl_decl
           |> (function
            | None -> None
            | Some constr -> Some (Ident_Map.add id (ICode constr) acc)))
    in
    Code_Map.fold f genv (Some local_env)
  in
  let run_main l_env_l =
    let l_env_l = [ l_env_l ] in
    let cl_decl_t = read_global cl_id >>= fun x -> return_n @@ get_class_decl x in
    let get_main cl_decl =
      let f res_opt el =
        match res_opt, el with
        | Some x, _ -> Some x
        | None, Method (sign, body) when equal_ident Base_lib.main_method sign.m_id ->
          Some (sign.m_type, body)
        | _ -> None
      in
      List.fold_left f None cl_decl.cl_mems
      |> function
      | Some (tp, body) -> return_n (tp, body)
      | _ -> fail (Interpret_error (Runtime_error "The program must contain 'Main'"))
    in
    cl_decl_t
    >>= fun cl_decl ->
    alloc_instance (eval_expr l_env_l) cl_decl
    >>= fun main_ad ->
    save_self_ad main_ad *> get_main cl_decl
    >>= fun (tp, body) ->
    let f = (eval_statement l_env_l) body in
    run_method tp [] [] main_ad l_env_l f
  in
  let eval =
    match lenv_with_constructors with
    | Some l_env_l -> run_main l_env_l
    | None -> fail (Interpret_error (Runtime_error "Some class has no constructor"))
  in
  run genv eval
;;

let interpret str =
  match Parser.parse_ast str with
  | Result.Error x ->
    Result.error (Interpret_error (Runtime_error ("Parsing error: " ^ x ^ "\n")))
  | Result.Ok x ->
    (match Type_check.type_check_with_main x with
     | Result.Error x -> Result.error x
     | Result.Ok (genv, main_id) ->
       (match interpret_ genv main_id with
        | _, Eval_res x -> Result.ok x
        | _, Signal (Err x) -> Result.error x
        | (_, _, (_, mem), _), Signal (Exn x) ->
          (match Mem_Map.find_opt x mem with
           | Some (cl_id, _) -> Result.error (Interpret_error (User_exception cl_id))
           | None ->
             Result.error (Interpret_error (System_error "Exception instance missing")))
        | _, _ -> Result.error (Interpret_error (System_error "Unrecognized signal"))))
;;
