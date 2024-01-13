(** Copyright 2021-2023, Georgy Sichkar *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Errors
open Env_types.Common_env
open Env_types.Eval_env
open Monads.Eval_Monad

(* TODO: Добавить в локальное окружение все конструкторы,
   это этап создания общего окружения - ядра, на основе него потом будет производиться
   любое сужение.*)

(* TODO: Провести тесты на факториале *)

let is_assignable = function
  | IConst x -> return_n (IConst x)
  | _ -> fail (Runtime_error "There isn't a assignable value")
;;

let is_env_const = function
  | IConst x -> return_n x
  | _ -> fail (Runtime_error "There isn't a calculated value")
;;

let is_init = function
  | Init x -> return_n x
  | _ -> fail Using_an_uninitialized_variable
;;

let is_base = function
  | Null_v -> return_n Null_v
  | String_v x -> return_n (String_v x)
  | Int_v x -> return_n (Int_v x)
  | Bool_v x -> return_n (Bool_v x)
  | Char_v x -> return_n (Char_v x)
  | _ -> fail (Runtime_error "Not a base value")
;;

let is_class = function
  | Instance_v x -> return_n (Instance_v x)
  | _ -> fail (Runtime_error "Not a class instance value")
;;

let is_inst = function
  | Instance_v x -> return_n x
  | _ -> fail (Runtime_error "Not a instance value")
;;

let is_not_null = function
  | Null_v -> fail Trying_to_change_Null
  | x -> return_n x
;;

let is_int = function
  | Int_v x -> return_n x
  | _ -> fail (Runtime_error "Type mismatch")
;;

let is_bool = function
  | Bool_v x -> return_n x
  | _ -> fail (Runtime_error "Type mismatch")
;;

let is_sting = function
  | String_v x -> return_n x
  | _ -> fail (Runtime_error "Type mismatch")
;;

let is_char = function
  | Char_v x -> return_n x
  | _ -> fail (Runtime_error "Type mismatch")
;;

let is_env_code = function
  | ICode x -> return_n x
  | _ -> fail (Runtime_error "There isn't method or consructor")
;;

let rec get_point_access_value e_id e1 lenv =
  let get_id =
    match e_id with
    | EIdentifier id -> return_n id
    | _ -> fail (Runtime_error "Impossible before point argument in point_access")
  in
  let helper =
    match e1 with
    | EIdentifier id -> lift2 (fun ad v -> id, ad, v) read_self_ad (read_local_el id)
    | EPoint_access (e1, e2) -> get_point_access_value e1 e2 lenv
    | _ -> fail (Runtime_error "Impossible after point argument in point_access")
  in
  get_id
  >>= read_local_el
  >>= function
  | IConst (Init (Instance_v ad)) -> run_in_another_self ad lenv helper
  | _ -> fail (Runtime_error "Point_access is only available with instances of a class")
;;

let eval_point_access e_id e1 lenv_kernel =
  get_point_access_value e_id e1 lenv_kernel
  >>= fun (_, _, v) ->
  match v with
  | IConst (Init x) -> return_n (to_const @@ to_init x)
  | _ -> fail (Return_error "Eval of the expression hsven't implemented yet")
;;

let get_params_id (Params x) = List.map (fun (Var_decl (_, id)) -> id) x
let is_const_v x = is_env_const x >>= is_init >>= is_base
let is_not_null_const_v x = is_const_v x >>= is_not_null
let get_int x = is_not_null_const_v x >>= is_int
let ret_int x = return_n (create_val (Int_v x))
let update_int op x = get_int x >>= fun x -> ret_int (op x)
let get_bool x = is_not_null_const_v x >>= is_bool
let ret_bool x = return_n (create_val (Bool_v x))
let update_bool op x = get_bool x >>= fun x -> ret_bool (op x)
let get_string x = is_not_null_const_v x >>= is_sting
let ret_string x = return_n (create_val (String_v x))
let get_char x = is_not_null_const_v x >>= is_char
let ret_char x = return_n (create_val (Char_v x))
let get_inst x = is_not_null_const_v x >>= is_inst
let ret_inst x = return_n (create_val (Instance_v x))

let eval_instrs_ f e args lenv e_stm e_expr =
  let f_new = f e_stm e_expr lenv in
  match e with
  | EIdentifier id ->
    let cur_ad = read_self_ad in
    let mc_decl = read_local_el id in
    cur_ad >>= fun ad -> mc_decl >>= is_env_code >>= f_new ad args
  | EPoint_access (e_id, e1) ->
    let info = get_point_access_value e_id e1 lenv in
    info >>= fun (_, ad, decl) -> return_n decl >>= is_env_code >>= f_new ad args
  | _ -> fail (Runtime_error "Impossible method name")
;;

let m_eval_ e_stm _ lenv ad args = function
  | IMethod (sign, body) ->
    let prms = get_params_id sign.m_params in
    run_method sign.m_type prms args ad lenv body e_stm
  | IConstructor _ -> fail (Constructor_error "Trying to call a constructor without new")
;;

let eval_method e lenv e_stm e_expr args = eval_instrs_ m_eval_ e args lenv e_stm e_expr

let c_eval_ e_stm e_expr lenv ad args = function
  | IMethod _ -> fail (Constructor_error "'New' can be used only with constructor")
  | IConstructor (sign, body) ->
    let prms = get_params_id sign.con_params in
    let cl_decl =
      read_global (Code_ident sign.con_id) >>= fun x -> return_n @@ get_class_decl x
    in
    cl_decl
    >>= alloc_instance e_expr
    >>= fun new_ad -> run_method Void prms args ad lenv body e_stm *> return_n new_ad
;;

let eval_constructor e lenv e_stm e_expr args =
  eval_instrs_ c_eval_ e args lenv e_stm e_expr
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
     | _ -> fail (Constructor_error "'New' can be used only with constructor"))
;;

let eval_bin_op op e1 e2 e_expr lenv =
  let _op get_ op_t =
    let res = lift2 (fun r1 r2 -> r1, r2) (e_expr e1 >>= get_) (e_expr e2 >>= get_) in
    res >>= fun (r1, r2) -> op_t r1 r2
  in
  (* int *)
  let int_op op_t = _op get_int op_t in
  let int_f op r1 r2 = return_n (op r1 r2) >>= ret_int in
  (* bool *)
  let bool_op op_t = _op get_bool op_t in
  let bool_f op r1 r2 = return_n (op r1 r2) >>= ret_bool in
  (* --- *)
  let div_f op r1 = function
    | 0 -> fail Division_by_zero
    | r2 -> return_n (op r1 r2) >>= ret_int
  in
  let eq_op op_t = _op (fun x -> is_const_v x) op_t in
  let eq_f op r1 r2 = return_n (op r1 r2) >>= ret_bool in
  (* --- *)
  match op with
  | Asterisk -> int_op @@ int_f ( * )
  | Plus -> int_op @@ int_f ( + )
  | Minus -> int_op @@ int_f ( - )
  | Mod -> int_op (div_f ( mod ))
  | Division -> int_op (div_f ( / ))
  | And ->
    let lazy_and r1 r2 =
      (match r1 with
       | false -> return_n false
       | true -> return_n r2)
      >>= ret_bool
    in
    bool_op lazy_and
  | Or ->
    let lazy_or r1 r2 =
      (match r1 with
       | true -> return_n true
       | false -> return_n r2)
      >>= ret_bool
    in
    bool_op lazy_or
  | Less -> int_op @@ bool_f ( < )
  | LessOrEqual -> int_op @@ bool_f ( <= )
  | More -> int_op @@ bool_f ( > )
  | MoreOrEqual -> int_op @@ bool_f ( >= )
  | NotEqual -> eq_op @@ eq_f (fun r1 r2 -> not (equal_iconst r1 r2)) (* != *)
  | Equal -> eq_op @@ eq_f equal_iconst (* == *)
  | Assign ->
    let res = e_expr e2 >>= is_assignable in
    res
    >>= fun x ->
    (match e1 with
     | EIdentifier id -> update_local_el id x
     | EPoint_access (e_id, e1) ->
       get_point_access_value e_id e1 lenv
       >>= fun (id, ad, _) -> update_instance_el id ad x
     | _ -> fail Methods_cannot_be_assignable)
    *> res
;;

let eval_expr expr eval_stm lenv_kernel =
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
      | None -> fail (Return_error "Void methods can't be used with assignable types"))
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
    | Some _ -> fail (Return_error "As statement can be used only 'Void' method")
    | None -> return_n ())
  | EBin_op (op, e1, e2) -> e_expr (EBin_op (op, e1, e2)) *> return_n ()
  | _ -> fail (System_error "Trying to use an expression as a statement")
;;

let eval_statement stm lenv_kernel =
  let rec helper stm_ =
    let eval_expr expr = eval_expr expr helper lenv_kernel in
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
      eval_expr e
      >>= get_bool
      >>= (function
      | true -> helper stm
      | false ->
        (match stm_opt with
         | None -> return_n ()
         | Some stm -> helper stm))
    (* | SBreak -> *)
    (* | SThrow e -> *)
    (* | SWhile (e, stm) -> *)
    (* | SFor for_sign -> *)
    (* | STry_catch_fin tcf_sign -> *)
    | _ -> fail (Return_error "TODO: REMOVE")
  in
  helper stm
;;
