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

let is_env_const = function
  | IConst x -> return_n x
  | _ -> fail (Runtime_error "There isn't a calculated value")
;;

let is_init = function
  | Init x -> return_n x
  | _ -> fail Using_an_uninitialized_variable
;;

let is_base = function
  | IBase_value x -> return_n x
  | _ -> fail (Runtime_error "Not a base value")
;;

let is_not_null = function
  | Null -> fail Trying_to_change_Null
  | x -> return_n x
;;

let is_int = function
  | VInt x -> return_n x
  | _ -> fail (Runtime_error "Type mismatch")
;;

let is_bool = function
  | VBool x -> return_n x
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
    | EIdentifier id -> lift2 (fun ad v -> ad, v) read_self_ad (read_local_el id)
    | EPoint_access (e1, e2) -> get_point_access_value e1 e2 lenv
    | _ -> fail (Runtime_error "Impossible after point argument in point_access")
  in
  get_id
  >>= read_local_el
  >>= function
  | IConst (Init (IInstance ad)) -> run_in_another_self ad lenv helper
  | _ -> fail (Runtime_error "Point_access is only available with instances of a class")
;;

let get_params_id (Params x) = List.map (fun (Var_decl (_, id)) -> id) x
let get_int x = is_env_const x >>= is_init >>= is_base >>= is_not_null >>= is_int
let get_bool x = is_env_const x >>= is_init >>= is_base >>= is_not_null >>= is_bool
let ret_int x = return_n (create_base_val (VInt x))
let ret_bool x = return_n (create_base_val (VBool x))
let update_int op x = get_int x >>= fun x -> ret_int (op x)
let update_bool op x = get_bool x >>= fun x -> ret_bool (op x)

let m_eval_ eval_st _ lenv ad args = function
  | IMethod (sign, body) ->
    let prms = get_params_id sign.m_params in
    run_method sign.m_type prms args ad lenv body eval_st
  | IConstructor _ -> fail (Constructor_error "Trying to call a constructor without new")
;;

let eval_instrs_ f e args lenv eval_st e_expr =
  let f_new = f eval_st e_expr lenv in
  match e with
  | EIdentifier id ->
    let cur_ad = read_self_ad in
    let mc_decl = read_local_el id in
    cur_ad >>= fun ad -> mc_decl >>= is_env_code >>= f_new ad args
  | EPoint_access (e_id, e1) ->
    let info = get_point_access_value e_id e1 lenv in
    info >>= fun (ad, decl) -> return_n decl >>= is_env_code >>= f_new ad args
  | _ -> fail (Runtime_error "Impossible method name")
;;

let eval_method e lenv e_st e_expr args = eval_instrs_ m_eval_ e args lenv e_st e_expr

let c_eval_ eval_st e_expr lenv ad args = function
  | IMethod _ -> fail (Constructor_error "'New' can be used only with constructor")
  | IConstructor (sign, body) ->
    let prms = get_params_id sign.con_params in
    let cl_decl =
      read_global (Code_ident sign.con_id) >>= fun x -> return_n @@ get_class_decl x
    in
    cl_decl
    >>= alloc_instance e_expr
    >>= fun new_ad -> run_method Void prms args ad lenv body eval_st *> return_n new_ad
;;

let eval_constructor e lenv e_st e_expr args =
  eval_instrs_ c_eval_ e args lenv e_st e_expr
;;

let eval_un_op un_op res lenv_kernel e_st e_expr =
  let get_res = e_expr res in
  match un_op with
  | UMinus -> get_res >>= update_int ( ~- )
  | UNot -> get_res >>= update_bool not
  | New ->
    (match res with
     | EMethod_invoke (e, Args args) ->
       let args = map_left e_expr args in
       args
       >>= eval_constructor e lenv_kernel e_st e_expr
       >>= fun ad -> return_n @@ create_inst ad
     | _ -> fail (Constructor_error "'New' can be used only with constructor"))
;;

let eval_bin_op op e1 e2 e_expr =
  let int_op op_t =
    let res =
      lift2 (fun r1 r2 -> r1, r2) (e_expr e1 >>= get_int) (e_expr e2 >>= get_int)
    in
    res >>= fun (r1, r2) -> op_t r1 r2
  in
  let div_f op r1 = function
    | 0 -> fail Division_by_zero
    | r2 -> return_n (op r1 r2) >>= ret_int
  in
  match op with
  | Asterisk -> int_op (fun r1 r2 -> return_n (r1 * r2)) >>= ret_int
  | Plus -> int_op (fun r1 r2 -> return_n (r1 + r2)) >>= ret_int
  | Minus -> int_op (fun r1 r2 -> return_n (r1 - r2)) >>= ret_int
  | Mod -> int_op (div_f ( mod ))
  | Division -> int_op (div_f ( / ))
  (* | Equal -> *)
  (* | NotEqual -> *)
  (* | Less -> *)
  (* | LessOrEqual -> *)
  (* | More -> *)
  (* | MoreOrEqual -> *)
  (* | And -> *)
  (* | Or -> *)
  (* | Assign -> *)
  | _ -> fail (Constructor_error "TODO: remove")
;;

let eval_expr expr eval_st lenv_kernel =
  (* ?применять is_env_const на полученном после рекурсивного применения eval_expr результате? *)
  let rec helper = function
    | EConst x -> to_const @@ to_init @@ to_val x |> return_n
    | EIdentifier id -> read_local_el id
    | EPoint_access (e_id, e1) ->
      get_point_access_value e_id e1 lenv_kernel
      >>= fun (_, v) ->
      (match v with
       | IConst (Init x) -> return_n (to_const @@ to_init x)
       | _ -> fail (Return_error "Eval of the expression hsven't implemented yet"))
    | EMethod_invoke (e, Args args) ->
      let args = map_left helper args in
      args
      >>= eval_method e lenv_kernel eval_st None
      >>= (function
      | Some x -> return_n x
      | None -> fail (Return_error "Void methods can't be used with assignable types"))
    | EUn_op (un, e) -> eval_un_op un e lenv_kernel eval_st helper
    | EBin_op (bin, e1, e2) -> eval_bin_op bin e1 e2 helper
  in
  helper expr
;;
