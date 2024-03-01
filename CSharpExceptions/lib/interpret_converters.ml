(** Copyright 2021-2023, Georgy Sichkar *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Errors
open Env_types.Eval_env
open Monads.Eval

let is_assignable = function
  | IConst x -> return_n (IConst x)
  | _ -> fail (Interpret_error (Runtime_error "There isn't a assignable value"))
;;

let is_env_const = function
  | IConst x -> return_n x
  | _ -> fail (Interpret_error (Runtime_error "There isn't a calculated value"))
;;

let is_init = function
  | Init x -> return_n x
  | _ -> fail (Interpret_error Uninitialized_variable)
;;

let is_base = function
  | Null_v -> return_n Null_v
  | String_v x -> return_n (String_v x)
  | Int_v x -> return_n (Int_v x)
  | Bool_v x -> return_n (Bool_v x)
  | Char_v x -> return_n (Char_v x)
  | _ -> fail (Interpret_error (Runtime_error "Not a base value"))
;;

let is_class = function
  | Instance_v x -> return_n (Instance_v x)
  | _ -> fail (Interpret_error (Runtime_error "Not a class instance value"))
;;

let is_inst = function
  | Instance_v x -> return_n x
  | _ -> fail (Interpret_error (Runtime_error "Not a instance value"))
;;

let is_not_null = function
  | Null_v -> fail (Interpret_error Null_reference)
  | x -> return_n x
;;

let is_int = function
  | Int_v x -> return_n x
  | _ -> fail (Interpret_error Type_mismatch)
;;

let is_bool = function
  | Bool_v x -> return_n x
  | _ -> fail (Interpret_error Type_mismatch)
;;

let is_sting = function
  | String_v x -> return_n x
  | _ -> fail (Interpret_error Type_mismatch)
;;

let is_char = function
  | Char_v x -> return_n x
  | _ -> fail (Interpret_error Type_mismatch)
;;

let is_env_code = function
  | ICode x -> return_n x
  | _ -> fail (Interpret_error (Runtime_error "There isn't method or consructor"))
;;

let get_params_id (Params x) = List.map (fun (Var_decl (_, id)) -> id) x
let is_init_v x = is_env_const x >>= is_init >>| fun x -> IConst (Init x)
let is_const_v x = is_env_const x >>= is_init >>= is_base
let is_inst_v x = is_env_const x >>= is_init >>= is_class
let is_v x = is_env_const x >>= is_init >>= fun x -> is_base x <|> is_class x
let is_not_null_const_v x = is_const_v x >>= is_not_null
let is_not_null_inst_v x = is_inst_v x >>= is_not_null
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
let get_inst x = is_not_null_inst_v x >>= is_inst
let ret_inst x = return_n (create_val (Instance_v x))
