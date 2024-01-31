(** Copyright 2021-2023, Georgy Sichkar *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

module Type_check_env = struct
  open Common_types

  type t_env_value =
    | Method_sig of method_sign
    | Constructor_sig of constructor_sign
    | Value_sig of var_type
    | Fild_sig of fild_sign
  [@@deriving show { with_path = false }]

  type t_loc_env = t_env_value Ident_Map.t
  type type_check_ctx = text * t_loc_env * meth_type option * code_ident option
  type tp_checked = TP_Ok
end

module Eval_env = struct
  open Common_types

  type iconst =
    | Null_v
    | String_v of string (** Operations on type are not supported *)
    | Int_v of int (** All arithmetic operations supported *)
    | Char_v of char (* Operations on type are not supported *)
    | Bool_v of bool (* All logical operations supported *)
    | Instance_v of address
      (* Calling by point and [==]; [!=] (equality by reference) are supported *)
  [@@deriving show { with_path = false }, eq]

  let val_to_iconst = function
    | Null -> Null_v
    | VString x -> String_v x
    | VInt x -> Int_v x
    | VChar x -> Char_v x
    | VBool x -> Bool_v x
  ;;

  let to_inst x = Instance_v x

  type instructions =
    | IMethod of method_sign * statement
    | IConstructor of constructor_sign * statement
  [@@deriving show { with_path = false }]

  let to_meth sign body = IMethod (sign, body)
  let to_cons sign body = IConstructor (sign, body)

  type t_env_eval_const =
    | Init of iconst
    | Not_init
  [@@deriving show { with_path = false }]

  let to_init x = Init x
  let to_not_init _ = Not_init

  type t_env_value =
    | IConst of t_env_eval_const
    | ICode of instructions
  [@@deriving show { with_path = false }]

  let to_const x = IConst x
  let to_code x = ICode x
  let create_val x = to_const @@ to_init x
  let create_inst x = to_const @@ to_init @@ to_inst x

  type t_loc_env = address * t_env_value Ident_Map.t list
  type t_global_env = text
  type mem_els = code_ident * (t_env_value * fild_sign) Ident_Map.t
  type memory = address * mem_els Mem_Map.t
  type interpret_ctx = t_global_env * t_loc_env * memory * sys_memory

  type ('b, 'sys_err) sig_ =
    | Return of 'b option
    | Exn of address
    | Break
    | Err of 'sys_err

  type ('a, 'b, 'sys_err) eval_t =
    | Eval_res of 'a
    | Signal of ('b, 'sys_err) sig_

  let to_sig x = Signal x
  let nsig x = Eval_res x
  let rsig x = to_sig (Return x)
  let bsig = to_sig Break
  let esig add = to_sig (Exn add)
  let error err = to_sig (Err err)
end
