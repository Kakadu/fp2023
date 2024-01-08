(** Copyright 2021-2023, Georgy Sichkar *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

module Common_env = struct
  type code_ident = Code_ident of ident

  module Code_id = struct
    type t = code_ident

    let compare = compare
  end

  module CodeMap = Stdlib.Map.Make (Code_id)

  module Ident = struct
    type t = ident

    let compare = compare
  end

  module IdentMap = Stdlib.Map.Make (Ident)

  type address = Link of int [@@deriving show { with_path = false }]

  module MemAddress = struct
    type t = address

    let compare = compare
  end

  module MemMap = Stdlib.Map.Make (MemAddress)

  type code_ctx =
    | Exception_ctx of class_decl
    | Class_ctx of class_decl

  type text = code_ctx CodeMap.t
end

module Type_check_env = struct
  open Common_env

  type t_env_value =
    | Metod_sig of method_sign
    | Constructor_sig of constructor_sign
    | Value_sig of var_type
    | Fild_sig of fild_sign
  [@@deriving show { with_path = false }]

  type t_loc_env = t_env_value IdentMap.t
  type type_check_ctx = text * t_loc_env * meth_type option * code_ident option
  type tp_checked = TP_Ok
end

module Eval_env = struct
  open Common_env

  type t_env_value =
    | ILink_access of address
    | IBase_value of value_
    | IMethod of method_sign*statement
    | IConstructor of method_sign*statement

  type t_env_value_opt = t_env_value option
  type t_loc_env = address * t_env_value_opt IdentMap.t

  (*  *)
  type t_global_env = text
  type stack_trace = method_sign list

  (*  *)
  type mem_el = code_ident * (t_env_value_opt * fild_sign) list
  type memory = address * mem_el MemMap.t
  type interpret_ctx = t_global_env * t_loc_env * memory * stack_trace

  type 'a tp_return =
    | Info of 'a
    | Return of 'a option
    | Break

  let to_info x = Info x
  let to_return x = Return x

end
