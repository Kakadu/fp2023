(** Copyright 2021-2023, Georgy Sichkar *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

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

type code_ctx =
  | Exception_ctx of class_decl
  | Class_ctx of class_decl

type text = code_ctx CodeMap.t

type t_env_value =
  | Metod_sig of method_sign
  | Constructor_sig of constructor_sign
  | Value_sig of var_type
  | Fild_sig of fild_sign
[@@deriving show { with_path = false }]

type t_loc_env = t_env_value IdentMap.t
type type_check_ctx = text * t_loc_env * meth_type option * code_ident option

type tp_checked = TP_Ok
