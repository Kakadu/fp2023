(** Copyright 2021-2023, Georgy Sichkar *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

module Type_check_env : sig
  (** A module containing types used during type checking. *)

  type t_env_value =
    | Method_sig of Ast.method_sign
    | Constructor_sig of Ast.constructor_sign
    | Value_sig of Ast.var_type
    | Fild_sig of Ast.fild_sign

  val pp_t_env_value : Format.formatter -> t_env_value -> unit
  val show_t_env_value : t_env_value -> string

  type t_loc_env = t_env_value Common_types.Ident_Map.t

  (** Classes * Local space * Signature of the method in which the check occurs * Information about main *)
  type type_check_ctx =
    Common_types.text * t_loc_env * Ast.meth_type option * Common_types.code_ident option

  type tp_checked = TP_Ok
end

module Eval_env : sig
  (** A module containing types used during interpretation. *)

  (** Values assigned during interpretation. *)
  type iconst =
    | Null_v
    | String_v of string
    | Int_v of int
    | Char_v of char
    | Bool_v of bool
    | Instance_v of Common_types.address

  val pp_iconst : Format.formatter -> iconst -> unit
  val show_iconst : iconst -> string
  val equal_iconst : iconst -> iconst -> bool
  val val_to_iconst : Ast.value_ -> iconst
  val to_inst : Common_types.address -> iconst

  type instructions =
    | IMethod of Ast.method_sign * Ast.statement
    | IConstructor of Ast.constructor_sign * Ast.statement

  val pp_instructions : Format.formatter -> instructions -> unit
  val show_instructions : instructions -> string
  val to_meth : Ast.method_sign -> Ast.statement -> instructions
  val to_cons : Ast.constructor_sign -> Ast.statement -> instructions

  type t_env_eval_const =
    | Init of iconst
    | Not_init

  val pp_t_env_eval_const : Format.formatter -> t_env_eval_const -> unit
  val show_t_env_eval_const : t_env_eval_const -> string
  val to_init : iconst -> t_env_eval_const
  val to_not_init : 'a -> t_env_eval_const

  type t_env_value =
    | IConst of t_env_eval_const
    | ICode of instructions

  val pp_t_env_value : Format.formatter -> t_env_value -> unit
  val show_t_env_value : t_env_value -> string
  val to_const : t_env_eval_const -> t_env_value
  val to_code : instructions -> t_env_value
  val create_val : iconst -> t_env_value
  val create_inst : Common_types.address -> t_env_value

  (** Describes the local interpretation scope.

      [address] -- The address of the instance from which the
      interpreted method is called.
      This allows you to access instance internal and public
      fields. The [address] changes when a new method is called.

      [t_env_value Ident_Map.t list] -- Contains a list of maps.

      Each map consists of key-value elements,
      where the value is an [Ast.ident] and the value
      is an element of [iconst].

      By starting to interpret a new scope, a new map
      is created and added to the top of the list.
      After leaving the current scope, the top map
      in the list is removed.

      New [Ast.ident] that appear during interpretation
      are added to the first map from the list of maps.
      Existing [Ast.ident]s are updated by overwriting the map into
      which the [Ast.ident] was initially added. The position of the
      map in the list is preserved.

      Dynamically changes depending on the scope.
      The new scope is any [{}]. *)
  type t_loc_env = Common_types.address * t_env_value Common_types.Ident_Map.t list

  (** Contains an ast in the form of a key-value,
      where the value is a description of the class,
      and the key of [Common_types.code_ctx]).

      Not updated during interpretation phase. *)
  type t_global_env = Common_types.text

  (** Describes the fields of an instance.

      [Common_types.code_ident] -- The name of the class from
      which the instance is created.

      [(t_env_value * Ast.fild_sign) Common_types.Ident_Map.t] --
      A map where the key is the field [Ast.ident], and the value is the
      field value along with its signature.*)
  type mem_els =
    Common_types.code_ident * (t_env_value * Ast.fild_sign) Common_types.Ident_Map.t

  (** Emulates memory within which new instances are created.

      [address] -- Next available address.

      [mem_els Mem_Map.t] -- A map consisting of key-value
      pairs, where the key is the address of an instance,
      and the value is the element of [mem_els]. *)
  type memory = Common_types.address * mem_els Common_types.Mem_Map.t

  (** [interpret_ctx] -- Is the state of a running program. *)
  type interpret_ctx = t_global_env * t_loc_env * memory * Common_types.sys_memory

  (** Runtime signals *)
  type ('b, 'sys_err) sig_ =
    | Return of 'b option
    (** Sent after processing the [return]. Contains the return value. *)
    | Exn of Common_types.address
    (** Sent after processing the [throw]. Contains the address of the instance of the thrown exception. *)
    | Break (** Sent after processing the [break]. *)
    | Err of 'sys_err (** Is a system error. Almost never stops. *)

  type ('a, 'b, 'sys_err) eval_t =
    | Eval_res of 'a
    | Signal of ('b, 'sys_err) sig_

  val to_sig : ('a, 'b) sig_ -> ('c, 'a, 'b) eval_t
  val nsig : 'a -> ('a, 'b, 'c) eval_t
  val rsig : 'a option -> ('b, 'a, 'c) eval_t
  val bsig : ('a, 'b, 'c) eval_t
  val esig : Common_types.address -> ('a, 'b, 'c) eval_t
  val error : 'a -> ('b, 'c, 'a) eval_t
end
