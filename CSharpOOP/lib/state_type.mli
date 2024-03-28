(** Copyright 2023-2024, Julia Kononova *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

module Name : sig
  type t = Ast.name

  val compare : 'a -> 'a -> int
end

module MapName : sig
  include Map.S with type key = Ast.name
end

type adr = Adr of int

val pp_adr : Format.formatter -> adr -> unit
val show_adr : adr -> string

module Adr : sig
  type t = adr

  val compare : 'a -> 'a -> int
end

module MapAdr : sig
  include Map.S with type key = adr
end

type obj_content =
  | VType of Ast.var_type
  | Method of Ast.methods
  | Field of Ast.field
  | Constructor of Ast.constructor

val pp_obj_content : Format.formatter -> obj_content -> unit
val show_obj_content : obj_content -> string
val equal_obj_content : obj_content -> obj_content -> bool

type context =
  | TC_Class of Ast.obj_class
  | TC_Interface of Ast.interface * obj_content MapName.t

module TypeCheck : sig
  type global_env = context MapName.t
  type local_env = obj_content MapName.t
  type cur_class_name = Ast.name
  type class_with_main_method = cur_class_name

  type st_type_check =
    global_env
    * local_env
    * cur_class_name option
    * Ast.meth_type option
    * cur_class_name option
end

module St_Interpreter : sig
  type context =
    | Int_Class of Ast.obj_class
    | Int_Interface of Ast.interface

  val pp_context : Format.formatter -> context -> unit
  val show_context : context -> string

  type idx = Idx of int

  val pp_idx : Format.formatter -> idx -> unit
  val show_idx : idx -> string

  type code =
    | Constructor of Ast.constructor * Ast.statement
    | Method of Ast.methods * Ast.statement

  val pp_code : Format.formatter -> code -> unit
  val show_code : code -> string

  type el =
    | IClass of adr
    | IValue of Ast.value

  val pp_el : Format.formatter -> el -> unit
  val show_el : el -> string

  type vl =
    | Init of el
    | Not_init

  val pp_vl : Format.formatter -> vl -> unit
  val show_vl : vl -> string

  type local_el =
    | Code of code
    | Value of vl * idx option

  type local_env = idx * local_el MapName.t

  type obj =
    { mems : (Ast.field * vl) MapName.t
    ; cl_name : Ast.name
    ; p_adr : adr option
    ; inh_adr : adr option
    }

  type memory = adr * obj MapAdr.t
  type local_adr = adr
  type global_env = context MapName.t
  type st_interpreter = global_env * local_env * local_adr * memory
end
