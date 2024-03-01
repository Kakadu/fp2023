(** Copyright 2021-2023, Georgy Sichkar *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type code_ident = Code_ident of Ast.ident

val pp_code_ident : Format.formatter -> code_ident -> unit
val show_code_ident : code_ident -> string
val equal_code_ident : code_ident -> code_ident -> bool

module Code_id : sig
  type t = code_ident

  val compare : 'a -> 'a -> int
end

module Code_Map : sig
  type key = code_ident
  type 'a t = 'a Map.Make(Code_id).t

  val empty : 'a t
  val is_empty : 'a t -> bool
  val add : key -> 'a -> 'a t -> 'a t
  val find : key -> 'a t -> 'a
  val find_opt : key -> 'a t -> 'a option
  val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
end

module Ident : sig
  type t = Ast.ident

  val compare : 'a -> 'a -> int
end

module Ident_Map : sig
  type key = Ast.ident
  type 'a t = 'a Map.Make(Ident).t

  val empty : 'a t
  val is_empty : 'a t -> bool
  val add : key -> 'a -> 'a t -> 'a t
  val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val find_opt : key -> 'a t -> 'a option
end

(** Represents the address of an object instance *)
type address = Link of int

val pp_address : Format.formatter -> address -> unit
val show_address : address -> string
val equal_address : address -> address -> bool
val incr_ : address -> address
val ln : int -> address

module Mem_Address : sig
  type t = address

  val compare : 'a -> 'a -> int
end

module Mem_Map : sig
  type key = address
  type 'a t = 'a Map.Make(Mem_Address).t

  val empty : 'a t
  val is_empty : 'a t -> bool
  val mem : key -> 'a t -> bool
  val add : key -> 'a -> 'a t -> 'a t
  val find_opt : key -> 'a t -> 'a option
end

(** Represents a reference to a description of the internal
    fields of a system class instance from [Base_lib]
    This is necessary because some field types are not
    representable in the language, such as [out_channel] *)
type internal_address = ILink of int

val pp_internal_address : Format.formatter -> internal_address -> unit
val show_internal_address : internal_address -> string
val equal_internal_address : internal_address -> internal_address -> bool
val i_incr_ : internal_address -> internal_address
val i_ln : int -> internal_address

module Intern_Mem_Address : sig
  type t = internal_address

  val compare : 'a -> 'a -> int
end

module Intern_Mem : sig
  type key = internal_address
  type 'a t = 'a Map.Make(Intern_Mem_Address).t

  val empty : 'a t
  val is_empty : 'a t -> bool
  val add : key -> 'a -> 'a t -> 'a t
  val find_opt : key -> 'a t -> 'a option
end

(** Type used as a key for Intern_Mems *)
type base_lib_id = Fl_descriptors

module Sys_id : sig
  type t = base_lib_id

  val compare : 'a -> 'a -> int
end

module Sys_Map : sig
  type key = base_lib_id
  type 'a t = 'a Map.Make(Sys_id).t

  val empty : 'a t
  val is_empty : 'a t -> bool
  val add : key -> 'a -> 'a t -> 'a t
  val find_opt : key -> 'a t -> 'a option
end

type file_st = W_File of out_channel

type sys_mem_el =
  | Files of (internal_address * file_st Intern_Mem.t)
  (** Next available internal_address * Map *)

type sys_memory = sys_mem_el Sys_Map.t

type code_ctx =
  | Exception_ctx of Ast.class_decl
  | Class_ctx of Ast.class_decl

val get_class_decl : code_ctx -> Ast.class_decl

type text = code_ctx Code_Map.t
