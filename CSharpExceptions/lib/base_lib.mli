(** Copyright 2021-2023, Georgy Sichkar *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(** This file describes the library that is included in each program and is available in the global scope *)

open Ast
open Common_types
open Monads.Eval

module type UNIT_METHOD = sig
  val tp : meth_type
  val name : ident
  val run : (unit, 'a) t
end

(** A module that allows you to interact with files (open) *)
module File_Info : sig
  val name : code_ident

  module System_mem_internal_functions : sig
    val read_fl_descriptors : (internal_address * file_st Intern_Mem.t, 'a) t
    val save_fl_descriptors : sys_mem_el -> (unit, 'a) t
    val read_out_channel : internal_address -> (out_channel, 'a) t
    val append_out_channel : out_channel -> (internal_address, 'a) t
    val read_out_channel_from_self : ident -> (out_channel, 'a) t
  end

  module Fields : sig
    val internal_address_name : ident
    val path : ident
    val state : ident
  end

  module Methods : sig
    module Open_file : sig
      val name : ident
      val run : string -> (out_channel, 'a) t
    end

    module Write_in_file : UNIT_METHOD
    module Close_file : UNIT_METHOD

    module After_constructor_handler : sig
      val run : address -> (unit, 'a) t
    end
  end

  val run_method : ident -> (unit, 'a) t

  module Declaration : sig
    val decl_ast : class_decl option
  end
end

(** Module describing the base exception class *)
module Exception : sig
  val name : ident

  module Declaration : sig
    val decl_ast : class_decl option
  end
end

val classes_with_system_classes : code_ident list
val get_system_method_opt : code_ident -> ident -> (unit, 'a) t option
val get_system_constr_opt : code_ident -> (address -> (unit, 'a) t) option
val run_sys_constructor_or_normal : (address, 'a) t -> code_ident -> (address, 'a) t

(* Main name *)
val main_method : ident

(** Get Ast of base lib *)
val base_lib_decl : tast option
