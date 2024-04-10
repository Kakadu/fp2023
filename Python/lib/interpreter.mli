(** Copyright 2023-2024, Averin Pavel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

module type MONAD = sig
  type 'a t

  val return : 'a -> 'a t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
end

module type MONADERROR = sig
  type 'a t

  val return : 'a -> 'a t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  val error : string -> 'a t
end

module Result : sig
  type 'a t = ('a, string) result

  val return : 'a -> 'a t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  val error : string -> 'a t
end

module Eval : functor (M : MONADERROR) -> sig
  type var_symb =
    { identifier : Ast.identifier
    ; value : Ast.value
    }

  type function_symb =
    { identifier : Ast.identifier
    ; params : Ast.identifier list
    ; body : Ast.statement list
    }

  type environment =
    { id : Ast.identifier
    ; local_envs : environment list
    ; classes : environment list
    ; functions : function_symb list
    ; flag : Ast.flag
    ; return_v : Ast.value
    ; vars : var_symb list
    ; vals_to_print : Ast.value list
    }

  val get_str_from_identifier : Ast.identifier -> string
  val pack_to_string : Ast.value -> string M.t
  val interpret : Ast.statement list -> environment M.t
end
