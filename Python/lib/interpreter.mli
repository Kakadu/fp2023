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
  val fold_left : ('a -> 'b -> 'a M.t) -> 'a -> 'b list -> 'a M.t
  val map1 : ('a -> 'b M.t) -> 'a list -> 'b list M.t

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

  val local_env : environment
  val temp_class_env : environment
  val global_env : environment
  val var_in_env : Ast.identifier -> environment -> bool
  val change_var : Ast.identifier -> Ast.value -> environment -> environment
  val change_or_add_var : environment -> var_symb -> environment
  val change_or_add_var_list : environment -> var_symb list -> environment
  val get_var : Ast.identifier -> environment -> var_symb
  val func_in_env : Ast.identifier -> environment -> bool
  val class_in_env : Ast.identifier -> environment -> bool
  val change_func : function_symb -> environment -> environment
  val change_or_add_func : function_symb -> environment -> environment
  val change_class : environment -> environment -> environment
  val change_or_add_class : environment -> environment -> environment
  val get_func : Ast.identifier -> environment -> function_symb
  val get_class : Ast.identifier -> environment -> environment
  val print_list : string list -> unit
  val combine_args_and_params : Ast.identifier list -> Ast.value list -> var_symb list
  val get_str_from_identifier : Ast.identifier -> string
  val print_funcs : function_symb list -> unit
  val pack_to_string : Ast.value -> string
  val print_vars : var_symb list -> unit

  type dispatch =
    { i_expr : dispatch -> environment -> Ast.expression -> Ast.value M.t
    ; i_stmt : dispatch -> environment -> Ast.statement -> environment M.t
    }

  val i_exp_or_stmt : dispatch
  val interpret_exp : Ast.expression -> environment -> Ast.value M.t
  val get_env : environment -> Ast.statement list -> environment M.t
  val interpret : Ast.statement list -> environment M.t
end
