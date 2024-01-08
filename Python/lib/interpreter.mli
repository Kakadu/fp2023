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
  val local_env : 'a
  val temp_class_env : 'a
  val global_env : 'a
  val var_in_env : 'a -> 'b -> bool
  val change_var : 'a -> 'b -> 'c -> 'd
  val change_or_add_var : 'a -> 'b -> 'c
  val change_or_add_var_list : '_weak3 -> '_weak4 list -> '_weak3
  val get_var : 'a -> 'b -> 'c
  val func_in_env : 'a -> 'b -> bool
  val class_in_env : 'a -> 'b -> bool
  val change_func : 'a -> 'b -> 'c
  val change_or_add_func : 'a -> 'b -> 'c
  val change_class : 'a -> 'b -> 'c
  val change_or_add_class : 'a -> 'b -> 'c
  val get_func : 'a -> 'b -> 'c
  val get_class : 'a -> 'b -> 'c
  val combine_args_and_params : 'a list -> 'b list -> 'c list
  val get_str_from_identifier : 'a -> 'b
  val pack_to_string : 'a -> 'b
  val i_exp_or_stmt : 'a
  val get_env : 'a -> 'b list -> 'a M.t
  val interpret : '_weak1 list -> '_weak2 M.t
end
