(** Copyright 2021-2023, Georgy Sichkar *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

module Type_check_Monad : sig
  val return : 'a -> 'st -> 'st * ('a, 'err) result
  val fail : 'err -> 'st -> 'st * ('a, 'err) result

  val ( >>= )
    :  ('st -> 'st * ('a, 'err) result)
    -> ('a -> 'st -> 'st * ('b, 'err) result)
    -> 'st
    -> 'st * ('b, 'err) result

  val save : 'st -> 'st -> 'st * (unit, 'err) result
  val continue : ('st -> 'st * ('a, 'err) result) -> 'st -> 'st * ('a, 'err) result

  val ( <|> )
    :  ('st -> 'st * ('a, 'err) result)
    -> ('st -> 'st * ('a, 'err) result)
    -> 'st
    -> 'st * ('a, 'err) result

  val lift2
    :  ('a -> 'b -> 'c)
    -> ('st -> 'st * ('a, 'err) result)
    -> ('st -> 'st * ('b, 'err) result)
    -> 'st
    -> 'st * ('c, 'err) result

  val lift3
    :  ('a -> 'b -> 'c -> 'd)
    -> ('st -> 'st * ('a, 'err) result)
    -> ('st -> 'st * ('b, 'err) result)
    -> ('st -> 'st * ('c, 'err) result)
    -> 'st
    -> 'st * ('d, 'err) result

  val lift4
    :  ('a -> 'b -> 'c -> 'd -> 'e)
    -> ('st -> 'st * ('a, 'err) result)
    -> ('st -> 'st * ('b, 'err) result)
    -> ('st -> 'st * ('c, 'err) result)
    -> ('st -> 'st * ('d, 'err) result)
    -> 'st
    -> 'st * ('e, 'err) result

  val ( *> )
    :  ('st -> 'st * ('a, 'err) result)
    -> ('st -> 'st * ('b, 'err) result)
    -> 'st
    -> 'st * ('b, 'err) result

  val ( <* )
    :  ('st -> 'st * ('a, 'err) result)
    -> ('st -> 'st * ('b, 'err) result)
    -> 'st
    -> 'st * ('a, 'err) result

  val ( >>| )
    :  ('st -> 'st * ('a, 'err) result)
    -> ('a -> 'b)
    -> 'st
    -> 'st * ('b, 'err) result

  val map_left
    :  ('a -> 'st -> 'st * ('b, 'err) result)
    -> 'a list
    -> 'st
    -> 'st * ('b list, 'err) result

  val iter_left
    :  ('a -> 'st -> 'st * (unit, 'err) result)
    -> 'a list
    -> 'st
    -> 'st * (unit, 'err) result

  type ctx_env = Env_types.Type_check_env.type_check_ctx
  type 'a t = ctx_env -> ctx_env * ('a, Errors.error) result

  val choice : 'a t list -> 'a t
  val run : 'a t -> ctx_env * ('a, Errors.error) result
  val save_local : Env_types.Type_check_env.t_loc_env -> unit t
  val save_local_el : Ast.ident -> Env_types.Type_check_env.t_env_value -> unit t
  val save_global : Common_types.text -> unit t
  val save_global_el : Common_types.code_ident -> Common_types.code_ctx -> unit t
  val save_scope_tp : Ast.meth_type option -> unit t
  val save_main_ctx : Common_types.code_ident option -> unit t
  val read : ctx_env t
  val read_local_el : Ast.ident -> Env_types.Type_check_env.t_env_value t
  val read_local_el_opt : Ast.ident -> Env_types.Type_check_env.t_env_value option t
  val read_global_el : Common_types.code_ident -> Common_types.code_ctx t
  val read_global_el_opt : Common_types.code_ident -> Common_types.code_ctx option t
  val read_scope_tp : Ast.meth_type option t
  val read_main_ctx : Common_types.code_ident option t
end

module Eval_Monad : sig
  (** The module provides basic SE-monad functionality localized for a specific interpreter. *)

  type ctx_env = Env_types.Eval_env.interpret_ctx
  type ('a, 'b) t = ctx_env -> ctx_env * ('a, 'b, Errors.error) Env_types.Eval_env.eval_t
end

module Eval : sig
  (** The module is an extension of Eval_Monad. *)

  type ctx_env = Eval_Monad.ctx_env
  type ('a, 'b) t = ctx_env -> ctx_env * ('a, 'b, Errors.error) Env_types.Eval_env.eval_t

  val return : ('a, 'b, Errors.error) Env_types.Eval_env.eval_t -> ('a, 'b) t

  (** Return [Eval_res] [sig_] *)
  val return_n : 'a -> ('a, 'b) t

  (** Return [Return] [sig_] *)
  val return_r : 'b option -> ('a, 'b) t

  (** Return [Break] [sig_] *)
  val return_b : 'c -> (_, 'b) t

  (** Return [Exn] [sig_] *)
  val return_e : Common_types.address -> ('a, 'b) t

  val fail : Errors.error -> ('a, 'b) t
  val ( >>= ) : ('a, 'c) t -> ('a -> ('b, 'c) t) -> ('b, 'c) t
  val ( <|> ) : ('b, 'a) t -> ('b, 'a) t -> ('b, 'a) t
  val init_sys_mem : Common_types.sys_mem_el Common_types.Sys_Map.t

  val run
    :  Common_types.code_ctx Common_types.Code_Map.t
    -> ('a, 'b) t
    -> ctx_env * ('a, 'b, Errors.error) Env_types.Eval_env.eval_t

  val save : ctx_env -> (unit, 'b) t
  val read : (ctx_env, 'c) t

  (* ******************** Secondary helper functions ******************** *)

  val ( >>| ) : ('a, 'b) t -> ('a -> 'c) -> ('c, 'b) t
  val ( *> ) : ('a, 'b) t -> ('c, 'b) t -> ('c, 'b) t
  val lift2 : ('a -> 'b -> 'c) -> ('a, 'd) t -> ('b, 'd) t -> ('c, 'd) t
  val fold_left : ('a -> 'b -> ('a, 'c) t) -> 'a -> 'b list -> ('a, 'c) t
  val map_left : ('a -> ('b, 'c) t) -> 'a list -> ('b list, 'c) t
  val map2_left : ('a -> 'b -> ('c, 'd) t) -> 'a list -> 'b list -> ('c list, 'd) t
  val iter_left : ('a -> ('b, 'c) t) -> 'a list -> (unit, 'c) t
  val iter2_left : ('a -> 'b -> ('c, 'd) t) -> 'a list -> 'b list -> (unit, 'd) t

  (* ******************** State interaction functions (extension) ******************** *)

  val read_global : Common_types.code_ident -> (Common_types.code_ctx, 'c) t

  val find_cl_meth
    :  Ast.ident
    -> Common_types.code_ctx
    -> Env_types.Eval_env.instructions option

  val read_global_method
    :  Common_types.code_ident
    -> Ast.ident
    -> (Env_types.Eval_env.instructions option, 'a) t

  val save_mem : Env_types.Eval_env.memory -> (unit, 'c) t
  val read_mem : (Env_types.Eval_env.memory, 'a) t

  val alloc_instance
    :  (Ast.expr -> (Env_types.Eval_env.t_env_value, 'a) t)
    -> Ast.class_decl
    -> (Common_types.address, 'a) t

  val read_inst_cl : Common_types.address -> (Common_types.code_ident, 'a) t

  val read_inst_el
    :  Ast.ident
    -> Common_types.address
    -> (Env_types.Eval_env.t_env_value, 'a) t

  val update_instance_el
    :  Ast.ident
    -> Common_types.address
    -> Env_types.Eval_env.t_env_value
    -> (unit, 'a) t

  val save_smem : Common_types.sys_memory -> (unit, 'c) t
  val read_smem : (Common_types.sys_memory, 'a) t
  val read_local_el : Ast.ident -> (Env_types.Eval_env.t_env_value, 'a) t
  val update_local_el : Ast.ident -> Env_types.Eval_env.t_env_value -> (unit, 'a) t
  val add_local_el : Ast.ident -> Env_types.Eval_env.t_env_value -> (unit, 'a) t
  val read_self_ad : (Common_types.address, 'a) t
  val save_self_ad : Common_types.address -> (unit, 'a) t

  (* ******************** Interpretation helper functions ******************** *)

  (** Saves [t_loc_env], executes a function inside,
      overwrites the [t_loc_env] with the old one

      [local func] *)
  val local : ('a, 'b) t -> ('a, 'b) t

  (** Starts execution in a new scope

      [in_isolation func] *)
  val in_isolation : ('a, 'b) t -> ('a, 'b) t

  (** Allows you to run a method from another instance. *)
  val run_in_another_self
    :  Common_types.address
    -> Env_types.Eval_env.t_env_value Common_types.Ident_Map.t list
    -> ('a, 'b) t
    -> ('a, 'b) t

  val further : ('a, 'b, Errors.error) Env_types.Eval_env.eval_t -> ('a, 'b) t

  (** Allows you to run execution with try catch finally

      [run_tcf try_handler catch_handler finally_handler] *)
  val run_tcf
    :  ('a, 'b) t
    -> (Common_types.address -> ('a, 'b) t)
    -> ('c, 'b) t
    -> ('a, 'b) t

  (** Allows you to start method execution.

      [run_method mt params args self_ad handler]

      [mt] -- Method type. If it is equal to empty,
      the method can end not only with a signal [Return].

      [params] -- list with params.

      [args] -- List of calculated values.

      [self_ad] -- The address of the instance
      from which the method is run.

      [handler] -- Function that evaluates the method body. *)
  val run_method
    :  Ast.meth_type
    -> Ast.ident list
    -> Env_types.Eval_env.t_env_value list
    -> Common_types.address
    -> Env_types.Eval_env.t_env_value Common_types.Ident_Map.t list
    -> ('a, 'c) t
    -> ('c option, 'd) t

  (** Allows you to run loops such as for and while.

      [run_loop condition handler]

      [condition] -- Function for processing the loop termination condition.

      [handler] -- Function that evaluates the loop body. *)
  val run_loop : (bool, 'a) t -> ('b, 'a) t -> (unit, 'a) t
end
