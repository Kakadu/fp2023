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
  (** The module provides basic SE-monad functionality localized for a specific interpreter *)

  type ctx_env = Env_types.Eval_env.interpret_ctx
  type ('a, 'b) t = ctx_env -> ctx_env * ('a, 'b, Errors.error) Env_types.Eval_env.eval_t

  val return : ('a, 'b, Errors.error) Env_types.Eval_env.eval_t -> ('a, 'b) t
  val return_n : 'a -> ('a, 'b) t
  val return_r : 'b option -> ('a, 'b) t
  val return_b : 'c -> ('a, 'b) t
  val return_e : Common_types.address -> ('a, 'b) t
  val fail : Errors.error -> ('a, 'b) t
  val ( >>= ) : ('a, 'c) t -> ('a -> ('b, 'c) t) -> ('b, 'c) t

  (** Allows you to catch signal 'Return' and process it.
      The 'Eval_res' handler function is also passed to the input *)
  val ( |>>= )
    :  ('a, 'c) t
    -> ('c option -> ('b, 'd) t) * ('a -> ('b, 'd) t)
    -> ('b, 'd) t

  val ( @!|>>= )
    :  ('a, 'b) t
    -> (('a, 'b, Errors.error) Env_types.Eval_env.eval_t -> ('d, 'c) t)
    -> ('d, 'c) t

  val ( <|> ) : ('b, 'a) t -> ('b, 'a) t -> ('b, 'a) t
  val init_sys_mem : Common_types.sys_mem_el Common_types.Sys_Map.t

  val run
    :  Common_types.code_ctx Common_types.CodeMap.t
    -> ('a, 'b) t
    -> ctx_env * ('a, 'b, Errors.error) Env_types.Eval_env.eval_t

  val save : ctx_env -> (unit, 'b) t
  val read : (ctx_env, 'c) t
end

module Eval : sig
  type ctx_env = Eval_Monad.ctx_env
  type ('a, 'b) t = ctx_env -> ctx_env * ('a, 'b, Errors.error) Env_types.Eval_env.eval_t

  val return : ('a, 'b, Errors.error) Env_types.Eval_env.eval_t -> ('a, 'b) t
  val return_n : 'a -> ('a, 'b) t
  val return_r : 'b option -> ('a, 'b) t
  val return_b : 'c -> ('a, 'b) t
  val return_e : Common_types.address -> ('a, 'b) t
  val fail : Errors.error -> ('a, 'b) t
  val ( >>= ) : ('a, 'c) t -> ('a -> ('b, 'c) t) -> ('b, 'c) t

  val ( |>>= )
    :  ('a, 'c) t
    -> ('c option -> ('b, 'd) t) * ('a -> ('b, 'd) t)
    -> ('b, 'd) t

  val ( @!|>>= )
    :  ('a, 'b) t
    -> (('a, 'b, Errors.error) Env_types.Eval_env.eval_t -> ('d, 'c) t)
    -> ('d, 'c) t

  val ( <|> ) : ('b, 'a) t -> ('b, 'a) t -> ('b, 'a) t
  val init_sys_mem : Common_types.sys_mem_el Common_types.Sys_Map.t

  val run
    :  Common_types.code_ctx Common_types.CodeMap.t
    -> ('a, 'b) t
    -> ctx_env * ('a, 'b, Errors.error) Env_types.Eval_env.eval_t

  val save : ctx_env -> (unit, 'b) t
  val read : (ctx_env, 'c) t
  val ( >>| ) : ('a, 'b) t -> ('a -> 'c) -> ('c, 'b) t
  val ( *> ) : ('a, 'b) t -> ('c, 'b) t -> ('c, 'b) t
  val lift2 : ('a -> 'b -> 'c) -> ('a, 'd) t -> ('b, 'd) t -> ('c, 'd) t
  val fold_left : ('a -> 'b -> ('a, 'c) t) -> 'a -> 'b list -> ('a, 'c) t
  val map_left : ('a -> ('b, 'c) t) -> 'a list -> ('b list, 'c) t
  val map2_left : ('a -> 'b -> ('c, 'd) t) -> 'a list -> 'b list -> ('c list, 'd) t
  val iter_left : ('a -> ('b, 'c) t) -> 'a list -> (unit, 'c) t
  val iter2_left : ('a -> 'b -> ('c, 'd) t) -> 'a list -> 'b list -> (unit, 'd) t
  val read_global : Common_types.code_ident -> (Common_types.code_ctx, 'c) t

  val find_cl_decl_meth
    :  Ast.ident
    -> Ast.class_decl
    -> Env_types.Eval_env.instructions option

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

  val read_instance : Common_types.address -> (Env_types.Eval_env.mem_els, 'a) t
  val save_instance : Common_types.address -> Env_types.Eval_env.mem_els -> (unit, 'a) t
  val read_inst_cl : Common_types.address -> (Common_types.code_ident, 'a) t

  val read_inst_el
    :  Ast.ident
    -> Common_types.address
    -> (Env_types.Eval_env.t_env_value, 'a) t

  val read_inst_meth
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
  val save_local : Env_types.Eval_env.t_loc_env -> (unit, 'c) t
  val read_local : (Env_types.Eval_env.t_loc_env, 'a) t
  val find_local_ : Ast.ident -> 'a Common_types.IdentMap.t list -> ('a, 'b) t

  val find_local_split_
    :  Ast.ident
    -> 'a Common_types.IdentMap.t list
    -> ( 'a Common_types.IdentMap.t list
         * ('a Common_types.IdentMap.t * 'a)
         * 'a Common_types.IdentMap.t list
       , 'b )
       t

  val read_local_el : Ast.ident -> (Env_types.Eval_env.t_env_value, 'a) t
  val update_local_el : Ast.ident -> Env_types.Eval_env.t_env_value -> (unit, 'a) t
  val add_local_el : Ast.ident -> Env_types.Eval_env.t_env_value -> (unit, 'a) t
  val read_self_ad : (Common_types.address, 'a) t
  val save_self_ad : Common_types.address -> (unit, 'a) t
  val local : ('a, 'b) t -> ('a, 'b) t
  val in_isolation : ('a, 'b) t -> ('a, 'b) t

  val run_in_another_self
    :  Common_types.address
    -> Env_types.Eval_env.t_env_value Common_types.IdentMap.t list
    -> ('a, 'b) t
    -> ('a, 'b) t

  val further : ('a, 'b, Errors.error) Env_types.Eval_env.eval_t -> ('a, 'b) t

  val run_tcf
    :  ('a, 'b) t
    -> (Common_types.address -> ('a, 'b) t)
    -> ('c, 'b) t
    -> ('a, 'b) t

  val run_method
    :  Ast.meth_type
    -> Ast.ident list
    -> Env_types.Eval_env.t_env_value list
    -> Common_types.address
    -> Env_types.Eval_env.t_env_value Common_types.IdentMap.t list
    -> ('a, 'c) t
    -> ('c option, 'd) t

  val run_loop : (bool, 'a) t -> ('b, 'a) t -> (unit, 'a) t
end
