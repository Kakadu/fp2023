(** Copyright 2023-2024, Ermolovich Anna *)

(** SPDX-License-Identifier: CC0-1.0 *)

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
  val fail : string -> 'a t
end

module Result : sig
  type 'a t = ('a, string) result

  val ( >>= ) : ('a, 'b) result -> ('a -> ('c, 'b) result) -> ('c, 'b) result
  val ( let* ) : ('a, 'b) result -> ('a -> ('c, 'b) result) -> ('c, 'b) result
  val return : 'a -> ('a, 'b) result
  val fail : 'a -> ('b, 'a) result
end

module Eval : functor (M : MONADERROR) -> sig
  type var_ctx =
    { id : Ast.id
    ; value : Ast.value
    }

  type func_ctx =
    { id : Ast.id
    ; args : Ast.id list
    ; stmts : Ast.statement list
    }

  type class_ctx =
    { id : Ast.id
    ; vars : var_ctx list
    ; funcs : func_ctx list
    }

  type global_ctx =
    { id : Ast.id
    ; vars : var_ctx list
    ; sub_ctx : global_ctx list
    ; funcs : func_ctx list
    ; classes : class_ctx list
    ; signal : Ast.signal
    ; last_return : Ast.value
    ; output : string list
    }

  val tmp_ctx : global_ctx
  val tmp_class_ctx : class_ctx
  val main_ctx : global_ctx
  val if_var_exists_in_ctx : Ast.id -> global_ctx -> bool
  val if_func_exists_in_ctx : Ast.id -> global_ctx -> bool
  val if_class_exists_in_ctx : Ast.id -> global_ctx -> bool
  val if_func_exists_in_class_ctx : Ast.id -> class_ctx -> bool
  val add_var_in_ctx : Ast.id -> Ast.value -> global_ctx -> global_ctx
  val change_var_in_ctx : Ast.id -> Ast.value -> global_ctx -> global_ctx

  val add_func_in_ctx
    :  Ast.id
    -> Ast.id list
    -> Ast.statement list
    -> global_ctx
    -> global_ctx

  val get_var_ctx_from_ctx : Ast.id -> global_ctx -> var_ctx
  val get_func_ctx_from_ctx : Ast.id -> global_ctx -> func_ctx
  val get_class_ctx_from_ctx : Ast.id -> global_ctx -> class_ctx
  val get_func_ctx_from_class_ctx : Ast.id -> class_ctx -> func_ctx option
  val multliple_str_unpacker : Ast.value -> string
  val unpack_identifier_in_value : Ast.value -> Ast.id M.t
  val unpack_string_in_identifier : Ast.id -> string M.t
  val unpack_int_in_value : Ast.value -> int M.t
  val unpack_list_in_value : Ast.value -> Ast.expr list M.t
  val if_list : Ast.id -> global_ctx -> bool
  val combine_args_and_params : Ast.id list -> Ast.value list -> var_ctx list
  val get_identifiers_from_args : Ast.expr list -> Ast.id M.t list
  val update_or_add_var : global_ctx -> var_ctx -> global_ctx
  val update_or_add_var_list : global_ctx -> var_ctx list -> global_ctx
  val form_var_ctx : Ast.id -> Ast.value -> var_ctx
  val mmap : ('a -> 'b M.t) -> 'a list -> 'b list M.t
  val fold_left : ('a -> 'b -> 'a M.t) -> 'a -> 'b list -> 'a M.t

  val expr
    :  (global_ctx -> Ast.statement -> global_ctx M.t)
    -> global_ctx
    -> Ast.expr
    -> Ast.value M.t

  val stmt : global_ctx -> Ast.statement -> global_ctx M.t
  val add_output : global_ctx -> Ast.value -> global_ctx
  val init_main_ctx : Ast.statement list -> global_ctx M.t
end

type var_ctx = Eval(Result).var_ctx =
  { id : Ast.id
  ; value : Ast.value
  }

type func_ctx = Eval(Result).func_ctx =
  { id : Ast.id
  ; args : Ast.id list
  ; stmts : Ast.statement list
  }

type class_ctx = Eval(Result).class_ctx =
  { id : Ast.id
  ; vars : var_ctx list
  ; funcs : func_ctx list
  }

type global_ctx = Eval(Result).global_ctx =
  { id : Ast.id
  ; vars : var_ctx list
  ; sub_ctx : global_ctx list
  ; funcs : func_ctx list
  ; classes : class_ctx list
  ; signal : Ast.signal
  ; last_return : Ast.value
  ; output : string list
  }

val tmp_ctx : global_ctx
val tmp_class_ctx : class_ctx
val main_ctx : global_ctx
val if_var_exists_in_ctx : Ast.id -> global_ctx -> bool
val if_func_exists_in_ctx : Ast.id -> global_ctx -> bool
val if_class_exists_in_ctx : Ast.id -> global_ctx -> bool
val if_func_exists_in_class_ctx : Ast.id -> class_ctx -> bool
val add_var_in_ctx : Ast.id -> Ast.value -> global_ctx -> global_ctx
val change_var_in_ctx : Ast.id -> Ast.value -> global_ctx -> global_ctx

val add_func_in_ctx
  :  Ast.id
  -> Ast.id list
  -> Ast.statement list
  -> global_ctx
  -> global_ctx

val get_var_ctx_from_ctx : Ast.id -> global_ctx -> var_ctx
val get_func_ctx_from_ctx : Ast.id -> global_ctx -> func_ctx
val get_class_ctx_from_ctx : Ast.id -> global_ctx -> class_ctx
val get_func_ctx_from_class_ctx : Ast.id -> class_ctx -> func_ctx option
val multliple_str_unpacker : Ast.value -> string
val unpack_identifier_in_value : Ast.value -> Ast.id Result.t
val unpack_string_in_identifier : Ast.id -> string Result.t
val unpack_int_in_value : Ast.value -> int Result.t
val unpack_list_in_value : Ast.value -> Ast.expr list Result.t
val if_list : Ast.id -> global_ctx -> bool
val combine_args_and_params : Ast.id list -> Ast.value list -> var_ctx list
val get_identifiers_from_args : Ast.expr list -> Ast.id Result.t list
val update_or_add_var : global_ctx -> var_ctx -> global_ctx
val update_or_add_var_list : global_ctx -> var_ctx list -> global_ctx
val form_var_ctx : Ast.id -> Ast.value -> var_ctx
val mmap : ('a -> 'b Result.t) -> 'a list -> 'b list Result.t
val fold_left : ('a -> 'b -> 'a Result.t) -> 'a -> 'b list -> 'a Result.t

val expr
  :  (global_ctx -> Ast.statement -> global_ctx Result.t)
  -> global_ctx
  -> Ast.expr
  -> Ast.value Result.t

val stmt : global_ctx -> Ast.statement -> global_ctx Result.t
val add_output : global_ctx -> Ast.value -> global_ctx
val init_main_ctx : Ast.statement list -> global_ctx Result.t
val run_ruby : string -> unit
