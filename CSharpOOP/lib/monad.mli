(** Copyright 2023-2024, Julia Kononova *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type typecheck_error =
  | Occurs_check
  | Mismatch
  | Access_error
  | Impossible_result of string
  | Other of string

val pp_typecheck_error : Format.formatter -> typecheck_error -> unit
val show_typecheck_error : typecheck_error -> string

type interpreter_error =
  | No_variable of string
  | Address_not_found of int
  | Var_declared of string
  | Mismatch
  | Impossible_result of string
  | Other of string

val pp_interpreter_error : Format.formatter -> interpreter_error -> unit
val show_interpreter_error : interpreter_error -> string

type error =
  | Typecheck_error of typecheck_error
  | Interpret_error of interpreter_error

val pp_error : Format.formatter -> error -> unit
val show_error : error -> string

module Monad_SE : sig
  type ('st, 'a) t = 'st -> 'st * ('a, error) result

  val return : 'a -> ('st, 'a) t
  val fail : error -> ('st, 'b) t
  val ( >>= ) : ('st, 'a) t -> ('a -> ('st, 'b) t) -> ('st, 'b) t
  val ( >>| ) : ('st, 'a) t -> ('a -> 'b) -> ('st, 'b) t
  val ( <|> ) : ('st, 'a) t -> ('st, 'a) t -> ('st, 'a) t
  val ( *> ) : ('st, 'a) t -> ('st, 'b) t -> ('st, 'b) t
  val ( <* ) : ('st, 'a) t -> ('st, 'b) t -> ('st, 'a) t
  val lift2 : ('a -> 'b -> 'c) -> ('st, 'a) t -> ('st, 'b) t -> ('st, 'c) t

  val lift3
    :  ('a -> 'b -> 'c -> 'd)
    -> ('st, 'a) t
    -> ('st, 'b) t
    -> ('st, 'c) t
    -> ('st, 'd) t

  val lift4
    :  ('a -> 'b -> 'c -> 'd -> 'e)
    -> ('st, 'a) t
    -> ('st, 'b) t
    -> ('st, 'c) t
    -> ('st, 'd) t
    -> ('st, 'e) t

  val read : ('st, 'st) t
  val write : 'st -> ('st, unit) t
  val run : ('st, 'a) t -> 'st -> 'st * ('a, error) result
  val map : ('a -> ('st, 'b) t) -> 'a list -> ('st, 'b list) t
  val fold_left : ('a -> 'b -> ('st, 'a) t) -> 'a -> 'b list -> ('st, 'a) t
  val iter : ('a -> ('st, unit) t) -> 'a list -> ('st, unit) t
end

module Monad_TypeCheck : sig
  val return : 'a -> 'st -> 'st * ('a, error) result
  val fail : error -> 'st -> 'st * ('b, error) result

  val ( >>= )
    :  ('st -> 'st * ('a, error) result)
    -> ('a -> 'st -> 'st * ('b, error) result)
    -> 'st
    -> 'st * ('b, error) result

  val ( >>| )
    :  ('st -> 'st * ('a, error) result)
    -> ('a -> 'b)
    -> 'st
    -> 'st * ('b, error) result

  val ( <|> )
    :  ('st -> 'st * ('a, error) result)
    -> ('st -> 'st * ('a, error) result)
    -> 'st
    -> 'st * ('a, error) result

  val ( *> )
    :  ('st -> 'st * ('a, error) result)
    -> ('st -> 'st * ('b, error) result)
    -> 'st
    -> 'st * ('b, error) result

  val ( <* )
    :  ('st -> 'st * ('a, error) result)
    -> ('st -> 'st * ('b, error) result)
    -> 'st
    -> 'st * ('a, error) result

  val lift3
    :  ('a -> 'b -> 'c -> 'd)
    -> ('st -> 'st * ('a, error) result)
    -> ('st -> 'st * ('b, error) result)
    -> ('st -> 'st * ('c, error) result)
    -> 'st
    -> 'st * ('d, error) result

  val run : ('st -> 'st * ('a, error) result) -> 'st -> 'st * ('a, error) result

  val map
    :  ('a -> 'st -> 'st * ('b, error) result)
    -> 'a list
    -> 'st
    -> 'st * ('b list, error) result

  val fold_left
    :  ('a -> 'b -> 'st -> 'st * ('a, error) result)
    -> 'a
    -> 'b list
    -> 'st
    -> 'st * ('a, error) result

  val iter
    :  ('a -> 'st -> 'st * (unit, error) result)
    -> 'a list
    -> 'st
    -> 'st * (unit, error) result

  type 'a t = (State_type.TypeCheck.st_type_check, 'a) Monad_SE.t

  val read_local : State_type.obj_content State_type.MapName.t t

  val read_local_el_opt
    :  Ast.name
    -> State_type.TypeCheck.st_type_check
    -> State_type.TypeCheck.st_type_check * (State_type.obj_content option, error) result

  val read_local_el
    :  Ast.name
    -> State_type.TypeCheck.st_type_check
    -> State_type.TypeCheck.st_type_check * (State_type.obj_content, error) result

  val read_global_el_opt
    :  Ast.name
    -> State_type.TypeCheck.st_type_check
    -> State_type.TypeCheck.st_type_check * (State_type.context option, error) result

  val read_global_el
    :  Ast.name
    -> State_type.TypeCheck.st_type_check
    -> State_type.TypeCheck.st_type_check * (State_type.context, error) result

  val get_cur_class_name : Ast.name t
  val read_meth_type : Ast.meth_type option t
  val read_main_class : Ast.name option t

  val write_local
    :  'a
    -> 'b * 'a * 'c * 'd * 'e
    -> ('b * 'a * 'c * 'd * 'e) * (unit, error) result

  val write_local_el
    :  Ast.name
    -> State_type.obj_content
    -> State_type.TypeCheck.st_type_check
    -> State_type.TypeCheck.st_type_check * (unit, error) result

  val write_meth_type
    :  'a
    -> 'b * 'c * 'd * 'a option * 'e
    -> ('b * 'c * 'd * 'a option * 'e) * (unit, error) result

  val write_main_class
    :  'a
    -> 'b * 'c * 'd * 'e * 'a
    -> ('b * 'c * 'd * 'e * 'a) * (unit, error) result

  val write_global_el
    :  Ast.name
    -> State_type.context
    -> State_type.TypeCheck.st_type_check
    -> State_type.TypeCheck.st_type_check * (unit, error) result

  val write_cur_class_name
    :  'a
    -> 'b * 'c * 'a option * 'd * 'e
    -> ('b * 'c * 'a option * 'd * 'e) * (unit, error) result
end

module Monad_Interpreter : sig
  type ('weak2431, 'r) runtime_signal =
    | Pipe of 'weak2431
    | Return of 'r

  type ('a, 'r, 'e) result =
    | Signal of ('a, 'r) runtime_signal
    | IError of 'e

  type st = State_type.St_Interpreter.st_interpreter
  type ('a, 'r) t = st -> st * ('a, 'r, error) result

  val return : 'a -> ('a, 'r) t
  val fun_return : 'r -> ('a, 'r) t
  val fail : error -> ('a, 'r) t
  val ( >>= ) : ('a, 'r) t -> ('a -> ('b, 'r) t) -> ('b, 'r) t
  val ( >>| ) : ('a, 'r) t -> ('a -> 'b) -> ('b, 'r) t
  val ( <|> ) : ('a, 'r) t -> ('a, 'r) t -> ('a, 'r) t
  val ( *> ) : ('a, 'r) t -> ('b, 'r) t -> ('b, 'r) t
  val ( <* ) : ('a, 'r) t -> ('b, 'r) t -> ('a, 'r) t
  val run : ('a, 'r) t -> st * ('a, 'r, error) result
  val lift2 : ('a -> 'b -> 'c) -> ('a, 'd) t -> ('b, 'd) t -> ('c, 'd) t
  val map : ('a -> ('b, 'c) t) -> 'a list -> ('b list, 'c) t
  val iter : ('a -> ('b, 'c) t) -> 'a list -> (unit, 'c) t
  val fold_left : ('a -> 'b -> ('a, 'c) t) -> 'a -> 'b list -> ('a, 'c) t
  val pipe_name_with_fail : Ast.name -> 'a option -> ('a, 'b) t
  val read_local : (State_type.St_Interpreter.local_env, 'a) t
  val read_local_el : Ast.name -> (State_type.St_Interpreter.local_el, 'a) t
  val read_global_el : Ast.name -> (State_type.St_Interpreter.context, 'a) t
  val read_local_adr : (State_type.adr, 'a) t
  val read_memory : (State_type.St_Interpreter.memory, 'a) t
  val read_memory_obj : State_type.adr -> (State_type.St_Interpreter.obj, 'a) t
  val write_local : State_type.St_Interpreter.local_env -> (unit, 'a) t
  val write_new_local_el : Ast.name -> State_type.St_Interpreter.local_el -> (unit, 'a) t
  val write_global_el : Ast.name -> State_type.St_Interpreter.context -> (unit, 'a) t
  val write_cur_adr : State_type.adr -> (State_type.adr, 'a) t
  val write_memory_obj : State_type.adr -> State_type.St_Interpreter.obj -> (unit, 'a) t
  val find_local_el : Ast.name -> (State_type.St_Interpreter.local_el, 'a) t
  val get_new_idx : (State_type.St_Interpreter.idx, 'a) t

  val run_method
    :  State_type.St_Interpreter.local_el list
    -> Ast.params
    -> State_type.adr
    -> Ast.meth_type
    -> ('a, 'b option) t
    -> ('b option, 'c) t

  val allocate_object
    :  Ast.constructor
    -> State_type.St_Interpreter.local_el list
    -> (Ast.expr -> (State_type.St_Interpreter.local_el, 'a) t)
    -> (Ast.statement -> ('b, 'c option) t)
    -> (State_type.adr, 'a) t
end
