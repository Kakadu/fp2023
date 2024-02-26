(** Copyright 2023-2024, Efremov Alexey *)

(** SPDX-License-Identifier: CC0-1.0 *)

type instr_launch_res =
  | Ret of Ast.const
  | Jmp of Ast.variable
  | NoRes

val to_ptr : int -> Ast.const
val err_type : Ast.tp -> Ast.tp -> 'a -> 'a * ('b, string) result
val err_type_c : Ast.tp -> Ast.const -> 'a -> 'a * ('b, string) result
val get_const_from_value : Ast.value -> (Ihelp.State.state, Ast.const) Ihelp.State.t
val is_block : Ast.const -> 'a -> 'a * (Ast.basic_block, string) result
val is_bool : Ast.const -> 'a -> 'a * (bool, string) result
val is_int : Ast.const -> 'a -> 'a * (int64, string) result
val is_float : Ast.const -> 'a -> 'a * (float, string) result
val is_ptr : Ast.const -> (Ihelp.State.state, int) Ihelp.State.t

val is_vector
  :  (Ast.const -> ('a, 'b) Ihelp.State.t)
  -> Ast.const
  -> ('a, 'b list) Ihelp.State.t

val is_aggregate
  :  (Ast.const -> ('a, 'b) Ihelp.State.t)
  -> Ast.const
  -> ('a, 'b list) Ihelp.State.t

val vectorize1
  :  ('a -> Ast.const)
  -> (Ast.const -> (Ihelp.State.state, 'a) Ihelp.State.t)
  -> Ast.value
  -> (Ihelp.State.state, Ast.const) Ihelp.State.t

val vectorize2
  :  ('a -> 'a -> Ast.const)
  -> (Ast.const -> (Ihelp.State.state, 'a) Ihelp.State.t)
  -> Ast.value
  -> Ast.value
  -> (Ihelp.State.state, Ast.const) Ihelp.State.t

val write_binop_res
  :  Ast.tp
  -> (Ast.tp -> 'a -> 'a -> Ast.const)
  -> (Ast.const -> (Ihelp.State.state, 'a) Ihelp.State.t)
  -> Ast.value
  -> Ast.value
  -> Ast.variable
  -> (Ihelp.State.state, unit) Ihelp.State.t

val check_to_int : (int -> 'a -> 'b -> Ast.const) -> Ast.tp -> 'a -> 'b -> Ast.const
val get_bb_var_from_label : Ast.value -> 'a -> 'a * (Ast.variable, string) result
