(** Copyright 2023-2024, Efremov Alexey *)

(** SPDX-License-Identifier: CC0-1.0 *)

open Ihelp.State
open CommonInterpInstructions

let real_fneg v = Ast.CFloat (Float.neg v)

let ifneg var tp value =
  match tp with
  | Ast.TFloat ->
    let* v = get_const_from_value value >>= is_float in
    write_var var (real_fneg v)
  | Ast.TVector (_, Ast.TFloat) ->
    let* v = vectorize1 real_fneg is_float value in
    write_var var v
  | _ -> fail "Fneg work with floats\n"
;;

let launch_unary_operation : Ast.unary_operation -> (state, instr_launch_res) t =
  fun instr ->
  (match instr with
   | Ast.Fneg (variable, tp, value) -> ifneg variable tp value)
  *> return NoRes
;;
