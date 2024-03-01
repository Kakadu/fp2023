(** Copyright 2023-2024, Efremov Alexey *)

(** SPDX-License-Identifier: CC0-1.0 *)

open Ihelp.State
open CommonInterpInstructions

let real_shl sz x y = Common.IrInts.create (Int64.shift_left x (Int64.to_int y)) sz

let real_lshr sz x y =
  Common.IrInts.create (Int64.shift_right_logical x (Int64.to_int y)) sz
;;

let real_ashr sz x y = Common.IrInts.create (Int64.shift_right x (Int64.to_int y)) sz
let real_and sz x y = Common.IrInts.create (Int64.logand x y) sz
let real_or sz x y = Common.IrInts.create (Int64.logor x y) sz
let real_xor sz x y = Common.IrInts.create (Int64.logxor x y) sz

let launch_bitwise_operation : Ast.bitwise_binary_operation -> (state, instr_launch_res) t
  =
  fun instr ->
  (match instr with
   | Ast.Shl (var, tp, v1, v2) ->
     write_binop_res tp (check_to_int real_shl) is_int v1 v2 var
   | Ast.Lshr (var, tp, v1, v2) ->
     write_binop_res tp (check_to_int real_lshr) is_int v1 v2 var
   | Ast.Ashr (var, tp, v1, v2) ->
     write_binop_res tp (check_to_int real_ashr) is_int v1 v2 var
   | Ast.And (var, tp, v1, v2) ->
     write_binop_res tp (check_to_int real_and) is_int v1 v2 var
   | Ast.Or (var, tp, v1, v2) ->
     write_binop_res tp (check_to_int real_or) is_int v1 v2 var
   | Ast.Xor (var, tp, v1, v2) ->
     write_binop_res tp (check_to_int real_xor) is_int v1 v2 var)
  *> return NoRes
;;
