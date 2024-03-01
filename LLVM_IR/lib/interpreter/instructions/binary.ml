(** Copyright 2023-2024, Efremov Alexey *)

(** SPDX-License-Identifier: CC0-1.0 *)

open Ihelp.State
open CommonInterpInstructions

let check_div oper tp x y =
  if Int64.equal 0L y then raise Division_by_zero else check_to_int oper tp x y
;;

let real_add sz x y = Common.IrInts.create (Int64.add x y) sz
let real_mul sz x y = Common.IrInts.create (Int64.mul x y) sz
let real_sub sz x y = Common.IrInts.create (Int64.sub x y) sz
let real_udiv sz x y = Common.IrInts.create (Int64.div x y) sz
let real_urem sz x y = Common.IrInts.create (Int64.rem x y) sz

let real_sdiv sz x y =
  let x = Common.IrInts.sget x sz in
  let y = Common.IrInts.sget y sz in
  Common.IrInts.create (Int64.div x y) sz
;;

let real_srem sz x y =
  let x = Common.IrInts.sget x sz in
  let y = Common.IrInts.sget y sz in
  Common.IrInts.create (Int64.rem x y) sz
;;

let real_fadd _tp x y = Ast.CFloat (Float.add x y)
let real_fmul _tp x y = Ast.CFloat (Float.mul x y)
let real_fsub _tp x y = Ast.CFloat (Float.sub x y)
let real_fdiv _tp x y = Ast.CFloat (Float.div x y)
let real_frem _tp x y = Ast.CFloat (Float.rem x y)

let launch_binary_operation : Ast.binary_operation -> (state, instr_launch_res) t =
  fun instr ->
  (match instr with
   | Ast.Add (var, tp, v1, v2) ->
     write_binop_res tp (check_to_int real_add) is_int v1 v2 var
   | Ast.Mul (var, tp, v1, v2) ->
     write_binop_res tp (check_to_int real_mul) is_int v1 v2 var
   | Ast.Sub (var, tp, v1, v2) ->
     write_binop_res tp (check_to_int real_sub) is_int v1 v2 var
   | Ast.Sdiv (var, tp, v1, v2) ->
     write_binop_res tp (check_div real_sdiv) is_int v1 v2 var
   | Ast.Srem (var, tp, v1, v2) ->
     write_binop_res tp (check_div real_srem) is_int v1 v2 var
   | Ast.Udiv (var, tp, v1, v2) ->
     write_binop_res tp (check_to_int real_udiv) is_int v1 v2 var
   | Ast.Urem (var, tp, v1, v2) ->
     write_binop_res tp (check_div real_urem) is_int v1 v2 var
   | Ast.Fadd (var, tp, v1, v2) -> write_binop_res tp real_fadd is_float v1 v2 var
   | Ast.Fmul (var, tp, v1, v2) -> write_binop_res tp real_fmul is_float v1 v2 var
   | Ast.Fdiv (var, tp, v1, v2) -> write_binop_res tp real_fdiv is_float v1 v2 var
   | Ast.Frem (var, tp, v1, v2) -> write_binop_res tp real_frem is_float v1 v2 var
   | Ast.Fsub (var, tp, v1, v2) -> write_binop_res tp real_fsub is_float v1 v2 var)
  *> return NoRes
;;
