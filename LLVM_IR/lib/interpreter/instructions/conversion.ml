(** Copyright 2023-2024, Efremov Alexey *)

(** SPDX-License-Identifier: CC0-1.0 *)

open Ihelp.State
open CommonInterpInstructions

let write_conversion_res operat is_elem s_tp_decon d_tp_decon s_tp s_val d_tp var =
  let* res =
    match s_tp, d_tp with
    | Ast.TVector (n1, s_el_tp), Ast.TVector (n2, d_el_tp) ->
      if n1 <> n2
      then fail "Source vector and destination have different size"
      else vectorize1 (operat (s_tp_decon s_el_tp) (d_tp_decon d_el_tp)) is_elem s_val
    | _ ->
      let* v = get_const_from_value s_val >>= is_elem in
      return ((operat (s_tp_decon s_tp) (d_tp_decon d_tp)) v)
  in
  write_var var res
;;

let int_d = function
  | Ast.TInteger sz -> sz
  | _ ->
    let _ = Printf.printf "Impossible error: get wrong type at conversion operation" in
    -1
;;

let empty_d _tp = ()
let real_trunc _ssz dsz x = Common.IrInts.create x dsz
let real_zext = real_trunc
let real_sext ssz dsz x = Common.IrInts.create (Common.IrInts.sget x ssz) dsz
let real_fptoui _ dsz x = Common.IrInts.create (Int64.of_float x) dsz
let real_fptosi = real_fptoui
let real_uitofp _ _ x = Ast.CFloat (Int64.to_float x)
let real_sitofp ssz _ x = Ast.CFloat (Int64.to_float (Common.IrInts.sget x ssz))
let real_ptrtoint _ dsz x = Common.IrInts.create (Int64.of_int x) dsz
let real_inttoptr _ _ x = to_ptr (Int64.to_int x)

let launch_conversion_instruction
  : Ast.conversion_instruction -> (state, instr_launch_res) t
  =
  fun instr ->
  (match instr with
   | Ast.TruncTo (var, s_tp, s_val, d_tp) ->
     write_conversion_res real_trunc is_int int_d int_d s_tp s_val d_tp var
   | Ast.ZextTo (var, s_tp, s_val, d_tp) ->
     write_conversion_res real_zext is_int int_d int_d s_tp s_val d_tp var
   | Ast.SextTo (var, s_tp, s_val, d_tp) ->
     write_conversion_res real_sext is_int int_d int_d s_tp s_val d_tp var
   | Ast.FptosiTo (var, s_tp, s_val, d_tp) ->
     write_conversion_res real_fptosi is_float empty_d int_d s_tp s_val d_tp var
   | Ast.FptouiTo (var, s_tp, s_val, d_tp) ->
     write_conversion_res real_fptoui is_float empty_d int_d s_tp s_val d_tp var
   | Ast.UitofpTo (var, s_tp, s_val, d_tp) ->
     write_conversion_res real_uitofp is_int int_d empty_d s_tp s_val d_tp var
   | Ast.SitofpTo (var, s_tp, s_val, d_tp) ->
     write_conversion_res real_sitofp is_int int_d empty_d s_tp s_val d_tp var
   | Ast.PrttointTo (var, s_tp, s_val, d_tp) ->
     write_conversion_res real_ptrtoint is_ptr empty_d int_d s_tp s_val d_tp var
   | Ast.InttoprtTo (var, s_tp, s_val, d_tp) ->
     write_conversion_res real_inttoptr is_int int_d empty_d s_tp s_val d_tp var)
  *> return NoRes
;;
