(** Copyright 2023-2024, Efremov Alexey *)

(** SPDX-License-Identifier: CC0-1.0 *)

open Ihelp.State
open CommonInterpInstructions

let real_frem _tp x y = Ast.CFloat (Float.rem x y)

let parse_icmp_mode str =
  let open Common.IrInts in
  match str with
  | "ne" -> return (uget, [ -1; 1 ])
  | "eq" -> return (uget, [ 0 ])
  | "ugt" -> return (uget, [ 1 ])
  | "uge" -> return (uget, [ 0; 1 ])
  | "ult" -> return (uget, [ -1 ])
  | "ule" -> return (uget, [ -1; 0 ])
  | "sgt" -> return (sget, [ 1 ])
  | "sge" -> return (sget, [ 0; 1 ])
  | "slt" -> return (sget, [ -1 ])
  | "sle" -> return (sget, [ -1; 0 ])
  | _ -> fail "get unknown icmp predicate"
;;

let real_icmp trans right_vals x y =
  let x = trans x in
  let y = trans y in
  let res = Int64.compare x y in
  match List.find_opt (fun x -> x = res) right_vals with
  | Some _ -> Common.IrInts.create 1L 1
  | None -> Common.IrInts.create 0L 1
;;

let iicmp var com_mode tp v1 v2 =
  let* buf = parse_icmp_mode com_mode in
  let trans, right_vals = buf in
  let* cnst =
    match tp with
    | Ast.TVector (_, Ast.TInteger sz) ->
      vectorize2 (real_icmp (fun x -> trans x sz) right_vals) is_int v1 v2
    | Ast.TInteger sz ->
      let* v1 = get_const_from_value v1 >>= is_int in
      let* v2 = get_const_from_value v2 >>= is_int in
      return (real_icmp (fun x -> trans x sz) right_vals v1 v2)
    | tp -> fail (Printf.sprintf "icmp compare ints, not %s" (Ast.show_tp tp))
  in
  write_var var cnst
;;

let parse_fcmp_mode str =
  let ord x y b = (not (Float.is_nan x)) && (not (Float.is_nan y)) && b in
  let uno x y b = Float.is_nan x || Float.is_nan y || b in
  match str with
  | "false" -> return (ord, [])
  | "oeq" -> return (ord, [ 0 ])
  | "ogt" -> return (ord, [ 1 ])
  | "oge" -> return (ord, [ 0; 1 ])
  | "olt" -> return (ord, [ -1 ])
  | "ole" -> return (ord, [ -1; 0 ])
  | "one" -> return (ord, [ -1; 1 ])
  | "ord" -> return (ord, [ -1; 0; 1 ])
  | "ueq" -> return (ord, [ 0 ])
  | "ugt" -> return (uno, [ 1 ])
  | "uge" -> return (uno, [ 0; 1 ])
  | "ult" -> return (uno, [ -1 ])
  | "ule" -> return (uno, [ -1; 0 ])
  | "une" -> return (uno, [ -1; 1 ])
  | "uno" -> return (uno, [])
  | "true" -> return (uno, [ -1; 0; 1 ])
  | _ -> fail "get unknown fcmp predicate"
;;

let real_fcmp addcmp right_vals x y =
  let res = Float.compare x y in
  let b =
    match List.find_opt (fun x -> x = res) right_vals with
    | Some _ -> true
    | None -> false
  in
  let res = addcmp x y b in
  Common.IrInts.create (Int64.of_int (Bool.to_int res)) 1
;;

let ifcmp var com_mode tp v1 v2 =
  let* buf = parse_fcmp_mode com_mode in
  let addcmp, right_vals = buf in
  let* cnst =
    match tp with
    | Ast.TVector (_, Ast.TFloat) ->
      vectorize2 (real_fcmp addcmp right_vals) is_float v1 v2
    | _ ->
      let* v1 = get_const_from_value v1 >>= is_float in
      let* v2 = get_const_from_value v2 >>= is_float in
      return ((real_fcmp addcmp right_vals) v1 v2)
  in
  write_var var cnst
;;

let iphi var _tp lst =
  let f (v1, v2) =
    let* v2 = get_bb_var_from_label v2 in
    return (v1, v2)
  in
  let* lst = map_list f lst in
  let* last_block = read_last_block in
  let res = List.find_opt (fun (_, label) -> Ast.variable_equal label last_block) lst in
  match res with
  | Some (v, _) -> get_const_from_value v >>= write_var var
  | None -> fail "LLVM do crash-crash (clang-17.0.3 )"
;;

let real_select b (v1, v2) = if b then v1 else v2

let iselect var cond_tp cond res_tp v1 v2 =
  let* v1 = get_const_from_value v1 in
  let* v2 = get_const_from_value v2 in
  let* res =
    match cond_tp with
    | Ast.TVector (n, _) ->
      (match res_tp with
       | Ast.TVector (m, _) when m = n ->
         let* v1 = is_vector return v1 in
         let* v2 = is_vector return v2 in
         let vvs = List.combine v1 v2 in
         let* bls = get_const_from_value cond >>= is_vector is_bool in
         return (Ast.CVector (List.map2 real_select bls vvs))
       | _ -> fail "selected values for vector select must be vectors with same size")
    | _ ->
      let* b = get_const_from_value cond >>= is_bool in
      return (real_select b (v1, v2))
  in
  write_var var res
;;
