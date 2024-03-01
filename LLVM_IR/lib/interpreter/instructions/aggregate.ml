(** Copyright 2023-2024, Efremov Alexey *)

(** SPDX-License-Identifier: CC0-1.0 *)

open Ihelp.State
open CommonInterpInstructions

let rec real_extract var cnst = function
  | [] -> write_var var cnst
  | ind :: tl ->
    let* lst = is_aggregate return cnst in
    if ind >= List.length lst
    then fail "Runtime error: try get element outside of aggregate type"
    else (
      let new_cnst = List.nth lst ind in
      real_extract var new_cnst tl)
;;

let iextractvalue var _agg_tp agg_val ints =
  let* agg_const = get_const_from_value agg_val in
  real_extract var agg_const ints
;;

let aggregate_const_by_const = function
  | Ast.CArr _ -> return (fun x -> Ast.CArr x)
  | Ast.CStruct _ -> return (fun x -> Ast.CStruct x)
  | _ -> fail "Impossible error: this function only for aggregate"
;;

let single_insert new_val lst ind =
  if ind >= List.length lst
  then fail "Runtime error: try get element outside of aggregate type"
  else return (List.mapi (fun c x -> if c = ind then new_val else x) lst)
;;

let rec real_insert var new_val agg_const ints =
  let* lst = is_aggregate return agg_const in
  let* construct = aggregate_const_by_const agg_const in
  match ints with
  | [ x ] ->
    let* finlst = single_insert new_val lst x in
    let old_tp = Ast.const_to_tp (List.nth lst x) in
    let new_tp = Ast.const_to_tp new_val in
    if not (Ast.tp_equal old_tp new_tp)
    then
      fail
        (Printf.sprintf
           "Want to insert value with type %s instead of value with type %s"
           (Ast.show_tp new_tp)
           (Ast.show_tp old_tp))
    else return (construct finlst)
  | ind :: tl ->
    if ind >= List.length lst
    then fail "Runtime error: try get element outside of aggregate type"
    else (
      let new_cnst = List.nth lst ind in
      let* new_val = real_insert var new_val new_cnst tl in
      let* finlst = single_insert new_val lst ind in
      return (construct finlst))
  | _ -> fail "Imposible error: got empty index list during insertvalue instruction"
;;

let iinsertvalue var _agg_tp agg_val _v_tp v ints =
  let* agg_const = get_const_from_value agg_val in
  let* new_val = get_const_from_value v in
  let* new_value = real_insert var new_val agg_const ints in
  write_var var new_value
;;

let launch_aggregate_instruction
  : Ast.aggregate_instruction -> (state, instr_launch_res) t
  =
  fun instr ->
  (match instr with
   | Ast.Extractvalue (var, agg_tp, agg_val, ints) ->
     iextractvalue var agg_tp agg_val ints
   | Ast.Insertvalue (var, agg_tp, agg_val, v_tp, v, ints) ->
     iinsertvalue var agg_tp agg_val v_tp v ints)
  *> return NoRes
;;
