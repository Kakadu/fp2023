(** Copyright 2023-2024, Efremov Alexey *)

(** SPDX-License-Identifier: CC0-1.0 *)

open Ihelp.State
open CommonInterpInstructions

let real_extract var vec_val ind = write_var var (List.nth vec_val ind)

let get_data vec_val ival =
  let* vec_val = get_const_from_value vec_val >>= is_vector return in
  let n = List.length vec_val in
  let* ind = get_const_from_value ival >>= is_int in
  let ind = Int64.to_int ind in
  if ind >= n
  then fail "Runtime error: try get element outside of vector"
  else return (vec_val, ind)
;;

let iextractelement var _vec_tp vec_val _itp ival =
  let* buf = get_data vec_val ival in
  let vec_val, ind = buf in
  real_extract var vec_val ind
;;

let real_insert var vec_val ind new_val =
  let new_lst = List.mapi (fun c x -> if c = ind then new_val else x) vec_val in
  write_var var (Ast.CVector new_lst)
;;

let iinsertelement var _vec_tp vec_val new_val _itp ival =
  let* buf = get_data vec_val ival in
  let* new_val = get_const_from_value new_val in
  let vec_val, ind = buf in
  real_insert var vec_val ind new_val
;;

let real_shuffle bvec ivec var =
  let len = List.length bvec in
  let f n =
    let n = Int64.to_int n in
    if n >= len
    then fail "Runtime error: try get element outside of vector"
    else return (List.nth bvec n)
  in
  let* new_vec = map_list f ivec in
  write_var var (Ast.CVector new_vec)
;;

let ishufflevector var _vec_tp vec1 vec2 _m vec3 =
  let* vec1 = get_const_from_value vec1 >>= is_vector return in
  let* vec2 = get_const_from_value vec2 >>= is_vector return in
  let* vec3 = is_vector is_int vec3 in
  let bvec = List.append vec1 vec2 in
  real_shuffle bvec vec3 var
;;

let launch_vector_instruction : Ast.vector_instruction -> (state, instr_launch_res) t =
  fun instr ->
  (match instr with
   | Ast.Extractelement (var, vec_tp, vec_val, itp, ival) ->
     iextractelement var vec_tp vec_val itp ival
   | Ast.Insertelement (var, vec_tp, vec_val, new_val, itp, ival) ->
     iinsertelement var vec_tp vec_val new_val itp ival
   | Ast.Shufflevector (var, vec_tp, vec1, vec2, m, vec3) ->
     ishufflevector var vec_tp vec1 vec2 m vec3)
  *> return NoRes
;;
