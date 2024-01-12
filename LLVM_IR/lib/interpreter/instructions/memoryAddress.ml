(** Copyright 2023-2024, Efremov Alexey *)

(** SPDX-License-Identifier: CC0-1.0 *)

open Ihelp
open State
open CommonInterpInstructions

let ialloca variable tp value align =
  let* n = get_const_from_value value >>= is_int in
  let sz = Int64.to_int n * Serialisation.raw_date_len tp in
  let* addr = Memory.alloc_stack_align sz align in
  write_var variable (to_ptr addr)
;;

let istore _tp value ptr align =
  let* value = get_const_from_value value in
  let* ptr = get_const_from_value ptr >>= is_ptr in
  (* if ptr mod align = 0
     then *)
  Memory.put_cnst_in_heap ptr value
;;

(* else fail "Runtime error: get unaligned pointer for store" *)

let iload var tp ptr align =
  let* ptr = get_const_from_value ptr >>= is_ptr in
  (* if   ptr mod align = 0
     then *)
  let* cnst = Memory.take_cnst_from_heap ptr tp in
  write_var var cnst
;;

(* else fail "Runtime error: get unaligned pointer for load" *)

let rec rec_real_getelementprt tp ptr = function
  | [] -> return ptr
  | x :: tl ->
    let* delta_and_ntp =
      match tp with
      | Ast.TArr (_, el_tp) | Ast.TVector (_, el_tp) ->
        return (Serialisation.raw_date_len el_tp * x, el_tp)
      | Ast.TStruct tp_lst when List.length tp_lst > x ->
        let f (c, acc) mtp =
          if c < x then c + 1, acc + Serialisation.raw_date_len mtp else c, acc
        in
        let _, delt = List.fold_left f (0, 0) tp_lst in
        return (delt, List.nth tp_lst x)
      | _ -> fail "Runtime error: invalid getelementptr indices"
    in
    let delta, ntp = delta_and_ntp in
    rec_real_getelementprt ntp (ptr + delta) tl
;;

let real_getelemetptr tp ptr = function
  | [] -> return (to_ptr ptr)
  | x :: tl ->
    let ptr = ptr + (Serialisation.raw_date_len tp * x) in
    let* fin_ptr = rec_real_getelementprt tp ptr tl in
    return (to_ptr fin_ptr)
;;

let igetelementptr var v_tp ptr_tp ptr ilist =
  let rec transpose = function
    | [] -> []
    | [] :: xss -> transpose xss
    | (x :: xs) :: xss ->
      (x :: List.map List.hd xss) :: transpose (xs :: List.map List.tl xss)
  in
  match ptr_tp with
  | Ast.TVector (_, _) ->
    let f (_, b) =
      get_const_from_value b >>= is_vector is_int >>| List.map Int64.to_int
    in
    let* ptrs = get_const_from_value ptr >>= is_vector is_ptr in
    let ptr_len = List.length ptrs in
    let* vec_ind = map_list f ilist in
    if not (List.for_all (fun l -> ptr_len = List.length l) vec_ind)
    then fail "Vectors have different size in getlementptr"
    else (
      let indss = transpose vec_ind in
      let cnsts = List.map2 (real_getelemetptr v_tp) ptrs indss in
      let* cnst = map_list (fun s -> s >>= return) cnsts >>| fun x -> Ast.CVector x in
      write_var var cnst)
  | _ ->
    let f (_, b) = get_const_from_value b >>= is_int >>| Int64.to_int in
    let* inds = map_list f ilist in
    let* ptr = get_const_from_value ptr >>= is_ptr in
    let* cnst = real_getelemetptr v_tp ptr inds in
    write_var var cnst
;;

let launch_memory_address_operation
  : Ast.memory_address_instruction -> (state, instr_launch_res) t
  =
  fun instr ->
  (match instr with
   | Ast.Alloca (variable, tp, value, align) -> ialloca variable tp value align
   | Ast.Store (tp, value, ptr, align) -> istore tp value ptr align
   | Ast.Load (var, tp, ptr, align) -> iload var tp ptr align
   | Ast.Getelementptr (var, v_tp, ptr_tp, ptr, ilist) ->
     igetelementptr var v_tp ptr_tp ptr ilist)
  *> return NoRes
;;
