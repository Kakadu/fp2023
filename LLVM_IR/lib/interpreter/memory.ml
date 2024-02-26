(** Copyright 2023-2024, Efremov Alexey *)

(** SPDX-License-Identifier: CC0-1.0 *)

open State

let align_addr addr align isUp =
  let res = addr / align * align in
  if res <> addr then if isUp then res + align else res else res
;;

let put_bytes_in_heap : int -> char list -> (state, unit) t =
  fun addr lst ->
  let indxs = List.init (List.length lst) (fun x -> addr + x) in
  let bts = List.to_seq (List.combine indxs lst) in
  write_bytes bts
;;

let put_cnst_in_heap : int -> Ast.const -> (state, unit) t =
  fun addr cnst ->
  let* cnst =
    match cnst with
    | Ast.CPointer (Ast.PointerGlob x) -> read_var x
    | cns -> return cns
  in
  let* lst = Serialisation.serialise_with_state cnst in
  put_bytes_in_heap addr lst
;;

(* let put_cnst_in_heap_align : int -> Ast.const -> int -> (state, unit) t =
   fun addr cnst align -> put_cnst_in_heap (align_addr addr align true) cnst
   ;; *)

let take_cnst_from_heap : int -> Ast.tp -> (state, Ast.const) t =
  fun addr tp ->
  let indxs = List.init (Serialisation.raw_date_len tp) (fun x -> addr + x) in
  let* bts = map_list read_byte indxs in
  return (Serialisation.deserialise tp bts)
;;

let alloc_stack_align : int -> int -> (state, int) t =
  fun len align ->
  let* old_local, old_global, old_heap, old_stack, old_block = read in
  let addr = align_addr (old_stack - len) align false in
  write (old_local, old_global, old_heap, addr, old_block) *> return addr
;;

let free_stack : int -> (state, unit) t =
  fun new_stack ->
  let* old_local, old_global, old_heap, old_stack, old_block = read in
  let new_heap =
    MapInt.filter
      (fun cur_addr _ -> not (cur_addr < new_stack && cur_addr >= old_stack))
      old_heap
  in
  write (old_local, old_global, new_heap, new_stack, old_block)
;;
