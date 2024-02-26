(** Copyright 2023-2024, Efremov Alexey *)

(** SPDX-License-Identifier: CC0-1.0 *)

include Common.StateMonad

module MapString = struct
  include Map.Make (String)

  let pp pp_v ppf m =
    Format.fprintf ppf "@[[@[";
    iter (fun k v -> Format.fprintf ppf "@[\"%S\": %a@],@\n" k pp_v v) m;
    Format.fprintf ppf "@]]@]"
  ;;
end

module MapInt = struct
  include Map.Make (Int)

  let pp pp_v ppf m =
    Format.fprintf ppf "@[[@[";
    iter (fun k v -> Format.fprintf ppf "@[\"%x\": %a@],@\n" k pp_v v) m;
    Format.fprintf ppf "@]]@]"
  ;;
end

type map_heap = char MapInt.t [@@deriving show { with_path = false }]
type map_var = Ast.const MapString.t [@@deriving show { with_path = false }]
type bytes = (int * char) Seq.t

type state = map_var * map_var * map_heap * int * Ast.variable
(*local glob heap stack_pointer*) [@@deriving show { with_path = false }]

let glob_sect = 1024
let stack_sect = 0xcf000000
let last_block_init = Ast.LocalVar "<None>"

let empty_state : state =
  MapString.empty, MapString.empty, MapInt.empty, stack_sect, last_block_init
;;

let read_var : Ast.variable -> (state, Ast.const) t =
  let find_var name map =
    let value = MapString.find_opt name map in
    match value with
    | Some x -> return x
    | None -> fail (Printf.sprintf "Runtime error: Variable %s is not initialized" name)
  in
  fun variable ->
    let* local, glob, _, _, _ = read in
    match variable with
    | Ast.GlobalVar name -> find_var name glob
    | Ast.LocalVar name -> find_var name local
;;

let write_var : Ast.variable -> Ast.const -> (state, unit) t =
  fun key value ->
  let* old_local, old_global, old_heap, old_stack, old_block = read in
  match key with
  | Ast.GlobalVar name ->
    write (old_local, MapString.add name value old_global, old_heap, old_stack, old_block)
  | Ast.LocalVar name ->
    write (MapString.add name value old_local, old_global, old_heap, old_stack, old_block)
;;

let read_byte : int -> (state, char) t =
  fun number ->
  let* _, _, heap, _, _ = read in
  let bt = MapInt.find_opt number heap in
  match bt with
  | Some x -> return x
  | None ->
    fail (Printf.sprintf "Runtime error: Byte number %x is not initialized" number)
;;

let read_bytes : int -> int -> (state, char list) t =
  fun first len ->
  let read_right_byte num = read_byte (first + num) in
  let buf_list = List.init len (fun x -> x) in
  map_list read_right_byte buf_list
;;

let write_bytes : bytes -> (state, unit) t =
  fun bts ->
  let* old_local, old_global, old_heap, old_stack, old_block = read in
  write (old_local, old_global, MapInt.add_seq bts old_heap, old_stack, old_block)
;;

let read_last_block : (state, Ast.variable) t =
  let* _, _, _, _, block = read in
  return block
;;

let write_last_block : Ast.variable -> (state, unit) t =
  fun block ->
  let* old_local, old_global, old_heap, old_stack, _ = read in
  write (old_local, old_global, old_heap, old_stack, block)
;;

let print_loc_vars : (state, unit) t =
  let* old_local, old_global, old_heap, old_stack, block = read in
  let _ = Printf.printf "%s\n\n\n" (show_map_var old_local) in
  return ()
;;
