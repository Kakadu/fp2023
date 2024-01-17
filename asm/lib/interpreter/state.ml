(** Copyright 2023-2024, Vadim Yakshigulov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Containers
open Ast

module StateErrorMonad = struct
  type ('st, 'a) t = 'st -> 'st * ('a, string) Result.t

  let return x : _ = fun st -> st, Result.ok x
  let fail err st = st, Result.error err

  let ( >>= ) : 's 'a 'b. ('s, 'a) t -> ('a -> ('s, 'b) t) -> ('s, 'b) t =
    fun x f : _ ->
    fun st ->
    let st, x = x st in
    match x with
    | Result.Ok x -> f x st
    | Result.Error s -> st, Result.Error s
  ;;

  let read : ('st, 'st) t = fun st -> st, Result.Ok st
  let write : 'st -> ('st, unit) t = fun st _old_state -> st, Result.Ok ()
  let run : ('st, 'a) t -> 'st -> 'st * ('a, string) Result.t = fun f st -> f st
  let ( let* ) = ( >>= )
end

open StateErrorMonad

module GlobalState = struct
  type t =
    { registers : int StringMap.t
    ; xmm_registers : (int * int) StringMap.t
    ; stack : int ListStack.t
    ; label_to_jmp : string option
    ; zero_flag : int
    ; carry_flag : int
    ; exit_code : int option
    }

  let initialize_registers_with filter_fn value =
    let add_to_map map (key, value) = StringMap.add key value map in
    Variants_of_register.descriptions
    |> List.filter (fun (name, _) -> filter_fn name)
    |> List.map (fun (name, _) -> name, value)
    |> List.fold_left add_to_map StringMap.empty
  ;;

  let initialize_registers =
    initialize_registers_with (fun name -> not (String.starts_with ~prefix:"Xmm" name)) 0
  ;;

  let initialize_xmm_registers =
    initialize_registers_with (fun name -> String.starts_with ~prefix:"Xmm" name) (0, 0)
  ;;

  let initial_state =
    { registers = initialize_registers
    ; xmm_registers = initialize_xmm_registers
    ; stack = ListStack.empty
    ; label_to_jmp = None
    ; zero_flag = 0
    ; carry_flag = 0
    ; exit_code = None
    }
  ;;

  let update updater =
    let* state = read in
    let* new_state = updater state in
    write new_state
  ;;

  let show state =
    let show_map title show_value map =
      let show_item (key, value) = Printf.sprintf "%s: %s" key (show_value value) in
      Printf.sprintf
        "%s:\n%s"
        title
        (List.map show_item (StringMap.bindings map) |> String.concat "\n")
    in
    let show_list title format_item list =
      Printf.sprintf "%s: %s" title (List.map format_item list |> String.concat ", ")
    in
    let show_registers = show_map "Registers" string_of_int state.registers in
    let show_xmm_registers =
      show_map
        "XMM Registers"
        (fun (a, b) -> Printf.sprintf "(%d, %d)" a b)
        state.xmm_registers
    in
    let show_stack = show_list "Stack" string_of_int state.stack in
    let show_label_to_jump =
      Printf.sprintf "Label to jump: %s" (Option.value ~default:"None" state.label_to_jmp)
    in
    let show_zero_flag = Printf.sprintf "Zero flag: %d" state.zero_flag in
    [ show_registers; show_xmm_registers; show_stack; show_label_to_jump; show_zero_flag ]
    |> String.concat "\n"
  ;;
end

open GlobalState

(** Readers *)
let read_label_to_jmp =
  let* state = read in
  return state.label_to_jmp
;;

let read_zero_flag =
  let* state = read in
  return state.zero_flag
;;

let read_carry_flag =
  let* state = read in
  return state.carry_flag
;;

let peek_reg =
  let* state = read in
  return (ListStack.peek state.stack)
;;

let read_generic_reg reg get_registers =
  let key = Variants_of_register.to_name reg in
  let* state = read in
  match StringMap.find_opt key (get_registers state) with
  | Some x -> return x
  | None -> fail "Reading from uninitialized register"
;;

let read_reg reg = read_generic_reg reg (fun state -> state.registers)
let read_xmm_reg reg = read_generic_reg reg (fun state -> state.xmm_registers)

(** Writers *)
let write_reg reg value =
  let key = Variants_of_register.to_name reg in
  update (fun state ->
    return { state with registers = StringMap.add key value state.registers })
;;

let write_xmm_reg reg value =
  let key = Variants_of_register.to_name reg in
  update (fun state ->
    return { state with xmm_registers = StringMap.add key value state.xmm_registers })
;;

let push_reg reg =
  update (fun state -> return { state with stack = ListStack.push reg state.stack })
;;

let pop_to_reg reg =
  let pop state =
    match ListStack.pop state.stack with
    | Some x -> return { state with stack = x }
    | None -> fail "Pop from empty stack is prohibited"
  in
  let* top = peek_reg in
  match top with
  | Some v ->
    let* _ = update pop in
    write_reg reg v
  | None -> fail "Pop from empty stack is prohibited"
;;

let reset_label_to_jmp = update (fun state -> return { state with label_to_jmp = None })

let write_label_to_jmp label =
  update (fun state -> return { state with label_to_jmp = Some label })
;;

let write_zero_flag value = update (fun state -> return { state with zero_flag = value })

let write_carry_flag value =
  update (fun state -> return { state with carry_flag = value })
;;

let write_exit_code code =
  update (fun state -> return { state with exit_code = Some code })
;;
