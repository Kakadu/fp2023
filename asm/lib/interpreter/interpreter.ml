(** Copyright 2023-2024, Vadim Yakshigulov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open State
open Ast
open StateErrorMonad
open GlobalState

let ignore = return ()

let eval_syscall =
  let* rax = read_reg rax in
  let* rdi = read_reg rdi in
  let* rsi = read_reg rsi in
  let* rdx = read_reg rdx in
  match rax, rdi, rsi, rdx with
  | 60, errcode, _, _ -> write_exit_code errcode
  | _ -> fail "Unsupported syscall"
;;

let eval_directive = function
  | Section _ -> ignore
  | Global label -> write_label_to_jmp label
;;

let eval_statement = function
  | Instruction (Mov, args) ->
    (match args with
     | Reg_64_imm_a (r, Imm_int i) -> write_reg r i)
  | Instruction (Push, args) ->
    (match args with
     | Reg_64 r ->
       let* v = read_reg r in
       push_reg v)
  | Instruction (Add, args) ->
    (match args with
     | Reg_64_reg_64 (r1, r2) ->
       let* v1 = read_reg r1 in
       let* v2 = read_reg r2 in
       write_reg r1 (v1 + v2))
  | Instruction (Xor, args) ->
    (match args with
     | Reg_64_reg_64 (r1, r2) ->
       let* v1 = read_reg r1 in
       let* v2 = read_reg r2 in
       write_reg r1 (v1 lxor v2))
  | Instruction (Syscall, _) -> eval_syscall
  | Instruction (Pop, args) ->
    (match args with
     | Reg_64 r -> pop_to_reg r)
  | Instruction (Jmp, Label label) -> write_label_to_jmp label
  | Instruction (Sub, args) ->
    (match args with
     | Reg_64_imm_a (r, Imm_int i) ->
       let* v = read_reg r in
       write_reg r (v - i))
  | Instruction (Cmp, args) ->
    (match args with
     | Reg_64_imm_a (r, Imm_int i) ->
       let* v = read_reg r in
       let zero_flag = if v = i then 1 else 0 in
       let* _ = write_zero_flag zero_flag in
       let carry_flag = if v < i then 1 else 0 in
       write_carry_flag carry_flag)
  | Instruction (Je, Label label) ->
    let* zero_flag = read_zero_flag in
    if zero_flag = 1 then write_label_to_jmp label else ignore
  | Instruction (Jle, Label label) ->
    let* zero_flag = read_zero_flag in
    let* carry_flag = read_carry_flag in
    (match zero_flag, carry_flag with
     | 1, 1 | 0, 1 | 1, 0 -> write_label_to_jmp label
     | _, _ -> ignore)
  | Instruction (Jne, Label label) ->
    let* zero_flag = read_zero_flag in
    if zero_flag = 0 then write_label_to_jmp label else ignore
  | Instruction (Movq, args) ->
    (match args with
     | Reg_64_reg_128 (r1, r2) ->
       let* _high, low = read_xmm_reg r2 in
       write_reg r1 low
     | Reg_128_reg_64 (r1, r2) ->
       let* v2 = read_reg r2 in
       write_xmm_reg r1 (0, v2))
  | Instruction (Punpckhqdq, args) ->
    (match args with
     | Reg_128_reg_128 (r1, r2) ->
       let* high1, _low1 = read_xmm_reg r1 in
       let* high2, _low2 = read_xmm_reg r2 in
       write_xmm_reg r1 (high1, high2))
  | Instruction (Addpd, args) ->
    (match args with
     | Reg_128_reg_128 (r1, r2) ->
       let* high1, low1 = read_xmm_reg r1 in
       let* high2, low2 = read_xmm_reg r2 in
       write_xmm_reg r1 (high1 + high2, low1 + low2))
  | Instruction (Mulpd, args) ->
    (match args with
     | Reg_128_reg_128 (r1, r2) ->
       let* high1, low1 = read_xmm_reg r1 in
       let* high2, low2 = read_xmm_reg r2 in
       write_xmm_reg r1 (high1 * high2, low1 * low2))
  | Instruction (Pinsrq, args) ->
    (match args with
     | Reg_128_reg_64_imm_a (r1, r2, Imm_int i) when i land 1 = 0 ->
       let* high, _low = read_xmm_reg r1 in
       let* v2 = read_reg r2 in
       write_xmm_reg r1 (high, v2)
     | Reg_128_reg_64_imm_a (r1, r2, Imm_int i) when i land 1 = 1 ->
       let* _high, low = read_xmm_reg r1 in
       let* v2 = read_reg r2 in
       write_xmm_reg r1 (v2, low)
     | _ -> fail "Unsupported instruction operands")
  | Instruction (Movapd, args) ->
    (match args with
     | Reg_128_reg_128 (r1, r2) ->
       let* high1, low1 = read_xmm_reg r2 in
       write_xmm_reg r1 (high1, low1))
  | Instruction (Haddpd, args) ->
    (match args with
     | Reg_128_reg_128 (r1, r2) ->
       let* high1, low1 = read_xmm_reg r1 in
       let* high2, low2 = read_xmm_reg r2 in
       write_xmm_reg r1 (high2 + low2, high1 + low1))
  | Directive _ -> ignore
  | Label_decl _ -> ignore
;;

let find_and_set_starting_label ast =
  let label_from_global =
    List.find_map
      (function
        | Directive (Global label) -> Some label
        | _ -> None)
      ast
  in
  GlobalState.update (fun state -> return { state with label_to_jmp = label_from_global })
;;

let get_next_statement_index ast current_index =
  let find_index cond lst =
    let rec helper index = function
      | [] -> None
      | x :: xs -> if cond x then Some index else helper (index + 1) xs
    in
    helper 0 lst
  in
  let handle_label_jump label =
    match find_index (fun x -> x = Label_decl label) ast with
    | Some index ->
      let* _ = reset_label_to_jmp in
      return (index + 1)
    | None -> fail "Can't jump to undefined label"
  in
  let* maybe_label = read_label_to_jmp in
  match maybe_label with
  | Some label_to_jmp -> handle_label_jump label_to_jmp
  | None -> return (current_index + 1)
;;

let eval_ast ast =
  let* _ = find_and_set_starting_label ast in
  (* Determines if the program should exit based on the exit code. *)
  let should_exit_program =
    let* state = read in
    match state.exit_code with
    | Some _ -> return true
    | None -> return false
  in
  (* Checks if there is no more statements to execute. *)
  let no_statement_to_execute index = index >= List.length ast in
  (* Handles the situation when there are no more statements to execute. *)
  let handle_end_of_input index =
    if no_statement_to_execute index then write_exit_code 0 else ignore
  in
  (* Recursively executes statements starting from the given index. *)
  let rec execute_statements current_index =
    let* next_statement_index = get_next_statement_index ast current_index in
    let* _ = handle_end_of_input next_statement_index in
    let* should_exit = should_exit_program in
    if should_exit
    then return ()
    else (
      let statement = List.nth ast next_statement_index in
      let* _ = eval_statement statement in
      execute_statements next_statement_index)
  in
  execute_statements (-1)
;;

module State = State
