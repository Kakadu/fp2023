(** Copyright 2023-2024, Efremov Alexey *)

(** SPDX-License-Identifier: CC0-1.0 *)

open Ihelp.State
open CommonInterpInstructions

let real_ret res = return (Ret res)

let iret _tp value =
  let* res = get_const_from_value value in
  real_ret res
;;

let real_jmp bl = return (Jmp bl)

let ibr label =
  let* bl = get_bb_var_from_label label in
  real_jmp bl
;;

let ibrcond cond label1 label2 =
  let* cond = get_const_from_value cond in
  let* label1 = get_bb_var_from_label label1 in
  let* label2 = get_bb_var_from_label label2 in
  let* cond = is_bool cond in
  if cond then real_jmp label1 else real_jmp label2
;;

let real_switch sw dc cases =
  let case = List.find_opt (fun (x, _) -> Int64.equal x sw) cases in
  match case with
  | Some (_, x) -> real_jmp x
  | None -> real_jmp dc
;;

let iswitch tp switcher default_case additional_cases =
  match tp with
  | Ast.TInteger _ ->
    let* df = get_bb_var_from_label default_case in
    let* sw = get_const_from_value switcher >>= is_int in
    let* cases =
      map_list
        (fun (v1, v2) ->
          let* v1 = get_const_from_value v1 >>= is_int in
          let* v2 = get_bb_var_from_label v2 in
          return (v1, v2))
        additional_cases
    in
    real_switch sw df cases
  | _ -> fail "Switch can work only with integers\n"
;;

let launch_terminator_instruction
  : Ast.terminator_instruction -> (state, instr_launch_res) t
  = function
  | Ast.Ret (tp, value) -> iret tp value
  | Ast.Br label -> ibr label
  | Ast.BrCond (cond, label1, label2) -> ibrcond cond label1 label2
  | Ast.Switch (tp, switcher, default_case, additional_cases) ->
    iswitch tp switcher default_case additional_cases
  | Ast.Unreachable -> fail "Launch 'unreachable' instruction"
;;
