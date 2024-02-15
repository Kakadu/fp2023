(** Copyright 2023-2024, Efremov Alexey *)

(** SPDX-License-Identifier: CC0-1.0 *)

open Ihelp

let interpretate_programm prog =
  let p_res = Parser.Parsing.parse_program prog in
  match p_res with
  | Result.Ok ast ->
    let c_res = Checks.SsaCheck.run_ssa_glob_list ast in
    (match c_res with
     | Result.Ok _ ->
       let i_res = Interpreter.Interpreting.run_interpretate_on_ast ast in
       (match i_res with
        | _, Result.Ok res ->
          Printf.printf "Programm return: \n\t %s" (Ast.show_const res)
        | st, Result.Error s ->
          let _ = Printf.printf "Got error during interpretation \n\t%s" s in
          let _ = Printf.printf "\n\n%s" (State.show_state st) in
          ())
     | Result.Error s -> Printf.printf "SSA check failed: \n\t%s" s)
  | Result.Error s -> Printf.printf "Parser error: \n\t%s" s
;;
