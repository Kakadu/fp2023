(** Copyright 2021-2023, Kakadu, RozhkovAleksandr *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Format
open Interpreter

let interpret = Interpreter.interpret
let extract_expressions program = List.map (fun (expr, _) -> expr) program

let rec pp_value_list fmt = function
  | [] -> ()
  | [ v ] -> pp_value fmt v
  | v :: vs ->
    pp_value fmt v;
    Format.fprintf fmt "; ";
    pp_value_list fmt vs
;;

let process_program input =
  match Parser.parse input with
  | Result.Ok tree ->
    let infer_result = Inferencer.infer_program tree in
    if Result.is_ok infer_result
    then (
      let _, program = Result.get_ok infer_result in
      let interpret_result = Interpreter.interpret (extract_expressions program) in
      if Result.is_ok interpret_result
      then (
        let _, vl = Result.get_ok interpret_result in
        List.iter
          (function
            | VInt x -> Printf.printf "Integer result: %d\n" x
            | VBool b -> Printf.printf "Boolean result: %b\n" b
            | VUnit -> Printf.printf "Unit result\n"
            | VList lst ->
              Printf.printf "List result: ";
              pp_value_list Format.std_formatter lst;
              Printf.printf "\n"
            | VFun _ -> Printf.printf "Function result\n")
          vl)
      else printf "Interpretation error\n")
    else printf "Typecheck error\n"
  | _ -> printf "Parsing error\n"
;;

let () =
  let input1 =
    {|
      let number_to_word n =
        match n with
        | 1 -> 11
        | 2 -> 12
        | 3 -> 13
        | _ -> 14

      let x = number_to_word 2
    |}
  in
  process_program input1;
  let factorial =
    {|
      let rec fact n = if n = 1 then n else n * fact (n - 1)
      let x = fact 3
    |}
  in
  process_program factorial;
  let input2 =
    {|
      let add_two_numbers x = x + 2
      let result = add_two_numbers 5
    |}
  in
  process_program input2;
  let input3 = {|
  let numbers = match 
  |} in
  process_program input3;
  let input4 = {|
  let numbers = [1; 2; 3; 4; 5]
  |} in
  process_program input4;
  let error1 = {|
  let a = 0 in let b = 1 in b / a
  |} in
  process_program error1;
  let error2 = {|
  let x n = if 5 then 5 else 4
  |} in
  process_program error2
;;

(** Tests for interpret.ml*)
open Ast

let run_tests () =
  let env = EnvValues.empty in
  let test_eval_int_constant () =
    let c = Const (Int 42) in
    let result = Interpreter.eval env c in
    match result with
    | Ok (VInt x) -> assert (x = 42)
    | _ -> failwith "Test failed: eval_int_constant"
  in
  let test_eval_bool_constant () =
    let c = Const (Bool true) in
    let result = Interpreter.eval env c in
    match result with
    | Ok (VBool x) -> assert (x = true)
    | _ -> failwith "Test failed: eval_bool_constant"
  in
  let test_eval_var_in_environment () =
    let id = "x" in
    let env' = EnvValues.extend env (id, VInt 10) in
    let v = Var id in
    let result = Interpreter.eval env' v in
    match result with
    | Ok (VInt x) -> assert (x = 10)
    | _ -> failwith "Test failed: eval_var_in_environment"
  in
  let test_eval_if_then_else () =
    let c = IfThenElse (Const (Bool true), Const (Int 10), Const (Int 20)) in
    let result = Interpreter.eval env c in
    match result with
    | Ok (VInt x) -> assert (x = 10)
    | _ -> failwith "Test failed: eval_if_then_else"
  in
  let test_eval_function_application () =
    let f = Fun ("x", Binop (Add, Var "x", Const (Int 5))) in
    let e = Const (Int 7) in
    let app = App (f, e) in
    let result = Interpreter.eval env app in
    match result with
    | Ok (VInt x) -> assert (x = 12)
    | _ -> failwith "Test failed: eval_function_application"
  in
  test_eval_if_then_else ();
  test_eval_function_application ();
  test_eval_int_constant ();
  test_eval_bool_constant ();
  test_eval_var_in_environment ();
  print_endline "All tests passed!"
;;

run_tests ()
