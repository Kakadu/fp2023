(** Copyright 2021-2023, Kakadu and contributors *)

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
    (match Inferencer.infer_program tree with
     | Result.Ok (_, program) ->
       (match Interpreter.interpret (extract_expressions program) with
        | Result.Ok (_, rs) ->
          List.iter
            (fun result ->
              match result with
              | VInt x -> printf "Integer result: %d\n" x
              | VBool b -> printf "Boolean result: %b\n" b
              | VUnit -> printf "Unit result\n"
              | VList lst ->
                printf "List result: ";
                pp_value_list Format.std_formatter lst;
                printf "\n"
              | VFun _ -> printf "Function result\n")
            rs
        | _ -> printf "Interpretation error\n")
     | _ -> printf "Typecheck error\n")
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
  let numbers = [1; 2; 3; 4; 5]
  |} in
  process_program input3;
  let error1 = {|
  let a = 0 in let b = 1 in b / a
  |} in
  process_program error1;
  let error2 = {|
  let x n = if 5 then 5 else 4
  |} in
  process_program error2
;;
