(** Copyright 2021-2023, Kakadu, RozhkovAleksandr *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Format
open Interpreter

let interpret = Interpreter.interpret
let extract_expressions program = List.map (fun (expr, _) -> expr) program

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
          (fun value ->
            pp_value Format.std_formatter value;
            Format.pp_print_flush Format.std_formatter ())
          vl)
      else printf "Interpretation error\n")
    else printf "Typecheck error\n"
  | _ -> printf "Parsing error\n"
;;

let%expect_test _ =
  process_program
    {|
      let number_to_word n =
        match n with
        | 1 -> 11
        | 2 -> 12
        | 3 -> 13
        | _ -> 14

      let x = number_to_word 2
       |};
  [%expect {| 
        <fun>
        12
      |}]
;;

let%expect_test _ =
  process_program
    {|
        let rec fact n = if n = 1 then n else n * fact (n - 1)
      let x = fact 3
       |};
  [%expect {| 
        <fun>
        6
      |}]
;;

let%expect_test _ =
  process_program
    {|
        let add_two_numbers x = x + 2
      let result = add_two_numbers 5
       |};
  [%expect {| 
        <fun>
        7
      |}]
;;

let%expect_test _ =
  process_program {|
        let numbers = [1; 2; 3; 4; 5]
       |};
  [%expect {| 
        [1 ; 2 ; 3 ; 4 ; 5 ]
      |}]
;;

let%expect_test _ =
  process_program {|
          let x = 10
       |};
  [%expect {| 
        10
      |}]
;;

let%expect_test _ =
  process_program {|
         let x = true
       |};
  [%expect {| 
        true
      |}]
;;

let%expect_test _ =
  process_program {|
         let x = false
       |};
  [%expect {| 
        false
      |}]
;;

let%expect_test _ =
  process_program {|
        let f x = x + 5
        let x = f 6
       |};
  [%expect {| 
        <fun>
        11
      |}]
;;

let%expect_test _ =
  process_program
    {|
         let b = 5 < 4
  let c = 5 > 4
  let f = 5 / 4
  let q = true = true
       |};
  [%expect {| 
    false true 1 false
       |}]
;;

let%expect_test _ =
  process_program
    {|
       let number_to_word n =
        match n with
        | true -> 11
        | _ -> 14

      let x = number_to_word true
       |};
  [%expect {| 
    <fun>
    11
       |}]
;;

let%expect_test _ =
  process_program {|
       let a = 0 in let b = 1 in b / a
       |};
  [%expect {| Interpretation error |}]
;;

let%expect_test _ =
  process_program {|
         let x n = if 5 then 5 else 4
       |};
  [%expect {| Typecheck error |}]
;;

let%expect_test _ =
  process_program {|
         let _ = 5 . 4
       |};
  [%expect {| Parsing error |}]
;;
