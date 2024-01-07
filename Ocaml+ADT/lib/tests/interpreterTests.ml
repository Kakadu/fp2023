open Ocamladt_lib
open Format

let pp_expected_result_item fmt item =
  let item_name, item_value = item in
  fprintf fmt "%S: %a" item_name InterpreterTypes.pp_value item_value
;;

let rec pp_expected_result fmt = function
  | [] -> ()
  | h :: tl -> fprintf fmt "%a\n%a" pp_expected_result_item h pp_expected_result tl
;;

let parse_and_interpret_result str expected =
  match Parser.parse str with
  | Ok parse_result ->
    (match Interpreter.InterpreterResult.exec_program parse_result with
     | Ok actual
       when List.for_all
              (fun (expected_name, expected_value) ->
                match InterpreterTypes.StringMap.find_opt expected_name actual with
                | Some x when x = expected_value -> true
                | _ -> false)
              expected -> true
     | Ok actual ->
       printf
         "Expected: %a, actual: %a\n"
         pp_expected_result
         expected
         InterpreterTypes.pp_env
         actual;
       false
     | Error err ->
       printf "Interpretation error: %a\n" InterpreterTypes.pp_failure err;
       false)
  | Error err ->
    printf "Parsing error: %s\n" err;
    false
;;

(* tests *)
let%test _ = parse_and_interpret_result "let n = 5" [ "n", VInt 5 ]

let%test _ =
  parse_and_interpret_result "let x : bool = (false || true) && true" [ "x", VBool true ]
;;

let%test _ =
  parse_and_interpret_result "let n = fun x -> x+ 5\n    let a = n 4" [ "a", VInt 9 ]
;;

let%test _ =
  parse_and_interpret_result
    "let rec factorial_recursive = fun n -> if n <= 1 then 1 else n * \
     factorial_recursive (n - 1)\n\
    \     let a = factorial_recursive 5\n\
    \     let b = factorial_recursive 6"
    [ "a", VInt 120; "b", VInt 720 ]
;;

let%test _ = parse_and_interpret_result "let rec n = 5" [ "n", VInt 5 ]

let%test _ =
  parse_and_interpret_result
    "let n = fun y -> (let x = 5 in x + y)\n let f = n 7"
    [ "f", VInt 12 ]
;;

let%test _ =
  parse_and_interpret_result
    "let n = fun y -> if y > 7 then 5 else 3\n let f = n 7"
    [ "f", VInt 3 ]
;;

let%test _ =
  parse_and_interpret_result
    "let n = fun y -> if y > 7 then 5 else 3\n let f = n 100"
    [ "f", VInt 5 ]
;;

let%test _ =
  parse_and_interpret_result
    "let a: int -> bool = fun x -> match x with | Tepa 4 -> true | _ -> false\n\
    \ let n = a (Tepa 4)"
    [ "n", VBool true ]
;;

let%test _ =
  parse_and_interpret_result
    "let a: int -> bool = fun x -> match x with | Tepa 4 -> true | _ -> false\n\
    \ let n = a (Tepa 5)"
    [ "n", VBool false ]
;;

let%test _ =
  parse_and_interpret_result
    "let a: int -> bool = fun x -> match x with | Tepa 4 -> true | _ -> false\n\
    \ let n = a (NeTepa 5)"
    [ "n", VBool false ]
;;

let%test _ =
  parse_and_interpret_result
    "let h = fun h :: tl -> h\n\
    \ let tl = fun h :: tl -> tl\n\
    \ let n = h (4 :: 5 :: 6)\n\
    \ let m = tl (4 :: 5 :: 6)"
    [ "n", VInt 4; "m", VList [ VInt 5; VInt 6 ] ]
;;

let%test _ =
  parse_and_interpret_result
    "let sum = fun (a, b) -> a + b\n\n\
    \ let sub = fun (a, b) -> a - b\n\
    \ let mul = fun (a, b) -> a * b\n\
    \ let n = sum (4, 5)\n\
    \ let m = sub (5, 6)\n\
    \ let k = mul (5, 8)"
    [ "n", VInt 9; "m", VInt (-1); "k", VInt 40 ]
;;

let%test _ =
  parse_and_interpret_result
    "let h = fun h :: tl -> h\n\
    \ let tl = fun h :: tl -> tl\n\
    \ let n = h (Tepa 46 :: Tepa 45 :: Tepa 44)\n\
    \ let m = tl (Tepa 46 :: Tepa 45 :: Tepa 44)"
    [ "n", VAdt ("Tepa", VInt 46)
    ; "m", VList [ VAdt ("Tepa", VInt 45); VAdt ("Tepa", VInt 44) ]
    ]
;;
