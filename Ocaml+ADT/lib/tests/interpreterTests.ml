(** Copyright 2023-2024, tepa46 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ocamladt_lib
open Format

let parse_and_interpret_result str =
  match Parser.parse str with
  | Ok parse_result ->
    (match Inferencer.run_infer_program parse_result with
     | Ok (adt_env, env) ->
       (match Interpreter.InterpreterResult.exec_program parse_result with
        | Ok res -> InterpreterResultPrinter.pp_interpret_res adt_env env res
        | Error err -> printf "%a" InterpreterTypes.pp_failure err)
     | Error err -> printf "%a" InferencerTypes.pp_inf_err err)
  | Error err -> printf "Parsing error: %s\n" err
;;

(* tests *)
let%expect_test _ =
  let _ = parse_and_interpret_result {| let n = 5 |} in
  [%expect {| "n": int = 5 |}]
;;

let%expect_test _ =
  let _ = parse_and_interpret_result {| let x = (false || true) && true |} in
  [%expect {| "x": bool = true|}]
;;

let%expect_test _ =
  let _ = parse_and_interpret_result {| let x = false && true |} in
  [%expect {| "x": bool = false|}]
;;

let%expect_test _ =
  let _ = parse_and_interpret_result {| let x = true || true |} in
  [%expect {| "x": bool = true|}]
;;

let%expect_test _ =
  let _ = parse_and_interpret_result {| let x = true = true |} in
  [%expect {| "x": bool = true|}]
;;

let%expect_test _ =
  let _ = parse_and_interpret_result {| let x = true <> false |} in
  [%expect {| "x": bool = true|}]
;;

let%expect_test _ =
  let _ =
    parse_and_interpret_result
      {| let n = fun x -> 8 = x + 6 * 2 / 3
         let a = n 4 |}
  in
  [%expect {|
    "a": bool = true
    "n": int -> bool = <fun> |}]
;;

let%expect_test _ =
  let _ =
    parse_and_interpret_result
      {| let n = "a"
         let a =  "a" = n
         let b = "b" <> n |}
  in
  [%expect {|
    "a": bool = true
    "b": bool = true
    "n": string = "a" |}]
;;

let%expect_test _ =
  let _ =
    parse_and_interpret_result
      {| let rec factorial_recursive = fun n -> if n <= 1 then 1 else n * factorial_recursive (n - 1)
         let a = factorial_recursive 5
         let b = factorial_recursive 6 |}
  in
  [%expect
    {|
    "a": int = 120
    "b": int = 720
    "factorial_recursive": int -> int = <rec fun> |}]
;;

let%expect_test _ =
  let _ =
    parse_and_interpret_result
      {| let rec fix = fun f -> (fun x -> f (fix f) x)
         let fac = fix (fun self -> (fun n -> if n <= 1 then 1 else n * self (n - 1)))
         let a = fac 5 |}
  in
  [%expect
    {|
    "a": int = 120
    "fac": int -> int = <fun>
    "fix": '2 '3 . (('2 -> '3) -> '2 -> '3) -> '2 -> '3 = <rec fun> |}]
;;

let%expect_test _ =
  let _ = parse_and_interpret_result {| let rec n = 5 |} in
  [%expect {| "n": int = 5 |}]
;;

let%expect_test _ =
  let _ =
    parse_and_interpret_result
      {| let n = fun y -> (let x = 5 in x + y)
         let f = n 7 |}
  in
  [%expect {|
    "f": int = 12
    "n": int -> int = <fun> |}]
;;

let%expect_test _ =
  let _ =
    parse_and_interpret_result
      {| let n = fun y -> if y > 7 then 5 else 3
         let f = n 7 |}
  in
  [%expect {|
    "f": int = 3
    "n": int -> int = <fun>  |}]
;;

let%expect_test _ =
  let _ =
    parse_and_interpret_result
      {| let n = fun y -> if y > 7 then 5 else 3
         let f = n 100 |}
  in
  [%expect {|
    "f": int = 5
    "n": int -> int = <fun> |}]
;;

let%expect_test _ =
  let _ =
    parse_and_interpret_result
      {| type me = | Tepa of int
      let a = fun x -> match x with | Tepa 4 -> true | _ -> false
         let n = a (Tepa 4) |}
  in
  [%expect
    {|
    type me = | Tepa of int
    "a": me -> bool = <fun>
    "n": bool = true |}]
;;

let%expect_test _ =
  let _ =
    parse_and_interpret_result
      {| type me = | Tepa of int
      let a = fun x -> match x with | Tepa 4 -> true | _ -> false
         let n = a (Tepa 5) |}
  in
  [%expect
    {|
    type me = | Tepa of int
    "a": me -> bool = <fun>
    "n": bool = false |}]
;;

let%expect_test _ =
  let _ =
    parse_and_interpret_result
      {| type me = | Tepa of int | NeTepa of int
      let a = fun x -> match x with | Tepa 4 -> true | _ -> false
         let n = a (NeTepa 5) |}
  in
  [%expect
    {|
    type me = | Tepa of int | NeTepa of int
    "a": me -> bool = <fun>
    "n": bool = false |}]
;;

let%expect_test _ =
  let _ =
    parse_and_interpret_result
      {| let h = fun h :: tl -> h
         let tl = fun h :: tl -> tl
         let n = h (4 :: 5 :: 6 :: [])
         let m = tl (4 :: 5 :: 6 :: []) |}
  in
  [%expect
    {|
    "h": '0 . '0 list -> '0 = <fun>
    "m": int list = [5; 6]
    "n": int = 4
    "tl": '2 . '2 list -> '2 list = <fun> |}]
;;

let%expect_test _ =
  let _ =
    parse_and_interpret_result
      {| let sum = fun (a, b) -> a + b
         let sub = fun (a, b) -> a - b
         let mul = fun (a, b) -> a * b
         let n = sum (4, 5)
         let m = sub (5, 6)
         let k = mul (5, 8) |}
  in
  [%expect
    {|
    "k": int = 40
    "m": int = -1
    "mul": int * int -> int = <fun>
    "n": int = 9
    "sub": int * int -> int = <fun>
    "sum": int * int -> int = <fun> |}]
;;

let%expect_test _ =
  let _ =
    parse_and_interpret_result
      {| type me = | Tepa of int
         let h = fun h :: tl -> h
         let tl = fun h :: tl -> tl
         let n = h (Tepa 46 :: Tepa 45 :: Tepa 44 :: [])
         let m = tl (Tepa 46 :: Tepa 45 :: Tepa 44 :: []) |}
  in
  [%expect
    {|
      type me = | Tepa of int
      "h": '0 . '0 list -> '0 = <fun>
      "m": me list = [Tepa 45; Tepa 44]
      "n": me = Tepa 46
      "tl": '2 . '2 list -> '2 list = <fun> |}]
;;

let%expect_test _ =
  let _ =
    parse_and_interpret_result
      {| type tree = | Tree of tree * tree | Num of int
         let a = fun x -> match x with | Tree (_, _) -> true | _ -> false
         let n = a (Tree (Tree (Tree (Num 2, Num 3), Num 2), Tree(Num 2, Num 3))) |}
  in
  [%expect
    {|
    type tree = | Tree of tree * tree | Num of int
    "a": tree -> bool = <fun>
    "n": bool = true |}]
;;

let%expect_test _ =
  let _ =
    parse_and_interpret_result
      {| let a = fun x -> match x with | [] -> false | h :: tl -> true
         let n = a (44 :: 45 :: 56 :: []) |}
  in
  [%expect {|
    "a": '2 . '2 list -> bool = <fun>
    "n": bool = true |}]
;;

let%expect_test _ =
  let _ =
    parse_and_interpret_result
      {| let a = fun x -> match x with | [] -> false | h :: tl -> true
         let n = a [] |}
  in
  [%expect {|
    "a": '2 . '2 list -> bool = <fun>
    "n": bool = false |}]
;;

let%expect_test _ =
  let _ =
    parse_and_interpret_result
      {| let a = fun x -> match x with | true -> false | false -> true
         let n = a true |}
  in
  [%expect {|
    "a": bool -> bool = <fun>
    "n": bool = false |}]
;;

let%expect_test _ =
  let _ =
    parse_and_interpret_result
      {| let a = fun x -> match x with | "A" -> true | "B" -> true | _ -> false
         let n = a "C" |}
  in
  [%expect {|
    "a": string -> bool = <fun>
    "n": bool = false |}]
;;

let%expect_test _ =
  let _ = parse_and_interpret_result {| let n = 5 / 0 |} in
  [%expect {| DivisionByZeroError |}]
;;

let%expect_test _ =
  let _ =
    parse_and_interpret_result
      {| type me = | Tepa of int
         let h = fun (Tepa h) :: tl -> h
         let tl = fun h :: tl -> tl
         let n = h (Tepa 46 :: Tepa 45 :: Tepa 44 :: [])
         let m = tl (Tepa 46 :: Tepa 45 :: Tepa 44 :: []) |}
  in
  [%expect
    {|
      type me = | Tepa of int
      "h": me list -> int = <fun>
      "m": me list = [Tepa 45; Tepa 44]
      "n": int = 46
      "tl": '2 . '2 list -> '2 list = <fun> |}]
;;

let%expect_test _ =
  let _ =
    parse_and_interpret_result
      {| type color = | Red | Black

         type rbtree =
         | Empty
         | Node of color * int * rbtree * rbtree

         let rec member = fun x -> 
          (fun n -> 
          match n with 
           | Empty -> false
           | Node (_, y, left, right) -> if x = y then true else if x < y then member x left else member x right)

         let node_left_left = Node(Black, 3, Empty, Empty)

         let node_left = Node (Red, 4, node_left_left, Empty)

         let node_right = Node(Red, 10, Empty, Empty)
   
         let node = Node (Black, 5, node_left, node_right)

         let is_member = member 4 node
      |}
  in
  [%expect
    {|
      type color = | Red | Black
      type rbtree = | Node of color * int * rbtree * rbtree | Empty
      "is_member": bool = true
      "member": int -> rbtree -> bool = <rec fun>
      "node": rbtree = Node (Black, 5, Node (Red, 4, Node (Black, 3, Empty, Empty), Empty), Node (Red, 10, Empty, Empty))
      "node_left": rbtree = Node (Red, 4, Node (Black, 3, Empty, Empty), Empty)
      "node_left_left": rbtree = Node (Black, 3, Empty, Empty)
      "node_right": rbtree = Node (Red, 10, Empty, Empty) |}]
;;
