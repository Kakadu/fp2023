(** Copyright 2023-2024, tepa46 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ocamladt_lib
open Format

let parse_and_infer_result str =
  match Parser.parse str with
  | Ok parse_result ->
    (match Inferencer.run_infer_program parse_result with
     | Ok (adt_env, env) ->
       printf "%a\n%a" Inferencer.AdtEnv.pp_env adt_env Inferencer.TypeEnv.pp_env env
     | Error err -> printf "%a" InferencerTypes.pp_inf_err err)
  | Error err -> printf "Parsing error: %s\n" err
;;

let%expect_test _ =
  let _ = parse_and_infer_result {| let n = 5 |} in
  [%expect {| "n": int |}]
;;

let%expect_test _ =
  let _ = parse_and_infer_result {| let n = (5, 4, "a") |} in
  [%expect {| "n": int * int * string |}]
;;

let%expect_test _ =
  let _ = parse_and_infer_result {| let n = 5 :: [] 
  let b = n|} in
  [%expect {|
    "b": int list
    "n": int list |}]
;;

let%expect_test _ =
  let _ =
    parse_and_infer_result
      {| let b = []
         let a = "a"
         let f = 5 
         let id = f  |}
  in
  [%expect {|
    "a": string
    "b": '0 . '0 list
    "f": int
    "id": int |}]
;;

let%expect_test _ =
  let _ = parse_and_infer_result {| let n = fun x -> x + 1 
  let b = n 5|} in
  [%expect {|
    "b": int
    "n": int -> int |}]
;;

let%expect_test _ =
  let _ =
    parse_and_infer_result {| let n = fun x -> x
  let b = n 5
  let c = n true |}
  in
  [%expect {|
    "b": int
    "c": bool
    "n": '0 . '0 -> '0 |}]
;;

let%expect_test _ =
  let _ =
    parse_and_infer_result {| let n = fun a :: b -> a 
  let b = n (46 :: 52 :: []) |}
  in
  [%expect {|
      "b": int
      "n": '0 . '0 list -> '0 |}]
;;

let%expect_test _ =
  let _ = parse_and_infer_result {| let n = fun (a, b) -> a + 1 |} in
  [%expect {|
      "n": '1 . int * '1 -> int |}]
;;

let%expect_test _ =
  let _ =
    parse_and_infer_result
      {| let n = fun x -> match x with | true -> true | false -> false |}
  in
  [%expect {|
      "n": bool -> bool |}]
;;

let%expect_test _ =
  let _ =
    parse_and_infer_result {| let x = if true then if true then 1 else 2 else 3 |}
  in
  [%expect {|
      "x": int |}]
;;

let%expect_test _ =
  let _ =
    parse_and_infer_result
      {| let rec factorial_recursive = fun n -> if n <= 1 then 1 else n * factorial_recursive (n - 1)
    let a = factorial_recursive 5
    let b = factorial_recursive 6 |}
  in
  [%expect {|
      "a": int
      "b": int
      "factorial_recursive": int -> int |}]
;;

let%expect_test _ =
  let _ =
    parse_and_infer_result
      {| let rec fix = fun f -> (fun x -> f (fix f) x)
           let fac = fix (fun self -> (fun n -> if n <= 1 then 1 else n * self (n - 1)))
           let a = fac 5 |}
  in
  [%expect
    {|
      "a": int
      "fac": int -> int
      "fix": '2 '3 . (('2 -> '3) -> '2 -> '3) -> '2 -> '3 |}]
;;

let%expect_test _ =
  let _ = parse_and_infer_result {| let rec fix = fun f -> (fun x -> f (fix f) x) |} in
  [%expect {|
      "fix": '2 '3 . (('2 -> '3) -> '2 -> '3) -> '2 -> '3 |}]
;;

let%expect_test _ =
  let _ =
    parse_and_infer_result
      {|let rec f = fun (a, b) -> if a + b < 10 then a + b else f (a-1,b-1) |}
  in
  [%expect {|
      "f": int * int -> int |}]
;;

let%expect_test _ =
  let _ =
    parse_and_infer_result
      {|
      let rev = fun lst ->
        (let rec helper = fun acc -> (fun lst ->
        match lst with
          | [] -> acc
          | h :: tl -> helper (h :: acc) tl)
        in
        helper [] lst)
      let reversed1 = rev (1 :: 2 :: 3 :: 4 :: 5 :: [])
      let reversed2 = rev (true :: false :: false :: false :: [])
    |}
  in
  [%expect
    {|
      "rev": '14 . '14 list -> '14 list
      "reversed1": int list
      "reversed2": bool list |}]
;;

let%expect_test _ =
  let _ =
    parse_and_infer_result
      {| type name = | LName of string | RName of string
         let a = LName "tepa" |}
  in
  [%expect {|
      type name = | RName of string | LName of string

      "a": name |}]
;;

let%expect_test _ =
  let _ =
    parse_and_infer_result {| type name = | LName | RName 
         let a = LName |}
  in
  [%expect {|
      type name = | RName | LName

      "a": name |}]
;;

let%expect_test _ =
  let _ =
    parse_and_infer_result
      {| type kek = | Num of int | Str of string 
         let a = fun n -> Num n
         let b = fun s -> Str s
         let c = a 5
         let d = b "a" |}
  in
  [%expect
    {|
      type kek = | Str of string | Num of int

      "a": int -> kek
      "b": string -> kek
      "c": kek
      "d": kek |}]
;;

let%expect_test _ =
  let _ =
    parse_and_infer_result
      {| type kek = | Num1 of int | Num2 of int
    type lol = | Num of kek | Str of string 
         let a = fun n -> Num n
         let b = fun s -> Str s
         let c = a (Num1 5)
         let d = b "a"
         let check = fun n -> match n with | Num (Num1 n) -> n | _ -> 5
         let id = fun n -> n
         let e = id (Num (Num1 5))
         let f = check e
          |}
  in
  [%expect
    {|
      type kek = | Num2 of int | Num1 of int
      type lol = | Str of string | Num of kek

      "a": kek -> lol
      "b": string -> lol
      "c": lol
      "check": lol -> int
      "d": lol
      "e": lol
      "f": int
      "id": '8 . '8 -> '8 |}]
;;

let%expect_test _ =
  let _ =
    parse_and_infer_result
      {| type rectype = | Rec of rectype | Num of int 
         let a = fun n -> Rec n
         let c = a (Num 5) |}
  in
  [%expect
    {|
      type rectype = | Rec of rectype | Num of int

      "a": rectype -> rectype
      "c": rectype |}]
;;

let%expect_test _ =
  let _ =
    parse_and_infer_result
      {| type typ = | Num of int | Str of string 
         type typ2 = | Num2 of int | Str2 of string
         let convert = fun n -> match n with | Num num -> (Num2 num) | Str str -> (Str2 str)
         let b = convert (Num 5)
         let c = convert (Str "tepa")
         |}
  in
  [%expect
    {|
      type typ = | Str of string | Num of int
      type typ2 = | Str2 of string | Num2 of int

      "b": typ2
      "c": typ2
      "convert": typ -> typ2 |}]
;;

let%expect_test _ =
  let _ =
    parse_and_infer_result
      {| type color = | White | Green | Yellow | Blue | Red | Black
       type eatable = | Yes | No
       type thing = | Apple of color * eatable | Chair of color * eatable | Potato of color * eatable
       let a = Apple (Yellow, Yes)
         |}
  in
  [%expect
    {|
      type color = | Yellow | White | Red | Green | Blue | Black
      type eatable = | Yes | No
      type thing = | Potato of color * eatable | Chair of color * eatable | Apple of color * eatable

      "a": thing |}]
;;

let%expect_test _ =
  let _ =
    parse_and_infer_result
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

      "is_member": bool
      "member": int -> rbtree -> bool
      "node": rbtree
      "node_left": rbtree
      "node_left_left": rbtree
      "node_right": rbtree |}]
;;
