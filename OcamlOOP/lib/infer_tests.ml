(** Copyright 2021-2023, Artem-Rzhankoff *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Infer
open Parser
open Angstrom
open Pp_infer

let run_infer input =
  let run_parser = parse_string ~consume:Prefix program input in
  match run_parser with
  | Error e -> print_endline e
  | Ok statements ->
    (match check_program statements with
     | Error e -> pp_error Format.std_formatter e
     | Ok env -> pp_program Format.std_formatter env)
;;

(*=====================Simple tests=====================*)

let%expect_test _ =
  let () = run_infer "let a = 5 * 9 / 7" in
  [%expect {|var a: int|}]
;;

let%expect_test _ =
  let () = run_infer "let a = [1; 2; 3451; 12]" in
  [%expect {|var a: int list|}]
;;

let%expect_test _ =
  let () = run_infer "let a = (true, 55 - 892, [(1, 2); (3, 5)])" in
  [%expect {| var a: bool * int * (int * int) list |}]
;;

let%expect_test _ =
  let () = run_infer "let a = [(true, 55 - 892, [(1, 2); (3, 5)])]" in
  [%expect {| var a: bool * int * (int * int) list list |}]
;;

let%expect_test _ =
  let () = run_infer "let rec fact x =  if x = 0 then 1 else x * fact (x - 1)" in
  [%expect {|var fact: int -> int|}]
;;

let%expect_test _ =
  let () = run_infer "let a x = x :: [1; 2; 3]" in
  [%expect {|var a: int -> int list|}]
;;

(* Strange case *)
let%expect_test _ =
  let () = run_infer "let (5::a) = [3; 4; 6]" in
  [%expect {| var a: int list |}]
;;

let input = {|
let tuple = (22, 23)
let (_, b) = tuple
|}

let%expect_test _ =
  let () = run_infer input in
  [%expect {|
    var b: int
    var tuple: int * int |}]
;;

let input =
  {|
let pat_matching x = 
  match x with 
  | (52, 52) -> true
  | _ -> false
|}
;;

let%expect_test _ =
  let () = run_infer input in
  [%expect {|
    var pat_matching: int * int -> bool |}]
;;

let input =
  {|
let check_equal x y = 
  let sum a b = a + b in 
  let sub a b = a - b in 
  let rev x = -x in 
  if rev x = rev y then sub x y else sum x y
|}
;;

let%expect_test _ =
  let () = run_infer input in
  [%expect {|
    var check_equal: int -> int -> int |}]
;;

let%expect_test _ =
  let () = run_infer "let a = (fun x y -> (x, y)) 1 2" in
  [%expect {| var a: int * int |}]
;;

(*=====================Expressions=====================*)

(*
   let%expect_test _ =
   let _ = run_infer "" in
   [%expect {||}]
   ;;
*)

(*=====================Polymorphic value=====================*)

let%expect_test _ =
  let () = run_infer "let f x = x" in
  [%expect {| var f: 'a -> 'a |}]
;;

let%expect_test _ =
  let () = run_infer "let (a, b) = (fun a x -> a x), (fun a x -> a x)" in
  [%expect {|
    var a: ('a -> 'b) -> 'a -> 'b
    var b: ('a -> 'b) -> 'a -> 'b |}]
;;

let%expect_test _ =
  let () = run_infer "let f g a b = g a b" in
  [%expect {| var f: ('a -> 'b -> 'c) -> 'a -> 'b -> 'c |}]
;;

let%expect_test _ =
  let () = run_infer "let a x = let id x = x in let (a, b) = (id x, id x) in (a, b)" in
  [%expect {| var a: 'a -> 'a * 'a |}]
;;

(*=====================Errors=====================*)

let%expect_test _ =
  let () = run_infer "let a x = x x" in
  [%expect {| The type variable 'a occurs inside 'a -> 'b |}]
;;

let%expect_test _ =
  let () = run_infer "let dummy = 12 + true" in
  [%expect {| This expression has type bool but an expression was expected of type int |}]
;;

let%expect_test _ =
  let () = run_infer "let (x, x) = (1, 1)" in
  [%expect {| Variable x is bound several times |}]
;;

let%expect_test _ =
  let () = run_infer "let several_bounds x = let (y::y) = x in y" in
  [%expect {| Variable y is bound several times |}]
;;

let%expect_test _ =
  let () = run_infer "let nested_bounds x = let ((h::tl), h) = x in h" in
  [%expect {| Variable h is bound several times |}]
;;

let%expect_test _ =
  let () = run_infer "let rec (a, b) = 5" in
  [%expect {| Only variables are allowed as left-side of 'let rec' |}]
;;

let%expect_test _ =
  let () = run_infer "let increase = x + 1" in
  [%expect {| Unbound value 'x' |}]
;;

let%expect_test _ =
  let () = run_infer "let rec f x y = if x = y then f x else f y" in
  [%expect {| The type variable 'b occurs inside 'a -> 'b |}]
;;

