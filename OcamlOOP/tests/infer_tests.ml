(** Copyright 2023, Artem-Rzhankoff *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open OcamlOOP_lib
open Inferencer
open PP
open Base.Result

let run_infer input =
  match Parser.parse input >>= check_program with
  | Ok env -> pp_program Format.std_formatter env
  | Error err ->
    (match err with
     | Parser e -> Parser.PP.pp_error Format.std_formatter e
     | Infer e -> PP.pp_error Format.std_formatter e
     | _ -> Format.printf "o_0\n")
;;

(*=====================Simple tests=====================*)

let%expect_test _ =
  let () = run_infer {|let a = 5 * 9 / 7|} in
  [%expect {|var a: int|}]
;;

let%expect_test _ =
  let () = run_infer {|let a = [1; 2; 3451; 12]|} in
  [%expect {|var a: int list|}]
;;

let%expect_test _ =
  let () = run_infer {|let a = (true, 55 - 892, [(1, 2); (3, 5)])|} in
  [%expect {| var a: bool * int * (int * int) list |}]
;;

let%expect_test _ =
  let () = run_infer {|let a = [(true, 55 - 892, [(1, 2); (3, 5)])]|} in
  [%expect {| var a: bool * int * (int * int) list list |}]
;;

let%expect_test _ =
  let () = run_infer {|let rec fact x =  if x = 0 then 1 else x * fact (x - 1)|} in
  [%expect {|var fact: int -> int|}]
;;

let%expect_test _ =
  let () = run_infer {|let a x = x :: [1; 2; 3]|} in
  [%expect {|var a: int -> int list|}]
;;

(* Strange case *)
let%expect_test _ =
  let () = run_infer {|let (5::a) = [3; 4; 6]|} in
  [%expect {| var a: int list |}]
;;

let%expect_test _ =
  let () = run_infer {|let (a::b) = [3; 4; 6]|} in
  [%expect {|
    var a: int
    var b: int list |}]
;;

let%expect_test _ =
  let () = run_infer {|let (a::b::c) = [3; 4; 6]|} in
  [%expect {|
    var a: int
    var b: int
    var c: int list |}]
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
  let () = run_infer {|let a = (fun x y -> (x, y)) 1 2|} in
  [%expect {| var a: int * int |}]
;;

let%expect_test _ =
  let () = run_infer {|let list = (0, [(-1, [(-2, [], [])], [])], [])|} in
  [%expect
    {| var list: int * (int * (int * 'a list * 'b list) list * 'c list) list * 'd list |}]
;;

let input =
  {|
let reverse = 
  let rec helper acc list = 
    match list with 
      | [] -> acc
      | (h :: tl) -> helper (h :: acc) tl
  in
  let reverse_int = helper [] ([1; 2; 3; 4; 5]) in 
  let reverse_intt = helper [] ([true; false]) in
  (reverse_int, reverse_intt)
|}
;;

let%expect_test _ =
  let _ = run_infer input in
  [%expect {| var reverse: int list * bool list |}]
;;

(*=====================Objects=====================*)

let%expect_test _ =
  let _ = run_infer {|let a = object val a = 5 end|} in
  [%expect {| var a: <  > |}]
;;

let%expect_test _ =
  let _ = run_infer {|let a = object method a = 5 end|} in
  [%expect {| var a: < a : int > |}]
;;

let%expect_test _ =
  let _ = run_infer {|let a = object val a = true method a = a end|} in
  [%expect {| var a: < a : bool > |}]
;;

let%expect_test _ =
  let _ = run_infer {|let a = object (self) method b = 5 method a = self#b end|} in
  [%expect {| var a: < b : int; a : int > |}]
;;

let%expect_test _ =
  let _ = run_infer {|let a = object (self) method b = self#c method c = true end|} in
  [%expect {| var a: < b : bool; c : bool > |}]
;;

let%expect_test _ =
  let _ = run_infer {|let obj = object method get = 5 end let main = obj#get|} in
  [%expect {|
    var main: int
    var obj: < get : int > |}]
;;

let%expect_test _ =
  let _ = run_infer {|let obj = object method get x = 5 + x end let main = obj#get 5|} in
  [%expect {|
    var main: int
    var obj: < get : int -> int > |}]
;;

let%expect_test _ =
  let _ = run_infer {|let a = object (self) method b = self#c 5 method c x = x end|} in
  [%expect {| var a: < b : int; c : int -> int > |}]
;;

let%expect_test _ =
  let _ =
    run_infer
      {|let a = object (self) method d x = x + 2 method b = self#c method c = self#d 0 end|}
  in
  [%expect {| var a: < d : int -> int; b : int; c : int > |}]
;;

let%expect_test _ =
  let _ = run_infer {|let a o = o#meth|} in
  [%expect {| var a: < meth : 'a; .. > -> 'a |}]
;;

let%expect_test _ =
  let _ =
    run_infer
      {|let a o = o#get let obj = object method get = 8 method set x = x end let main = a obj |}
  in
  [%expect
    {|
    var a: < get : 'a; .. > -> 'a
    var main: int
    var obj: < get : int; set : 'a -> 'a > |}]
;;

let input =
  {|
let extract x = x#get
let obj = object method get = 8 method set = (5, 2) end
let objj = object method foo = if true then true else false method get = (true, 5) end

let some_o = extract obj
let some_oo = extract objj
|}
;;

let%expect_test _ =
  let _ = run_infer input in
  [%expect
    {|
    var extract: < get : 'a; .. > -> 'a
    var obj: < get : int; set : int * int >
    var objj: < foo : bool; get : bool * int >
    var some_o: int
    var some_oo: bool * int |}]
;;

let%expect_test _ =
  let _ = run_infer {|let o = object (self) method a = self#b method b = 5 end|} in
  [%expect {| var o: < a : int; b : int > |}]
;;

let%expect_test _ =
  let _ =
    run_infer {|let o = object (self) method a = self#c val b = 5 method c = b + 4 end|}
  in
  [%expect {| var o: < a : int; c : int > |}]
;;

let input = {|
let f x = x#a + 1
let g x = x#b

let h x = f x + g x
|}

let%expect_test _ =
  let _ = run_infer input in
  [%expect
    {|
    var f: < a : int; .. > -> int
    var g: < b : 'a; .. > -> 'a
    var h: < a : int; b : int; .. > -> int |}]
;;

let%expect_test _ =
  let _ = run_infer {|let a x = let b = x#get in x|} in
  [%expect {| var a: < get : 'a; .. > -> < get : 'a; .. > |}]
;;

let%expect_test _ =
  let _ =
    run_infer
      {|let o x = object (self) val n = x method c = n method b = {< n = true >} end|}
  in
  [%expect {| var o: bool -> < c : bool; b : 'a > as 'a |}]
;;

let%expect_test _ =
  let () =
    run_infer {| let o x = object (self) val n = x  method c = {< n = true>} end |}
  in
  [%expect {| var o: bool -> < c : 'a > as 'a |}]
;;

let%expect_test _ =
  let () =
    run_infer
      {| let o = object (self) val t = [] method b = if true then {<t = []>} else {<t = []>} end |}
  in
  [%expect {| var o: < b : 'a > as 'a |}]
;;

(*=====================Polymorphic=====================*)

let%expect_test _ =
  let () = run_infer {|let f x = x|} in
  [%expect {| var f: 'a -> 'a |}]
;;

let%expect_test _ =
  let () = run_infer {|let (a, b) = (fun a x -> a x), (fun a x -> a x)|} in
  [%expect {|
    var a: ('a -> 'b) -> 'a -> 'b
    var b: ('a -> 'b) -> 'a -> 'b |}]
;;

let%expect_test _ =
  let () = run_infer {|let f g a b = g a b|} in
  [%expect {| var f: ('a -> 'b -> 'c) -> 'a -> 'b -> 'c |}]
;;

let%expect_test _ =
  let () = run_infer {|let a x = let id x = x in let (a, b) = (id x, id x) in (a, b)|} in
  [%expect {| var a: 'a -> 'a * 'a |}]
;;

(*=====================Errors=====================*)

let%expect_test _ =
  let () = run_infer {|let a x = x x|} in
  [%expect {| The type variable 'a occurs inside 'a -> 'b |}]
;;

let%expect_test _ =
  let () = run_infer {|let dummy = 12 + true|} in
  [%expect {| This expression has type bool but an expression was expected of type int |}]
;;

let%expect_test _ =
  let () = run_infer {|let (x, x) = (1, 1)|} in
  [%expect {| Variable x is bound several times |}]
;;

let%expect_test _ =
  let () = run_infer {|let several_bounds x = let (y::y) = x in y|} in
  [%expect {| Variable y is bound several times |}]
;;

let%expect_test _ =
  let () = run_infer {|let nested_bounds x = let ((h::tl), h) = x in h|} in
  [%expect {| Variable h is bound several times |}]
;;

let%expect_test _ =
  let () = run_infer {|let rec (a, b) = 5|} in
  [%expect {| Only variables are allowed as left-side of 'let rec' |}]
;;

let%expect_test _ =
  let () = run_infer {| let increase = x + 1|} in
  [%expect {| Unbound value 'x' |}]
;;

let%expect_test _ =
  let () = run_infer {| let rec f x y = if x = y then f x else f y|} in
  [%expect {| The type variable 'b occurs inside 'a -> 'b |}]
;;

let%expect_test _ =
  let () = run_infer {|let o = object (self) val n = 5 method b = {< n = true >} end|} in
  [%expect {| This expression has type int but an expression was expected of type bool |}]
;;

let%expect_test _ =
  let () = run_infer "let a x = fun y -> y x" in
  [%expect {| var a: 'a -> ('a -> 'b) -> 'b |}]
;;
