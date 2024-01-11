(** Copyright 2023, Artem-Rzhankoff *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open OcamlOOP_lib
open Interpreter

let run_interpreter = eval_with_printing

let%expect_test _ =
  let () =
    run_interpreter
      {|let rec fact x = if x = 0 then 1 else x * fact (x-1) let eval =  fact 5|}
  in
  [%expect {|
    val eval : int = 120
    val fact : int -> int = <fun> |}]
;;

let%expect_test _ =
  let () = run_interpreter {|let a x = if x then 1 else 5;; let eval = a false|} in
  [%expect {|
    val a : bool -> int = <fun>
    val eval : int = 5 |}]
;;

let%expect_test _ =
  let () =
    run_interpreter
      {|let a = ((fun x -> x), (fun y -> y)) < ((fun z -> z), (fun a -> a))|}
  in
  [%expect {| Invalid argument for compare: functional value |}]
;;

let%expect_test _ =
  let () = run_interpreter {|let a = (5, 6, 10, 34)|} in
  [%expect {| val a : int * int * int * int = (5, 6, 10, 34) |}]
;;

let%expect_test _ =
  let () = run_interpreter {|let l = [5; 234; 22; -299; 50]|} in
  [%expect {| val l : int list = [5; 234; 22; -299; 50] |}]
;;

let%expect_test _ =
  let () = run_interpreter {|let l = [324; true]|} in
  [%expect {| This expression has type int but an expression was expected of type bool |}]
;;

let%expect_test _ =
  let () =
    run_interpreter {|let a x = match x with 1 -> true | _ -> false;; let b = a 3|}
  in
  [%expect {|
    val a : int -> bool = <fun>
    val b : bool = false |}]
;;

let%expect_test _ =
  let () = run_interpreter {|let (5, a) = (4, 3)|} in
  [%expect {| Match failure |}]
;;

let%expect_test _ =
  let () =
    run_interpreter
      {|let match_list = let l = [1; 2; 3; 4] in match l with (1::b) -> b | _ -> []|}
  in
  [%expect {| val match_list : int list = [2; 3; 4] |}]
;;

let%expect_test _ =
  let () = run_interpreter {|let a = []|} in
  [%expect {| val a : 'a list = [] |}]
;;

let%expect_test _ =
  let () =
    run_interpreter {|let tuple = ((fun x -> x + 1), (fun b -> if b then 1 else 0))|}
  in
  [%expect {| val tuple : (int -> int) * (bool -> int) = (<fun>, <fun>) |}]
;;

let%expect_test _ =
  let () = run_interpreter {|let rec a = a|} in
  [%expect {| This kind of expression is not allowed as right-hand side of `let rec a' |}]
;;

let%expect_test _ =
  let () = run_interpreter {|let a = object end|} in
  [%expect {| val a : <  > = <obj> |}]
;;

let input = {|
let a = object method get = 5 end

let main = a#get
|}

let%expect_test _ =
  let () = run_interpreter input in
  [%expect {|
    val a : < get : int > = <obj>
    val main : int = 5 |}]
;;

let input =
  {|
  let a = object (self) val n = 0 method increase x = n + x end
  
  let main = a#increase 5
  |}
;;

let%expect_test _ =
  let () = run_interpreter input in
  [%expect {|
      val a : < increase : int -> int > = <obj>
      val main : int = 5 |}]
;;

let input =
  {|
  let a = object (self) val n = 0 method increase x = n + x end
  
  let main = a#increase 5
  |}
;;

let%expect_test _ =
  let () = run_interpreter input in
  [%expect {|
      val a : < increase : int -> int > = <obj>
      val main : int = 5 |}]
;;

let input =
  {|
  let a = object (self) val n = 0 method b = 8 method increase x = self#b + x end
  
  let main = a#increase 5
  |}
;;

let%expect_test _ =
  let () = run_interpreter input in
  [%expect
    {|
      val a : < b : int; increase : int -> int > = <obj>
      val main : int = 13 |}]
;;

let input =
  {|
  let a = object (self) val n = 0 method increase = {< n = n + 1>} method get = n end
  
  let b = a#increase
  let get_a = a#get
  let get_b = b#get
  |}
;;

let%expect_test _ =
  let () = run_interpreter input in
  [%expect
    {|
      val a : < increase : 'a; get : int > as 'a = <obj>
      val b : < increase : 'a; get : int > as 'a = <obj>
      val get_a : int = 0
      val get_b : int = 1 |}]
;;

let input =
  {|
let a = object (self) val n = 0 method increase = {< n = n + 1>} method get = n end

let b = a#increase#increase#increase

let get_b = b#get
|}
;;

let%expect_test _ =
  let () = run_interpreter input in
  [%expect
    {|
    val a : < increase : 'a; get : int > as 'a = <obj>
    val b : < increase : 'a; get : int > as 'a = <obj>
    val get_b : int = 3 |}]
;;

let input =
  {|
let my_list =
  object (self)
    val t = []

    method get = t

    method add x = {<t = x :: t>}

    method rev =
      let rec helper acc list =
        match list with
        | [] -> acc
        | (h :: tl) -> helper (h :: acc) tl
      in
      let rev_list = helper [] t in
      {<t = rev_list>}

    method tail =
      match t with
      | [] -> {<t = []>}
      | (_ :: tl) -> {<t = tl>}

    method remove x =
      let rec helper acc list =
        match list with
        | [] -> acc
        | (h :: tl) -> if h = x then helper acc tl else helper (h :: acc) tl
      in
      {<t = helper [] t>}
  end

let l = ((my_list#add 5)#add 4)#add 6
let l_v = l#get

let rev_l = l#rev
let rev_v = rev_l#get

let tail_rev_l = rev_l#tail
let tail_v = tail_rev_l#get

let sl = ((l#add 9)#add 10)#add 2
let sl_v = sl#get

;;
|}
;;

let%expect_test _ =
  let () = run_interpreter input in
  [%expect
    {|
      val l : < get : int list; add : int -> 'a; rev : 'a; tail : 'a; remove : int -> 'a > as 'a = <obj>
      val l_v : int list = [6; 4; 5]
      val my_list : < get : 'b list; add : 'b -> 'a; rev : 'a; tail : 'a; remove : 'b -> 'a > as 'a = <obj>
      val rev_l : < get : int list; add : int -> 'a; rev : 'a; tail : 'a; remove : int -> 'a > as 'a = <obj>
      val rev_v : int list = [5; 4; 6]
      val sl : < get : int list; add : int -> 'a; rev : 'a; tail : 'a; remove : int -> 'a > as 'a = <obj>
      val sl_v : int list = [2; 10; 9; 6; 4; 5]
      val tail_rev_l : < get : int list; add : int -> 'a; rev : 'a; tail : 'a; remove : int -> 'a > as 'a = <obj>
      val tail_v : int list = [4; 6] |}]
;;

let%expect_test _ =
  let () = run_interpreter "let a = -(-(-1))" in
  [%expect {| val a : int = -1 |}]
;;

let%expect_test _ =
  let () = run_interpreter "let f a b c d = a b (c 5 (d + 1)) d" in
  [%expect
    {| val f : ('a -> 'b -> int -> 'c) -> 'a -> (int -> int -> 'b) -> int -> 'c = <fun> |}]
;;

let%expect_test _ =
  let () = run_interpreter "let f a b c = a(b+1)(not c)" in
  [%expect {| val f : (int -> bool -> 'a) -> int -> bool -> 'a = <fun> |}]
;;

let input =
  {|
let cmp_with_five o = o#get = 5

let num = object (self) val t = 0 method add x = {<t = t + x>} method get = t end

let l = object (self) val t = [] method add x = {<t = x::t>} method get = match t with [] -> 0 | (h::_) -> h method foo = 5 + 1 end

let eval = cmp_with_five num || (cmp_with_five l)
|}
;;

let%expect_test _ =
  let () = run_interpreter input in
  [%expect
    {|
    val cmp_with_five : < get : int; .. > -> bool = <fun>
    val eval : bool = false
    val l : < add : int -> 'a; get : int; foo : int > as 'a = <obj>
    val num : < add : int -> 'a; get : int > as 'a = <obj> |}]
;;

let input =
  {|
let a = object method id x = x end
let b = object (self) val t = 0 method increase = {<t = t + 1>} end
let c = a#id b;
|}
;;

let%expect_test _ =
  let () = run_interpreter input in
  [%expect
    {|
    val a : < id : 'a -> 'a > = <obj>
    val b : < increase : 'a > as 'a = <obj>
    val c : < increase : 'a > as 'a = <obj> |}]
;;

let%expect_test _ =
  let () =
    run_interpreter
      "let f a b c = c (let id x = x in let a = [1; 2; 3] in id a) (a (b (1, 2) + 7)) + 4"
  in
  [%expect
    {| val f : (int -> 'a) -> (int * int -> int) -> (int list -> 'a -> int) -> int = <fun> |}]
;;

let input =
  {|
let a x =
  match x with
  | (1, 1) -> true
  | (x, 0) ->
    (match x with
     | 0 -> true
     | _ -> false)
  | _ -> false
;;
|}
;;

let%expect_test _ =
  let () = run_interpreter input in
  [%expect {| val a : int * int -> bool = <fun> |}]
;;
