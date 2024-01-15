(** Copyright 2023-2024, Danil P *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(* Here infinite lists are defined as functions with wildcard pattern
   to avoid printing it out as an infinite list (otherwise we are stuck in a loop) *)

open HaskellLib
open Interpreter

let parse_interpret input =
  match Parser.parse input with
  | Ok e -> Eval.interpret e
  | Error err -> Format.printf "%s\n" err
;;

let%expect_test _ =
  parse_interpret {|fact n = if n < 2 then 1 else fact (n - 1) * n
  x = fact 7|};
  [%expect {|
    fact => <fun>
    x => 5040 |}]
;;

let%expect_test _ =
  parse_interpret {|  lst1 = [1, 2, 6]
    lst2 = [1, 2, 5]
    res = lst1 > lst2|};
  [%expect {|
    res => true
    lst2 => [1, 2, 5]
    lst1 => [1, 2, 6] |}]
;;

let%expect_test _ =
  parse_interpret {|x = 'a'|};
  [%expect {|
    x => 'a' |}]
;;

let%expect_test _ =
  parse_interpret {|(x:xs) = [52]|};
  [%expect {|
    xs => []
    x => 52 |}]
;;

let%expect_test _ =
  parse_interpret {|dam x = case x of
  [] -> 2
(x:xs) -> 1 + 1
y = dam []|};
  [%expect {|
    dam => <fun>
    y => 2 |}]
;;

let%expect_test _ =
  parse_interpret {|x = "asasdd" == "asd"
  y = 'a' >= 'b'
  z = 5 < 6 |};
  [%expect {|
    z => true
    y => false
    x => false |}]
;;

(* due to lazy evalution we don't propagate this error;
   this is a byproduct of printing it for test. *)
let%expect_test _ =
  parse_interpret {|  lst = [1, 2, 6]
    a = 1 / 0 
    x = a + 5 
    b = [1, 2]|};
  [%expect {|
    a => Infinity
    b => [1, 2]
    lst => [1, 2, 6]
    x => Infinity |}]
;;

let%expect_test _ =
  parse_interpret {|(x, y) = ('g', 2)|};
  [%expect {|
    y => 2
    x => 'g' |}]
;;

let%expect_test _ =
  parse_interpret
    {|
numbers_starting_at n = n : numbers_starting_at (n + 1)  

index (x:xs) n = if n == 0 then x else index xs (n - 1)

res = index (numbers_starting_at 2) 20|};
  [%expect {|
    res => 22
    numbers_starting_at => <fun>
    index => <fun> |}]
;;

let%expect_test _ =
  parse_interpret
    {|
take n lst = case (n, lst) of
  (n, (x:xs)) -> x : take (n - 1) xs
  (1, (x:_)) -> [x]
  (_, []) -> []
fib a b = a : fib b (a + b)

index (x:xs) n = if n == 0 then x else index xs (n - 1)

res = index (fib 0 1) 20
|};
  [%expect {|
    fib => <fun>
    res => 6765
    take => <fun>
    index => <fun> |}]
;;

let%expect_test _ =
  parse_interpret {|x = -5
  y = x + 5|};
  [%expect {|
    y => 0
    x => -5 |}]
;;

let%expect_test _ =
  parse_interpret {|(x, y) = (1, 2, 3)
  y = 5 + 5|};
  [%expect {|
    Non-exhausitve patterns in tuple |}]
;;
