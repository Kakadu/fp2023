(** Copyright 2023-2024, Danil P *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open HaskellLib
open Interpreter

let parse_interpret input =
  match Parser.parse input with
  | Ok e -> Eval.interpret e
  | Error err -> Format.printf "%s\n" err
;;

let%expect_test _ =
  parse_interpret {|fact n = if n < 2 then 1 else fact (n - 1) * n
  x = fact 3|};
  [%expect {|
    fact => <fun>
    x => 6 |}]
;;

let%expect_test _ =
  parse_interpret {|f a b = b + a
  x = f 5
  y = x 6|};
  [%expect {|
  f => <fun>
  x => <fun>
  y => 11 |}]
;;

let%expect_test _ =
  parse_interpret {|x = (Node 5 Leaf Leaf)|};
  [%expect {|
  x =>
  5 |}]
;;

let%expect_test _ =
  parse_interpret
    {|shared_node = (Node "hello" (Node "world" Leaf Leaf) (Node "you" Leaf Leaf))|};
  [%expect {|
  shared_node =>
  "hello"
  ├── "world"
  └── "you" |}]
;;

let%expect_test _ =
  parse_interpret
    {|shared_node = (Node "hello" (Node "world" Leaf Leaf) (Node "you" Leaf Leaf))
    tree = (Node "root" (Node "Mr. Poopypants" (Node "something something" shared_node Leaf) (Node "Ms. Poopypants" Leaf Leaf)) shared_node)|};
  [%expect
    {|
  shared_node =>
  "hello"
  ├── "world"
  └── "you"

  tree =>
  "root"
  ├── "Mr. Poopypants"
  │   ├── "something something"
  │   │   └── "hello"
  │   │       ├── "world"
  │   │       └── "you"
  │   └── "Ms. Poopypants"
  └── "hello"
      ├── "world"
      └── "you" |}]
;;

let%expect_test _ =
  parse_interpret
    {|shared_node = (Node 3 (Node 2 Leaf Leaf) (Node 10 Leaf Leaf))
    tree = (Node 6 (Node 5 (Node 0 shared_node Leaf) (Node 7 Leaf Leaf)) shared_node)
    
    increment tree = case tree of
      Leaf -> Leaf
      (Node v l r) -> Node (v * 10) (increment l) (increment r)
    res = increment (tree)|};
  [%expect
    {|
    increment => <fun>
    res =>
    60
    ├── 50
    │   ├── 0
    │   │   └── 30
    │   │       ├── 20
    │   │       └── 100
    │   └── 70
    └── 30
        ├── 20
        └── 100

    shared_node =>
    3
    ├── 2
    └── 10

    tree =>
    6
    ├── 5
    │   ├── 0
    │   │   └── 3
    │   │       ├── 2
    │   │       └── 10
    │   └── 7
    └── 3
        ├── 2
        └── 10 |}]
;;

let%expect_test _ =
  parse_interpret
    {|shared_node = (Node 3 (Node 2 Leaf Leaf) (Node 10 Leaf Leaf))
    tree = (Node 6 (Node 5 (Node 0 shared_node Leaf) (Node 7 Leaf Leaf)) shared_node)
    
    min x y = if x <= y then x else y


   min_of_tree minim tree = case tree of
     Leaf -> minim
     (Node v l r) -> min v (min (min_of_tree minim l) (min_of_tree minim r))
  
  tree_of_min tree =
     let replace_values value tree = case tree of 
       Leaf -> Leaf
       (Node v l r) -> Node value (replace_values value l) (replace_values value r)
     in 
     replace_values (min_of_tree 1000000 tree) tree
  res = tree_of_min tree|};
  [%expect
    {|
    min => <fun>
    min_of_tree => <fun>
    res =>
    0
    ├── 0
    │   ├── 0
    │   │   └── 0
    │   │       ├── 0
    │   │       └── 0
    │   └── 0
    └── 0
        ├── 0
        └── 0

    shared_node =>
    3
    ├── 2
    └── 10

    tree =>
    6
    ├── 5
    │   ├── 0
    │   │   └── 3
    │   │       ├── 2
    │   │       └── 10
    │   └── 7
    └── 3
        ├── 2
        └── 10

    tree_of_min => <fun> |}]
;;
