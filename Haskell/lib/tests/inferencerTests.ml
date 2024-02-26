(** Copyright 2023-2024, Danil P *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open HaskellLib

let parse_infer input =
  match Parser.parse input with
  | Ok e -> Inferencer.infer e
  | Error err -> Format.printf "%s\n" err
;;

let%expect_test _ =
  parse_infer {|(x, y) = (5, 6)|};
  [%expect {|
    x :: Int
    y :: Int |}]
;;

let%expect_test _ =
  parse_infer
    {|
  lst = [1, 2, 6]
incr x = x + 1
map f (x:xs) = f x : map f xs
(x:xs) = map incr lst
|};
  [%expect
    {|
    incr :: Int -> Int
    lst :: [Int]
    map :: (p8 -> p11) -> [p8] -> [p11]
    x :: Int
    xs :: [Int] |}]
;;

let%expect_test _ =
  parse_infer {|y = \x -> [x]|};
  [%expect {| y :: p2 -> [p2] |}]
;;

let%expect_test _ =
  parse_infer {|y = \x -> [x, 5]|};
  [%expect {| y :: Int -> [Int] |}]
;;

let%expect_test _ =
  parse_infer {|z = \x y -> (x, y)|};
  [%expect {|
    z :: p1 -> p2 -> (p1, p2) |}]
;;

let%expect_test _ =
  parse_infer {| fact n = if (n < 2) then 1 else fact (n - 1) * n|};
  [%expect {| fact :: Int -> Int |}]
;;

let%expect_test _ =
  parse_infer {| dam x y = (x, y)|};
  [%expect {|
    dam :: p1 -> p2 -> (p1, p2) |}]
;;

let%expect_test _ =
  parse_infer {|   f a = 
  let x = 3
      y = 5
      z = 7
  in x + y + z + a|};
  [%expect {|
    f :: Int -> Int |}]
;;

let%expect_test _ =
  parse_infer {|real n = if True || False then 1 else 2|};
  [%expect {|
    real :: p1 -> Int |}]
;;

let%expect_test _ =
  parse_infer {| dam (x, y) = x + y|};
  [%expect {|
    dam :: (Int, Int) -> Int |}]
;;

let%expect_test _ =
  parse_infer {| dam (x:xs) = xs|};
  [%expect {|
    dam :: [p3] -> [p3] |}]
;;

let%expect_test _ =
  parse_infer {| dam (y:x:xs) = x + y|};
  [%expect {|
    dam :: [Int] -> Int |}]
;;

let%expect_test _ =
  parse_infer {| f = let g x y = x + y in g|};
  [%expect {|
    f :: Int -> Int -> Int |}]
;;

let%expect_test _ =
  parse_infer {|  f = let (x,y) = (1, 2) in x + y|};
  [%expect {|
    f :: Int |}]
;;

let%expect_test _ =
  parse_infer {|dam = let f x g = g x in let id x = x in let fst x y = x in fst (f id)|};
  [%expect {|
    dam :: p11 -> ((p14 -> p14) -> p13) -> p13 |}]
;;

let%expect_test _ =
  parse_infer {|dam = \f -> \x -> f x|};
  [%expect {|
    dam :: (p2 -> p3) -> p2 -> p3 |}]
;;

let%expect_test _ =
  parse_infer {|dam f  = let z = f 5 in \x -> []|};
  [%expect {|
    dam :: (Int -> p3) -> p4 -> [p5] |}]
;;

let%expect_test _ =
  parse_infer {| (a:b:c) = [3, 100] |};
  [%expect {|
    a :: Int
    b :: Int
    c :: [Int] |}]
;;

let%expect_test _ =
  parse_infer
    {|
    numbers_starting_at n = n : numbers_starting_at (n + 1)

    index (x:xs) n = if n == 0 then x else index xs (n - 1)
    
        res = index (numbers_starting_at 2) 20|};
  [%expect
    {|
    index :: [p6] -> Int -> p6
    numbers_starting_at :: Int -> [Int]
    res :: Int |}]
;;

let%expect_test _ =
  parse_infer {|   f a = 
  let x = x
      y = 5
      z = 7
  in x + y + z + a|};
  [%expect {|
    f :: Int -> Int |}]
;;

let%expect_test _ =
  parse_infer {|
    fix f = f (fix f)

|};
  [%expect {| fix :: (p3 -> p3) -> p3 |}]
;;

let%expect_test _ =
  parse_infer {| take n lst = case (n, lst) of
      (_, (x:xs)) -> x|};
  [%expect {|
    take :: p4 -> [p7] -> p7 |}]
;;

let%expect_test _ =
  parse_infer {| dam (_, (x:xs)) = x|};
  [%expect {|
    dam :: (p1, [p4]) -> p4 |}]
;;

let%expect_test _ =
  parse_infer {| take lst = case lst of
      (x:xs) -> x|};
  [%expect {|
    take :: [p5] -> p5 |}]
;;

let%expect_test _ =
  parse_infer
    {| take n lst = case (n, lst) of
        (_, []) -> []
      (1, (x:_)) -> [x]
      (n, (x:xs)) -> x : xs|};
  [%expect {|
    take :: Int -> [p14] -> [p14] |}]
;;

let%expect_test _ =
  parse_infer
    {| merge (x:xs) (y:ys) = if x < y then x : merge xs (y:ys) else if x == y then x : merge xs ys else y : merge (x:xs) ys|};
  [%expect {|
    merge :: [p7] -> [p7] -> [p7] |}]
;;

let%expect_test _ =
  parse_infer
    {| merge (x:xs) (y:ys) = if x < y then x : merge xs (y:ys) else if x == y then x : merge xs ys else y : merge (x:xs) ys
    mul x y = x * y
   
   map1 f (x:xs) = f x : map1 f xs

   map f lst =
     case lst of
       [] -> []
       (x:xs) -> f x : map f xs
 
   hamming = 1 : merge (map (mul 2) hamming) (merge (map (mul 3) hamming) (map (mul 5) hamming))|};
  [%expect
    {|
    hamming :: [Int]
    map :: (p36 -> p39) -> [p36] -> [p39]
    map1 :: (p24 -> p27) -> [p24] -> [p27]
    merge :: [p7] -> [p7] -> [p7]
    mul :: Int -> Int -> Int |}]
;;

let%expect_test _ =
  parse_infer {|dumb = 1 + "asd"|};
  [%expect
    {|
    This expression has type (String) but an expression was expected of type (Int) |}]
;;

let%expect_test _ =
  parse_infer {|dumb = asd|};
  [%expect {|
    Variable not in scope: asd |}]
;;

let%expect_test _ =
  parse_infer {|(x, y, z) = ('c', 'e', 'r', 'f')|};
  [%expect {|
    x :: p0
    y :: p1
    z :: p2 |}]
;;

let%expect_test _ =
  parse_infer {|fix f = let x = f x in x|};
  [%expect {|
    fix :: (p3 -> p3) -> p3 |}]
;;

let%expect_test _ =
  parse_infer
    {|is_prime n =
        let is_divisible_by d = if d * d > n then False else (if (n - ((n / d) * d)) == 0 then True else is_divisible_by (d + 1))
        in
        (if n <= 1 then False else is_divisible_by 2)|};
  [%expect {|
    is_prime :: Int -> Bool |}]
;;

let%expect_test _ =
  parse_infer {|x = (Node 1 (Node 3 Leaf Leaf) Leaf)|};
  [%expect {|
    x :: 🌳 of Int |}]
;;

let%expect_test _ =
  parse_infer {|x = (Node 1 (Node \x -> x + 1 Leaf Leaf) Leaf)|};
  [%expect
    {|
    This expression has type (Int) but an expression was expected of type (Int -> Int) |}]
;;

let%expect_test _ =
  parse_infer {|x = Leaf|};
  [%expect {|
    x :: 🌳 of p1 |}]
;;

let%expect_test _ =
  parse_infer {|f (Node x  (Node 1  Leaf Leaf) Leaf) = Leaf|};
  [%expect {|
    f :: 🌳 of Int -> 🌳 of p7 |}]
;;
