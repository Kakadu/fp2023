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
    index :: [p10] -> Int -> p10
    numbers_starting_at :: Int -> [Int]
    res :: Int |}]
;;

let%expect_test _ =
  parse_infer {|
    fix f = let x = f x in x

|};
  [%expect {| fix :: (p2 -> p3) -> p3 |}]
;;

let%expect_test _ =
  parse_infer {|
    fix f = f (fix f)

|};
  [%expect {| fix :: (p3 -> p3) -> p3 |}]
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
    take :: Int -> [p5] -> [p5] |}]
;;

let%expect_test _ =
  parse_infer
    {| merge (x:xs) (y:ys) = if x < y then x : merge xs (y:ys) else if x == y then x : merge xs ys else y : merge (x:xs) ys|};
  [%expect {|
    take :: Int -> [p5] -> [p5] |}]
;;
