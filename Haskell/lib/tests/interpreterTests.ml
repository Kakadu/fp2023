(** Copyright 2023-2024, Danil P *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(* Here infinite lists are defined as functions with wildcard pattern
   to avoid printing it out as an infinite list (otherwise we are stuck in a loop) *)

open HaskellLib
open Interpreter

let parse_interpret input =
  match Parser.parse input with
  | Ok e -> Interpret.interpret e
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
  parse_interpret {|(x:xs)= [52]|};
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
  parse_interpret
    {|  lst = [1, 2, 6]
incr x = x + 1
map f lst = case lst of
[] -> []
(x:xs) -> f x : map f xs  
new_lst = map incr lst|};
  [%expect
    {|
    incr => <fun>
    lst => [1, 2, 6]
    map => <fun>
    new_lst => [2, 3, 7] |}]
;;

let%expect_test _ =
  parse_interpret {|  lst = [1, 2, 6]
    a = 1 / 0 
    x = a + 5 
    b = [1, 2]|};
  [%expect
    {|
    a => DivisionByZeroError
    b => [1, 2]
    lst => [1, 2, 6]
    x => DivisionByZeroError |}]
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

(* zipWith f = go
   where
   go [] _ = []
   go _ [] = []
   go (x:xs) (y:ys) = f x y : go xs ys*)

(* fibs = 0 : 1 : fibs *)

let%expect_test _ =
  parse_interpret
    {|
  zipWith f lst1 lst2 = case (lst1, lst2) of
    ((x:xs), (y:ys)) -> f x y : zipWith f xs ys
  
  index (x:xs) n = if n == 0 then x else index xs (n - 1)

  add x y = x + y

  tail (_:xs) = xs  
  
  fibs _ = 0 : 1 : zipWith add (fibs 0) (tail (fibs 0))

  fib = index (fibs 0) 20|};
  [%expect
    {|
    tail => <fun>
    fib => 6765
    fibs => <fun>
    zipWith => <fun>
    add => <fun>
    index => <fun> |}]
;;

(* tfw no guards *)
let%expect_test _ =
  parse_interpret
    {|
    merge (x:xs) (y:ys) = if x < y then x : merge xs (y:ys) else if x == y then x : merge xs ys else y : merge (x:xs) ys
    mapMul n lst = 
      case lst of
        [] -> []
        (x:xs) -> n * x : mapMul n xs  
    hamming _ = 1 : merge (mapMul 2 (hamming 0)) (merge (mapMul 3 (hamming 0)) (mapMul 5 (hamming 0)))

    take n lst = 
      case (n, lst) of
        (_, []) -> []
        (1, (x:_)) -> [x]
        (n, (x:xs)) -> x : take (n - 1) xs
    res = take 20 (hamming 0)|};
  [%expect
    {|
    merge => <fun>
    hamming => <fun>
    res => [1, 2, 3, 4, 5, 6, 8, 9, 10, 12, 15, 16, 18, 20, 24, 25, 27, 30, 32, 36]
    take => <fun>
    mapMul => <fun> |}]
;;

let%expect_test _ =
  parse_interpret
    {|
    s f g x = f x (g x)
    k x y   = x
    b f g x = f (g x)
    c f g x = f x g
    y f     = f (y f)

    cond p f g x = if p x then f x else g x
    equal a b = (a == b)
    mul a b = a * b
    pred a = a - 1

    fac  = y (b (cond (equal 0) (k 1)) (b (s mul) (c b pred)))

    res = fac 8
|};
  [%expect
    {|
    mul => <fun>
    s => <fun>
    res => 40320
    fac => <fun>
    b => <fun>
    y => <fun>
    k => <fun>
    equal => <fun>
    c => <fun>
    pred => <fun>
    cond => <fun> |}]
;;

let%expect_test _ =
  parse_interpret
    {|
    numbers_starting_at n = n : numbers_starting_at (n + 1)  

    take n lst = case (n, lst) of
      (_, []) -> []
      (1, (x:_)) -> [x]
      (n, (x:xs)) -> x : take (n - 1) xs
      

    mod x y = (x - (y * (x / y)))

    filter predicate xs = case xs of
      [] -> []
      (x:rest) -> (if predicate x
                    then x : filter predicate rest
                    else filter predicate rest)
                
    sieve asd = case asd of
      [] -> []
      (p:xs) -> p : sieve (filter (\x -> mod x p /= 0) xs)
        
    index (x:xs) n = if n == 0 then x else index xs (n - 1)
  
    res = take 12 (sieve (numbers_starting_at 2))|};
  [%expect
    {|
    res => [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37]
    sieve => <fun>
    take => <fun>
    numbers_starting_at => <fun>
    mod => <fun>
    index => <fun>
    filter => <fun> |}]
;;

let%expect_test _ =
  parse_interpret {|  f = 
   let x = 3
       y = x
       z = 7
   in x + y + z
|};
  [%expect {|
    f => 13
    x => 3 |}]
;;

let%expect_test _ =
  parse_interpret
    {|
    numbers_starting_at n = n : numbers_starting_at (n + 1)  

    take n lst = case (n, lst) of
      (_, []) -> []
      (1, (x:_)) -> [x]
      (n, (x:xs)) -> x : take (n - 1) xs
      

    mod x y = (x - (y * (x / y)))

    filter predicate xs = case xs of
      [] -> []
      (x:rest) -> (if predicate x
                    then x : filter predicate rest
                    else filter predicate rest)
  primes n = 
    let sieve asd = 
      case asd of
        [] -> []
        (p:xs) -> p : sieve (filter (\x -> mod x p /= 0) xs)
    in
    sieve (numbers_starting_at n)
        
    index (x:xs) n = if n == 0 then x else index xs (n - 1)
  
    res = take 10 (primes 2)|};
  [%expect
    {|
    res => [2, 3, 5, 7, 11, 13, 17, 19, 23, 29]
    take => <fun>
    numbers_starting_at => <fun>
    mod => <fun>
    index => <fun>
    filter => <fun>
    primes => <fun> |}]
;;
