  $ dune exec demoInterpret << EOF
  > let rec factorial_recursive = fun n -> if n <= 1 then 1 else n * factorial_recursive (n - 1)
  > let a = factorial_recursive 5
  > EOF
  "a": 120
  "factorial_recursive": <let rec>
  $ dune exec demoInterpret << EOF
  > let rec fix = fun f -> (fun x -> f (fix f) x)
  > let fac = fix (fun self -> (fun n -> if n <= 1 then 1 else n * self (n - 1)))
  > let a = fac 6
  > EOF
  "a": 720
  "fac": <fun>
  "fix": <let rec>
  $ dune exec demoInterpret << EOF
  > let a = fun x -> match x with | Tepa 46 -> true | _ -> false
  > let n = a (Tepa 46)
  > EOF
  "a": <fun>
  "n": true
  $ dune exec demoInterpret << EOF
  > let a = fun x -> match x with | Tree (_, _) -> true | _ -> false
  > let n = a (Tree (Tree (Tree (2, 3), 2), Tree(2, 3)))
  > EOF
  "a": <fun>
  "n": true
  $ dune exec demoInterpret << EOF
  > let a = fun x -> match x with | Tree (Tree (_, 1), _) -> true | _ -> false
  > let n = a (Tree (Tree (Tree (2, 3), 2), Tree(2, 3)))
  > EOF
  "a": <fun>
  "n": false
  $ dune exec demoInterpret << EOF
  > let a = fun x -> match x with | [] -> false | h :: tl -> true
  > let n = a (44 :: 45 :: 56)
  > EOF
  "a": <fun>
  "n": true
  $ dune exec demoInterpret << EOF
  > let a = fun x -> match x with | [] -> false | h :: tl -> true
  > let n = a []
  > EOF
  "a": <fun>
  "n": false
  $ dune exec demoInterpret << EOF
  > let x : bool = (false || true) && true
  > EOF
  "x": true
  $ dune exec demoInterpret << EOF
  > let h = fun h :: tl -> h
  > let tl = fun h :: tl -> tl
  > let n = h (4 :: 5 :: 6)
  > let m = tl (4 :: 5 :: 6)
  > EOF
  "h": <fun>
  "m": [5; 6]
  "n": 4
  "tl": <fun>
  $ dune exec demoInterpret << EOF
  > let sum = fun (a, b) -> a + b
  > let sub = fun (a, b) -> a - b
  > let mul = fun (a, b) -> a * b
  > let n = sum (4, 5)
  > let m = sub (5, 6)
  > let k = mul (5, 8)
  > EOF
  "k": 40
  "m": -1
  "mul": <fun>
  "n": 9
  "sub": <fun>
  "sum": <fun>
  $ dune exec demoInterpret << EOF
  > let n = fun y -> (let x = 5 in x + y)
  > let f = n 7
  > EOF
  "f": 12
  "n": <fun>
