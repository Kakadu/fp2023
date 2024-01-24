  $ ./demoInterpret.exe << EOF
  > let rec factorial_recursive = fun n -> if n <= 1 then 1 else n * factorial_recursive (n - 1)
  > let a = factorial_recursive 5
  > EOF
  "a": int = 120
  "factorial_recursive": int -> int = <rec fun>
  $ ./demoInterpret.exe << EOF
  > let rec fix = fun f -> (fun x -> f (fix f) x)
  > let fac = fix (fun self -> (fun n -> if n <= 1 then 1 else n * self (n - 1)))
  > let a = fac 6
  > EOF
  "a": int = 720
  "fac": int -> int = <fun>
  "fix": '2 '3 . (('2 -> '3) -> '2 -> '3) -> '2 -> '3 = <rec fun>
  $ ./demoInterpret.exe << EOF
  > type me = | Tepa of int
  > 
  > let a = fun x -> match x with | Tepa 46 -> true | _ -> false
  > let n = a (Tepa 46)
  > EOF
  type me = | Tepa of int
  "a": me -> bool = <fun>
  "n": bool = true
  $ ./demoInterpret.exe << EOF
  > type tree = | Tree of tree * tree | Num of int
  > 
  > let a = fun x -> match x with | Tree (_, _) -> true | _ -> false
  > let n = a (Tree (Tree (Tree (Num 2, Num 3), Num 2), Tree(Num 2, Num 3)))
  > EOF
  type tree = | Tree of tree * tree | Num of int
  "a": tree -> bool = <fun>
  "n": bool = true
  $ ./demoInterpret.exe << EOF
  > let a = fun x -> match x with | [] -> false | h :: tl -> true
  > let n = a (44 :: 45 :: 56 :: [])
  > EOF
  "a": '2 . '2 list -> bool = <fun>
  "n": bool = true
  $ ./demoInterpret.exe << EOF
  > let a = fun x -> match x with | [] -> false | h :: tl -> true
  > let n = a []
  > EOF
  "a": '2 . '2 list -> bool = <fun>
  "n": bool = false
  $ ./demoInterpret.exe << EOF
  > let x = (false || true) && true
  > EOF
  "x": bool = true
  $ ./demoInterpret.exe << EOF
  > let h = fun h :: tl -> h
  > let tl = fun h :: tl -> tl
  > let n = h (4 :: 5 :: 6 :: [])
  > let m = tl (4 :: 5 :: 6 :: [])
  > EOF
  "h": '0 . '0 list -> '0 = <fun>
  "m": int list = [5; 6]
  "n": int = 4
  "tl": '2 . '2 list -> '2 list = <fun>
  $ ./demoInterpret.exe << EOF
  > let sum = fun (a, b) -> a + b
  > let sub = fun (a, b) -> a - b
  > let mul = fun (a, b) -> a * b
  > let n = sum (4, 5)
  > let m = sub (5, 6)
  > let k = mul (5, 8)
  > EOF
  "k": int = 40
  "m": int = -1
  "mul": int * int -> int = <fun>
  "n": int = 9
  "sub": int * int -> int = <fun>
  "sum": int * int -> int = <fun>
  $ ./demoInterpret.exe << EOF
  > let n = fun y -> (let x = 5 in x + y)
  > let f = n 7
  > EOF
  "f": int = 12
  "n": int -> int = <fun>
  $ ./demoInterpret.exe << EOF
  > let rev = fun lst ->
  >   (let rec helper = fun acc -> (fun lst ->
  >     match lst with
  >     | [] -> acc
  >     | h :: tl -> helper (h :: acc) tl)
  >   in
  >   helper [] lst)
  > 
  > let a = rev (true :: false :: [])
  > let b = rev ("str1" :: "str2" :: [])
  > let c = rev (52 :: 51 :: [])
  "a": bool list = [false; true]
  "b": string list = ["str2"; "str1"]
  "c": int list = [51; 52]
  "rev": '14 . '14 list -> '14 list = <fun>
  $ ./demoInterpret.exe << EOF
  > type node = | Red of int | Black of int
  > 
  > let is_black = fun x -> match x with | Black _ -> true | Red _ -> false
  > let a = is_black (Black 52)
  > let b = is_black (Red 52)
  > EOF
  type node = | Red of int | Black of int
  "a": bool = true
  "b": bool = false
  "is_black": node -> bool = <fun>
  $ ./demoInterpret.exe << EOF
  > type color = | Red | Black
  > 
  > type rbtree =
  > | Empty
  > | Node of color * int * rbtree * rbtree
  > 
  > let rec member = fun x -> 
  > (fun n -> 
  >  match n with 
  >  | Empty -> false
  >  | Node (_, y, left, right) -> if x = y then true else if x < y then member x left else member x right)
  > 
  > let node_left_left = Node(Black, 3, Empty, Empty)
  > 
  > let node_left = Node (Red, 4, node_left_left, Empty)
  > 
  > let node_right = Node(Red, 10, Empty, Empty) 
  > 
  > let node = Node (Black, 5, node_left, node_right)
  > 
  > let is_member = member 4 node
  > 
  > let is_member2 = member 52 node
  type color = | Red | Black
  type rbtree = | Node of color * int * rbtree * rbtree | Empty
  "is_member": bool = true
  "is_member2": bool = false
  "member": int -> rbtree -> bool = <rec fun>
  "node": rbtree = Node (Black, 5, Node (Red, 4, Node (Black, 3, Empty, Empty), Empty), Node (Red, 10, Empty, Empty))
  "node_left": rbtree = Node (Red, 4, Node (Black, 3, Empty, Empty), Empty)
  "node_left_left": rbtree = Node (Black, 3, Empty, Empty)
  "node_right": rbtree = Node (Red, 10, Empty, Empty)
THE TEST CHECKS THE COMPILER ERROR OUTPUT
  $ ./demoInterpret.exe << EOF
  > let rec fix f x = f (fix f) x
  > let map f p = let (a,b) = p in (f a, f b)
  > let fixpoly l =
  >   fix (fun self l -> map (fun li x -> li (self l) x) l) l
  > let feven p n =
  >   let (e, o) = p in
  >   if n = 0 then 1 else o (n - 1)
  > let fodd p n =
  >   let (e, o) = p in
  >   if n = 0 then 0 else e (n - 1)
  > let tie = fixpoly (feven, fodd)
  > let rezult =
  >   let (even,odd) = tie in
  >   (odd 1)
  > EOF
  Parsing error: : end_of_input
  $ ./demoInterpret.exe << EOF
  > let rec fix = fun f -> (fun x -> f (fix f) x)
  > let map = fun f -> (fun (a, b) -> (f a, f b))
  > let fixpoly = fun l ->
  >   fix (fun self -> (fun l -> map (fun li -> (fun x -> li (self l) x)) l)) l
  > let feven = fun (e, o) -> (fun n ->
  >   if n = 0 then 1 else o (n - 1))
  > let fodd = fun (e, o) -> (fun n ->
  >   if n = 0 then 0 else e (n - 1))
  > let tie = fixpoly (feven, fodd)
  > let helper = fun (even, odd) -> (odd 1)
  > let rezult = helper tie
  > EOF
  "feven": '27 . '27 * (int -> int) -> int -> int = <fun>
  "fix": '2 '3 . (('2 -> '3) -> '2 -> '3) -> '2 -> '3 = <rec fun>
  "fixpoly": '23 '24 . (('23 -> '24) * ('23 -> '24) -> '23 -> '24) * (('23 -> '24) * ('23 -> '24) -> '23 -> '24) -> ('23 -> '24) * ('23 -> '24) = <fun>
  "fodd": '33 . (int -> int) * '33 -> int -> int = <fun>
  "helper": '42 '44 . '42 * (int -> '44) -> '44 = <fun>
  "map": '7 '9 . ('7 -> '9) -> '7 * '7 -> '9 * '9 = <fun>
  "rezult": int = 1
  "tie": (int -> int) * (int -> int) = (<fun>, <fun>)
  $ ./demoInterpret.exe << EOF
  > let eq = fun a -> (fun b -> a = b)
  > let answ = eq (1 :: []) (1 :: [])
  "answ": bool = true
  "eq": '2 . '2 -> '2 -> bool = <fun>
  $ ./demoInterpret.exe << EOF
  > let eq = fun a -> (fun b -> a = b)
  > let answ = eq (1 :: 2 :: []) (1 :: 4 :: [])
  "answ": bool = false
  "eq": '2 . '2 -> '2 -> bool = <fun>
  $ ./demoInterpret.exe << EOF
  > type a = | First of int * string | Second of string * int
  > let eq = fun a -> (fun b -> a = b)
  > let answ1 = eq (First(5, "5")) (First(5, "5"))
  > let answ2 = eq (First(4, "5")) (First(5, "5"))
  > let answ3 = eq (First(4, "5")) (Second("5", 5))
  type a = | Second of string * int | First of int * string
  "answ1": bool = true
  "answ2": bool = false
  "answ3": bool = false
  "eq": '2 . '2 -> '2 -> bool = <fun>
THE TEST CHECKS THE INTERPRETER ERROR OF REC FUN WITHOUT ARGS
  $ ./demoInterpret.exe << EOF
  > type list = | Nil | Cons of list
  > let rec x = Cons x
  > EOF
  UnboundVariable: "x"
