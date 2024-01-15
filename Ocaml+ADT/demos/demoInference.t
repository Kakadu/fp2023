  $ dune exec demoInference << EOF
  > let n = fun (a, b) -> a + 1
  "n": '1 . int * '1 -> int
  $ dune exec demoInference << EOF
  > let n = fun a :: b -> a 
  > let b = n (46 :: 52 :: [])
  "b": int
  "n": '0 . '0 list -> '0
  $ dune exec demoInference << EOF
  > let rec factorial_recursive = fun n -> if n <= 1 then 1 else n * factorial_recursive (n - 1)
  > let a = factorial_recursive 5
  > let b = factorial_recursive 6
  "a": int
  "b": int
  "factorial_recursive": int -> int
  $ dune exec demoInference << EOF
  > let rec fix = fun f -> (fun x -> f (fix f) x)
  > let fac = fix (fun self -> (fun n -> if n <= 1 then 1 else n * self (n - 1)))
  > let a = fac 5
  "a": int
  "fac": int -> int
  "fix": '2 '3 . (('2 -> '3) -> '2 -> '3) -> '2 -> '3
  $ dune exec demoInference << EOF
  > let rev = fun lst ->
  >   (let rec helper = fun acc -> (fun lst ->
  >     match lst with
  >     | [] -> acc
  >     | h :: tl -> helper (h :: acc) tl)
  >   in
  >   helper [] lst)
  > let a = rev (true :: false :: [])
  > let b = rev ("str1" :: "str2" :: [])
  > let c = rev (52 :: 52 :: [])
  "a": bool list
  "b": string list
  "c": int list
  "rev": '14 . '14 list -> '14 list
  $ dune exec demoInference << EOF
  > type name = | LName of string | RName of string
  > let a = LName "tepa"
  type name = | RName of string | LName of string
  "a": name
  $ dune exec demoInference << EOF
  > type color = | White | Black 
  > let a = White
  type color = | White | Black
  "a": color
  $ dune exec demoInference << EOF
  > type typ1 = | Num1 of int | Num2 of int
  > 
  > type typ2 = | Num of typ1 | Str of string 
  > 
  > let a = fun n -> Num n
  > let b = fun s -> Str s
  > let c = a (Num1 5)
  > let d = b "a"
  > let check = fun n -> match n with | Num (Num1 n) -> n | _ -> 5
  > 
  > let id = fun n -> n
  > let e = id (Num (Num1 5))
  > let f = check e
  type typ1 = | Num2 of int | Num1 of int
  type typ2 = | Str of string | Num of typ1
  "a": typ1 -> typ2
  "b": string -> typ2
  "c": typ2
  "check": typ2 -> int
  "d": typ2
  "e": typ2
  "f": int
  "id": '8 . '8 -> '8
  $ dune exec demoInference << EOF
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
  >  | Node (_, y, left, right) -> if x == y then true else if x < y then member x left else member x right)
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
  "is_member": bool
  "is_member2": bool
  "member": int -> rbtree -> bool
  "node": rbtree
  "node_left": rbtree
  "node_left_left": rbtree
  "node_right": rbtree
