  $ cat << EOF | dune exec demo - 
  > let f x = x x 
  Typecheck error: Occurs check failed
  $ cat << EOF | dune exec demo - 
  > let o = privet 
  Typecheck error: Undefined variable 'privet'
  $ cat << EOF | dune exec demo -
  > let x = 10
  > let y = 12
  > let z = 15
  > let lists = [x; y; z]
  > let mymatch x =
  >      match x with
  >        | hd :: tl -> tl
  >        | _ -> 0
  > let a = mymatch lists
  Typecheck error: Unification fail on int and '2 list
  $ cat << EOF | dune exec demo -
  > let rec fibonacci n =
  > match n with
  > | 0 -> 0
  > | 1 -> 1
  > | _ -> fibonacci (n - 1) + fibonacci (n - 2)
  > let result = fibonacci 10
  val fibonacci : int -> int = <fun>
  val result : int = 55
  $ cat << EOF | dune exec demo -
  > let bin a = a * a * a
  > let res = bin 12
  val bin : int -> int = <fun>
  val res : int = 1728
  $ cat << EOF | dune exec demo -
  > let rec fact = fun x -> if x = 1 then 1 else x * fact (x - 1)
  > let a = fact 10
  val a : int = 3628800
  val fact : int -> int = <fun>
  $ cat << EOF | dune exec demo -
  > let fst = [1; 3; 5; 7; 9]
  > let f l = match l with
  > | h :: tl -> tl
  > | _ -> []
  > let a =  f fst
  > let func = fun x -> match x with
  > | h :: tl -> h + 10
  > | _ -> 0
  > let b = func a
  > let fff x y = x && y
  > let c = fff true true
  val a : int list = [3; 5; 7; 9]
  val b : int = 13
  val c : bool = true
  val f : '6 list -> '6 list = <fun>
  val fff : '16 -> '16 -> bool = <fun>
  val fst : int list = [1; 3; 5; 7; 9]
  val func : int list -> int = <fun>
  $ cat << EOF | dune exec demo -
  > let addn n = let adder m = m + n in adder
  > let a  = addn 5
  > let b = a 6
  val a : int -> int = <fun>
  val addn : int -> int -> int = <fun>
  val b : int = 11
  $ cat << EOF | dune exec demo -
  > let a = 1
  > let b = [2; 3]
  > let lst = a :: b
  > let tupl = (1, 2, 3)
  val a : int = 1
  val b : int list = [2; 3]
  val lst : int list = [1; 2; 3]
  val tupl : (int * int * int) = (1, 2, 3)
