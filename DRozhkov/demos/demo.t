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
  > | h :: [] -> []
  > | h :: tl -> tl
  > | _ -> []
  > let a =  f fst
  > let func = fun x -> match x with
  > | h :: tl -> h + 10
  > | _ -> 0
  > let b = func a
  val a : int list = [3; 5; 7; 9]
  val b : int = 13
  val f : '9 list -> '9 list = <fun>
  val fst : int list = [1; 3; 5; 7; 9]
  val func : int list -> int = <fun>
