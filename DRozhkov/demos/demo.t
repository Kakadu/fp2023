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
  Typecheck error: Unification fail on int and '3 list
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
