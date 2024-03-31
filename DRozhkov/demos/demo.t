  $ cat << EOF | dune exec demo - 
  > let f x = x x 
  Typecheck error: Occurs check failed
  $ cat << EOF | dune exec demo - 
  > let o = privet let a = 5 let a = 4 end
  Typecheck error: Undefined variable 'privet'
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
  > let x = 10
  > let y = 12
  > let z = 15
  > let lists = [x; y; z]
  > let mymatch x =
  >      match x with
  >        | hd :: tl -> tl
  >        | _ -> 0
  > let a = mymatch lists
  val a : int = [12; 15]
  val lists : int list = [10; 12; 15]
  val mymatch : int list -> int = <fun>
  val x : int = 10
  val y : int = 12
  val z : int = 15
