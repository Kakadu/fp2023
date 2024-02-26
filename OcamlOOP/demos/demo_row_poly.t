  $ cat << EOF | dune exec demo -
  > let f x = x#method_a + 2
  > 
  > let g x = x#method_b
  > 
  > let h x = f x + (if g x then 3 else f x)
  > EOF
  val f : < method_a : int; .. > -> int = <fun>
  val g : < method_b : 'a; .. > -> 'a = <fun>
  val h : < method_a : int; method_b : bool; .. > -> int = <fun>

  $ cat << EOF | dune exec demo - 
  > let f x = x#get
  > 
  > let a = object (self) val t = 52 method get = t method increase = {< t = t + 1 >} end
  > 
  > let b = object (self) val n = 21 method get = n method decrease = {< n = n - 1>} end 
  > 
  > let get_a = f a
  > let get_b = f b
  > EOF
  val a : < get : int; increase : 'a > as 'a = <obj>
  val b : < get : int; decrease : 'a > as 'a = <obj>
  val f : < get : 'a; .. > -> 'a = <fun>
  val get_a : int = 52
  val get_b : int = 21

