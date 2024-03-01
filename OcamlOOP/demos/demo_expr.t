  $ cat << EOF | dune exec demo -
  > let eval = 5 + 6 / 2 * 3 + 1
  > 
  > let tuple x = if x > 8 then (x + 1, x - 1) else (x * 2, x / 2)
  > 
  > let (a, b) = tuple 7
  > 
  > let fold_left l = 
  >   let rec helper acc l = 
  >     match l with 
  >       | [] -> acc
  >       | (h :: tl) -> helper (acc + h) tl 
  >   in
  >   helper 0 l 
  > 
  > let sum = fold_left ([1; 4; 2; 6; 3; -1; 7; -3])
  > EOF
  val a : int = 14
  val b : int = 3
  val eval : int = 15
  val fold_left : int list -> int = <fun>
  val sum : int = 19
  val tuple : int -> int * int = <fun>
  $ cat << EOF | dune exec demo - 
  > let fun_tuple x y = (fun x -> x y), (fun y -> y x)
  > 
  > let (c :: d) = [3; 34; 4]
  > 
  > let is_eight x = match x with 8 -> true | _ -> false
  > 
  > let a = is_eight 7
  > 
  > let f g a b = g a b
  > EOF
  val a : bool = false
  val c : int = 3
  val d : int list = [34; 4]
  val f : ('a -> 'b -> 'c) -> 'a -> 'b -> 'c = <fun>
  val fun_tuple : 'a -> 'b -> (('b -> 'c) -> 'c) * (('a -> 'd) -> 'd) = <fun>
  val is_eight : int -> bool = <fun>
  $ cat << EOF | dune exec demo - 
  > let compose f g = fun x -> f (g x)
  > 
  > let pair x = (x, x)
  > 
  > let eval = compose pair (compose pair pair)
  > EOF
  val compose : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b = <fun>
  val eval : 'a -> (('a * 'a) * ('a * 'a)) * (('a * 'a) * ('a * 'a)) = <fun>
  val pair : 'a -> 'a * 'a = <fun>
  $ cat << EOF | dune exec demo - 
  > let cmp = (object val a = 4 method b = 8 end) = (object val a = 4 method b = 8 end)
  > EOF
  val cmp : bool = false
