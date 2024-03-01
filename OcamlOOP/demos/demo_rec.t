  $ cat << EOF | dune exec demo -
  > let rec fact x = if x = 0 then 1 else x * fact (x - 1)
  > 
  > let eval_fact = fact 6
  > 
  > let fib n = 
  >   let rec helper n y x = 
  >     if n <= 0 then x
  >     else helper (n - 1) (x + y) y in
  >   helper n 1 0
  > 
  > let eval_fib = fib 10
  > 
  > let reverse l = 
  >   let rec helper acc list = 
  >     match list with 
  >       | [] -> acc
  >       | (h :: tl) -> helper (h :: acc) tl
  >   in
  >   helper [] l 
  > 
  > let reverse_int = reverse ([1; 2; 3; 4; 5])
  > let reverse_bool = reverse ([true; false])
  > 
  > let rec range a b = 
  >   if a > b then []
  >   else a :: range (a + 1) b
  > 
  > let eval_range = range 5 8
  > EOF
  val eval_fact : int = 720
  val eval_fib : int = 55
  val eval_range : int list = [5; 6; 7; 8]
  val fact : int -> int = <fun>
  val fib : int -> int = <fun>
  val range : int -> int -> int list = <fun>
  val reverse : 'a list -> 'a list = <fun>
  val reverse_bool : bool list = [false; true]
  val reverse_int : int list = [5; 4; 3; 2; 1]
  $ cat << EOF | dune exec demo -
  > let rec fix f x = f (fix f) x
  > let fact = fix (fun self n -> if n <= 1 then 1 else n * self (n-1))
  val fact : int -> int = <fun>
  val fix : (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b = <fun>

  $ cat << EOF | dune exec demo -
  > let rec fact n k = if n<=1 then k 1 else fact (n-1) (fun z -> k(z*n))
  > EOF
  val fact : int -> (int -> 'a) -> 'a = <fun>
