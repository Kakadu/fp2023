  $ ./demo.exe<< EOF
  > 
  > let rec fact n = if n = 1 then 1 else n * fact (n - 1);;
  > let res = fact 5;;
  > 
  > let rec fack n k = if n<2 then k 1 else fack (n-1) (fun m -> k (n*m));;
  > let res2 = fack 10 (fun x -> x)
  > 
  > let res3 = let f x = x in (f 1, f true);;
  > let rec fix f x = f (fix f) x ;;
  > let fib_open self a b n = if n=1 then a else self b (a+b) (n-1);;
  > let fib = fix fib_open;;
  > let fib5 = fib 0 1 5;;
  > EOF
  val fact : int -> int = <fun>
  val res : int = 120
  val fack : int -> (int -> 'a) -> 'a = <fun>
  val res2 : int = 3628800
  val res3 : int * bool = (1, true)
  val fix : (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b = <fun>
  val fib_open : (int -> int -> int -> int) -> int -> int -> int -> int = <fun>
  val fib : int -> int -> int -> int = <fun>
  val fib5 : int = 3
