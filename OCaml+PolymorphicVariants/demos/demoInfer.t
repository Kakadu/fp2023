  $ dune exec demoInfer << EOF
  > let f x y z = (x, y, z);;
  > let p1 = (f, f, f);;
  > EOF
  val f : '0 -> '1 -> '2 -> ('0 * '1 * '2)
  val p1 : (('3 -> '4 -> '5 -> ('3 * '4 * '5)) * ('6 -> '7 -> '8 -> ('6 * '7 * '8)) * ('9 -> '10 -> '11 -> ('9 * '10 * '11)))
  $ dune exec demoInfer << EOF
  > let ff f x = f (f x);;
  > let p = (ff (fun x -> x + 1) 1, ff (fun x -> x && true) false);;
  > EOF
  val ff : ('1 -> '1) -> '1 -> '1
  val p : (int * bool)
  $ dune exec demoInfer << EOF
  > let rec fib_cps n k =
  >   if n < 2 then k 1
  >   else fib_cps (n - 1) (fun n1 -> fib_cps (n - 2) (fun n2 ->  k (n1 + n2)))
  > ;;
  > let id x = x;;
  > let fib_nth n = fib_cps n id;;
  > let fibs = [fib_nth 1; fib_nth 2; fib_nth 3];;
  > EOF
  val fib_cps : int -> (int -> '4) -> '4
  val fib_nth : int -> int
  val fibs : int list
  val id : '12 -> '12
  $ dune exec demoInfer << EOF
  > let rec fix f x = f (fix f) x;;
  > let fac fac_ n = if n < 2 then 1 else n * fac_ (n - 1);;
  > let x = fix fac 5;;
  > EOF
  val fac : (int -> int) -> int -> int
  val fix : (('2 -> '3) -> '2 -> '3) -> '2 -> '3
  val x : int
