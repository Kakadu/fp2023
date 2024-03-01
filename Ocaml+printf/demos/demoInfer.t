  $ dune exec demoInfer << EOF
  > let rec fac n = if n <= 1 then 1 else n * fac (n - 1)
  > let sum x y = x + y
  > let id x = x 
  > let f x = match id x with
  > | a::b::c -> let s = sum a b in printf "%d" s; s
  > | a::b -> printf "%d" a; fac a 
  > | [] -> printf "Achieved empty\n"; 1 
  > EOF
  val f : int list -> int
  val fac : int -> int
  val id : forall 'a. 'a -> 'a
  val sum : int -> int -> int
PRINTF
  $ dune exec demoInfer << EOF
  > let my_printf = printf
  > let fmt_printf = my_printf ("string: %s\n" ^^ format_of_string "int: %d\n");;
  > fmt_printf "abcd" 123
  > EOF
  val fmt_printf : string -> int -> unit
  val my_printf : forall 'a. 'a format_string -> 'a
MORE EXAMPLES
  $ dune exec demoInfer << EOF
  > let fst (a, b) = a
  > let snd (a, b) = b 
  > let tuple1 = (fst, snd, ('a',"bcd"))
  > let tuple2 = let unpack (f1, f2, t) = (f1 t, f2 t) in unpack tuple1
  > EOF
  val fst : forall 'a 'b. 'a * 'b -> 'a
  val snd : forall 'a 'b. 'a * 'b -> 'b
  val tuple1 : forall 'a 'b 'c 'd. ('a * 'b -> 'a) * ('c * 'd -> 'd) * (char * string)
  val tuple2 : char * string
FACTORIAL WITH FIX
  $ dune exec demoInfer << EOF
  > let rec fix f x = f (fix f) x
  > let fac = fix (fun self n -> if n <= 1 then 1 else n * self (n - 1))
  > let x = fac 1
  > let y = fac 10
  val fac : int -> int
  val fix : forall 'a 'b. (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b
  val x : int
  val y : int
