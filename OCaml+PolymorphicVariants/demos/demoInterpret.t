  $ dune exec demoInterpret << EOF
  > let rec fac n = if n < 1 then 1 else n * fac (n - 1);;
  > EOF
  val fac : int -> int = <fun>
  $ dune exec demoInterpret << EOF
  > let map_cps f l = 
  >   let rec helper k xs =
  >     match xs with
  >     | [] -> k []
  >     | h :: tl -> helper (fun r -> k ((f h) :: r)) tl
  >   in
  >   helper (fun x -> x) l
  > ;;
  > EOF
  val map_cps : ('6 -> '8) -> '6 list -> '8 list = <fun>
  $ dune exec demoInterpret << EOF
  >   let rec fix f x = f (fix f) x;;
  >   let fac fac_ n = if n = 1 then 1 else n * fac_ (n - 1);;
  >   let x = fix fac 5;;
  > EOF
  val fac : (int -> int) -> int -> int = <fun>
  val fix : (('2 -> '3) -> '2 -> '3) -> '2 -> '3 = <fun>
  val x : int = 120
  $ dune exec demoInterpret << EOF
  > let rec merge l1 l2 = 
  >   match l1, l2 with
  >   | [], l2 -> l2
  >   | l1, [] -> l1
  >   | h1 :: t1, h2 :: t2 ->
  >       if h1 <= h2
  >       then h1 :: merge t1 l2
  >       else h2 :: merge l1 t2
  > ;;
  > let merged = merge [1; 3; 6] [2; 4; 5];;
  > EOF
  val merge : '7 list -> '7 list -> '7 list = <fun>
  val merged : int list = [1; 2; 3; 4; 5; 6]
  $ dune exec demoInterpret << EOF
  > let rec fac_cps n k = if n = 0 then k 1 else fac_cps (n - 1) (fun r -> k (n * r));;
  > let x = fac_cps 5 (fun x -> x);;
  > EOF
  val fac_cps : int -> (int -> '5) -> '5 = <fun>
  val x : int = 120
  $ dune exec demoInterpret << EOF
  > let rec fib n = if n <= 1 then 1 else fib (n - 1) + fib (n - 2);;
  > let init f n =
  >   let rec helper i last f k =
  >     if i = last then k []
  >     else helper (i + 1) last f (fun c -> k (f i :: c))
  >   in
  >   helper 0 n f (fun x -> x)
  > ;;
  > let first_n_fibs n = init (fun i -> fib i) n;;
  > let l = first_n_fibs 10;;
  > EOF
  val fib : int -> int = <fun>
  val first_n_fibs : int -> int list = <fun>
  val init : (int -> '26) -> int -> '26 list = <fun>
  val l : int list = [1; 1; 2; 3; 5; 8; 13; 21; 34; 55]
  $ dune exec demoInterpret << EOF
  > let rec lcmp l1 l2 = 
  >   match l1, l2 with
  >   | [], [] -> 0
  >   | [], _ -> -1
  >   | _, [] -> 1
  >   | _ :: l1, _ :: l2 -> lcmp l1 l2
  > ;;
  > let equally_long = lcmp [1; 2; 3; 4; 5] [5;4;3;2;1];;
  > EOF
  val equally_long : int = 0
  val lcmp : '4 list -> '5 list -> int = <fun>
