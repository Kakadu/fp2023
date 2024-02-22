FACTORIAL:
  $ dune exec demoInterpreter << EOF
  > let rec fac n = if n <= 1 then 1 else n * fac (n - 1)
  > let x = fac 6
  > EOF
  val fac : int -> int = <fun>
  val x : int = 720
FIBONACCI:
  $ dune exec demoInterpreter << EOF
  > let rec fib = fun n -> match n with 
  > | 0 -> 0
  > | 1 -> 1 
  > | n -> fib (n - 1) + fib (n - 2) 
  > let x = fib 10
  > EOF
  val fib : int -> int = <fun>
  val x : int = 55
REVERESE LIST:
  $ dune exec demoInterpreter << EOF
  > let rev list =
  >   let rec helper acc list =
  >     match list with
  >     | [] -> acc
  >     | h :: tl -> helper (h :: acc) tl
  >   in
  >   helper [] list
  > let reversed1 = rev [1;2;3;4;5]
  > let reversed2 = rev [true;false;false;false]
  > EOF
  val rev : 'a list -> 'a list = <fun>
  val reversed1 : int list = [5; 4; 3; 2; 1]
  val reversed2 : bool list = [false; false; false; true]
FORMAT STRINGS, PRINTF, ETC
  $ dune exec demoInterpreter << EOF
  > let fmt1 = format_of_string "int: %d, char: %c\n" 
  > let fmt1 = "string: %s, " ^^ fmt1
  > let fmt1 = format_of_string fmt1
  > let str = "abcdef";;
  > printf fmt1 str (length str) str.[0]
  > EOF
  string: abcdef, int: 6, char: a
  val fmt1 : string -> int -> char -> unit format_string = "string: %s, int: %d, char: %c\n" format
  val str : string = "abcdef"
PRINTF 
  $ dune exec demoInterpreter << EOF
  > let my_printf = printf ("%d\n%d" ^^ "\n%B %B\n");;
  > my_printf 1 2 true false
  > EOF
  1
  2
  true false
  val my_printf : int -> int -> bool -> bool -> unit = <fun>
TUPLES
  $ dune exec demoInterpreter << EOF
  > let result =
  >   let id x = x in
  >   let fst (a, _) = a in
  >   let snd (_, b) = b in
  >   let tuple = 1, (2, 3) in
  >   let x = id (fst (snd tuple)) in
  >   printf "%d %d\n" x (id x)
  2 2
  val result : unit = ()
STRINGS, OPERATIONS 
  $ dune exec demoInterpreter << EOF 
  > let str = "abcde" ^ "str2"
  > let partial = printf "%c %d\n" str.[7];;
  > partial 8
  r 8
  val partial : int -> unit = <fun>
  val str : string = "abcdestr2"
CONCATINATION STRING LIST AND PRINT STEPS
  $ dune exec demoInterpreter << EOF 
  > let rec concat list =
  >   match list with
  >   | [] -> ""
  >   | h :: tl ->
  >     printf "%s\n" h;
  >     h ^ concat tl
  > let s = concat [ "aaa"; "bb"; "c" ]
  aaa
  bb
  c
  val concat : string list -> string = <fun>
  val s : string = "aaabbc"
SEVERAL_PRINTF
  $ dune exec demoInterpreter << EOF 
  > printf "a1\n";;
  > printf "b2\n";;
  > printf "c3\n";;
  a1
  b2
  c3
FACTORIAL WITH FIX
  $ dune exec demoInterpreter << EOF
  > let rec fix f x = f (fix f) x
  > let fac = fix (fun self n -> if n <= 1 then 1 else n * self (n - 1))
  > let x = fac 1
  > let y = fac 6
  val fac : int -> int = <fun>
  val fix : (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b = <fun>
  val x : int = 1
  val y : int = 720
