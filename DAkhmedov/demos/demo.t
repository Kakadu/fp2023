  $ dune exec fact_demo
  val fact : int = 3628800
  val factorial : int -> int = <fun>
  val fib : int = 34
  val fibonacci : int -> int = <fun>
  val is_prime : int -> bool = <fun>
  val is_prime_number : bool = false
  val number : int = 10
  val res : int * int * int * bool = (10, 3628800, 34, false)

  $ dune exec lists_demo
  val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a = <fun>
  val init : int -> int list -> int list = <fun>
  val map : ('a -> 'b) -> 'a list -> 'b list = <fun>
  val map_square : int list -> int list -> int list = <fun>
  val next_number : int -> (int * int) list = <fun>
  val numbers : int list = [10; 9; 8; 7; 6; 5; 4; 3; 2; 1]
  val numbers_anamorph : int list = [9; 8; 7; 6; 5; 4; 3; 2; 1]
  val res : int * int * int = (385, 385, 285)
  val square : int -> int = <fun>
  val squares : int list = [1; 4; 9; 16; 25; 36; 49; 64; 81; 100]
  val sum_of_squares_anamorph : int = 285
  val sum_of_squares_fold : int = 385
  val sum_of_squares_map : int = 385
  val unfoldr_next_number : int list -> int -> int list = <fun>

  $ dune exec cps_demo
  5040
  1
  val a : int = 6
  val fac : int -> int = <fun>
  val fac_cps : int -> (int -> 'a) -> 'a = <fun>
  val fac_test : int = 5040
  val id : 'a -> 'a = <fun>
  val map : ('a -> 'b) -> 'a list -> 'b list = <fun>
  val map_cps : ('a -> 'b) -> 'a list -> ('b list -> 'c) -> 'c = <fun>
  val map_test : int list = [2; 3; 4; 5; 6]
  val print_int : int = 5
  val sumlist : int list -> int = <fun>
  val sumlist_cps : int list -> (int -> 'a) -> 'a = <fun>
  val sumlist_test : int = 116


