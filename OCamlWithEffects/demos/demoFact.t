  $ dune exec demo << EOF
  > 
  > let rec fact n = if n = 1 then 1 else n * fact (n - 1);;
  > let res = fact 5;;
  > 
  > EOF
  val fact : int -> int = <fun>
  val res : int = 120
