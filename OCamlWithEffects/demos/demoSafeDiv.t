# If the denominator is equal to zero, we consider the expression equal to zero.
  $ ./demo.exe << EOF
  > 
  > effect DevisionByZero : int effect ;;
  > 
  > let div x y = match y with | 0 -> perform DevisionByZero | _ -> x / y
  > let safe_div x y = try (div x y) with | DevisionByZero k -> continue k 0
  > let result1 = safe_div 25 5
  > let result2 = safe_div 1 0
  > 
  > EOF
  val DevisionByZero : int effect = <effect>
  val div : int -> int -> int = <fun>
  val safe_div : int -> int -> int = <fun>
  val result1 : int = 5
  val result2 : int = 0
