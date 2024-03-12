# This example does not carry a strong semantic load, but is intended only to demonstrate how the effects work.
  $ ./demo.exe << EOF
  > 
  > effect GreaterThanThousand : int -> int effect ;;
  > effect LessThanThousand : int -> int effect ;;
  > 
  > let check_number x =
  >   match x >= 1000 with
  >   | true -> perform GreaterThanThousand x
  >   | false -> perform LessThanThousand x
  > ;;
  > 
  > let check x =
  >   try check_number x with
  >   | (GreaterThanThousand x) k -> continue k (x - 1000)
  >   | (LessThanThousand x) k -> continue k (x + 1000)
  > 
  > let result1 = check 1000
  > let result2 = check 2383
  > let result3 = check 23
  > let result4 = check 0
  > 
  > EOF
  val GreaterThanThousand : int -> int effect = <effect>
  val LessThanThousand : int -> int effect = <effect>
  val check_number : int -> int = <fun>
  val check : int -> int = <fun>
  val result1 : int = 0
  val result2 : int = 1383
  val result3 : int = 1023
  val result4 : int = 1000
