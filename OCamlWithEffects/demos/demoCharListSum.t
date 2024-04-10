  $ ./demo.exe << EOF
  > 
  > effect NotDigit: char -> int effect
  > 
  > let int_of_char c = match c with
  >  | '0' -> 0
  >  | '1' -> 1 
  >  | '2' -> 2
  >  | '3' -> 3
  >  | '4' -> 4
  >  | '5' -> 5
  >  | '6' -> 6
  >  | '7' -> 7
  >  | '8' -> 8
  >  | '9' -> 9
  >  | c -> perform (NotDigit c)
  > ;;
  > 
  > let rec sum_up li = match li with
  >  | [] -> 0
  >  | h :: tl -> int_of_char h + sum_up tl
  > ;;
  > 
  > let test_l = ['1'; 'a'; '0'; '1'; '5'; '7'; 'v'; '2'; '9']
  > 
  > let res = 
  >   try sum_up test_l with
  >   | (NotDigit x) k -> continue k 0
  > 
  > EOF
  val NotDigit : char -> int effect = <effect>
  val int_of_char : char -> int = <fun>
  val sum_up : char list -> int = <fun>
  val test_l : char list = ['1'; 'a'; '0'; '1'; '5'; '7'; 'v'; '2'; '9']
  val res : int = 25
