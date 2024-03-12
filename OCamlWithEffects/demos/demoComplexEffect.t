# An simulated example that is intended only to demonstrate how you can work with effects with complex types.
# Doesn't carry any special meaning.
  $ ./demo.exe << EOF
  > 
  > effect E : (int list -> int) -> int effect
  > ;;
  > 
  > let list_sum lt =
  >   let rec helper acc lt =
  >     match lt with
  >     | [] -> acc
  >     | hd :: tl -> helper (acc + hd) tl
  >   in helper 0 lt
  > ;;
  > 
  > let list_sum_with_effect int_list =
  >   try perform E list_sum with
  >   | (E f) k ->
  >     let res = f int_list in
  >     continue k res
  > ;;
  > 
  > let result1 = list_sum_with_effect [1 ; 2 ; 3 ; 4 ; 5]
  > let result2 = list_sum_with_effect (1 :: 1 :: 1 :: 1 :: 1 :: [])
  > 
  > EOF
  val E : (int list -> int) -> int effect = <effect>
  val list_sum : int list -> int = <fun>
  val list_sum_with_effect : 'a -> int = <fun>
  val result1 : int = 15
  val result2 : int = 5
  $ ./demo.exe <<-EOF
  >  effect E: int effect;;
  >  let exp = try perform E with
  >    | E k -> continue k 10 + continue k 10;;
  > EOF
  Type error: unification failed - type continuation int does not match expected type int

  $ ./demo.exe <<-EOF
  >  effect Xchg: int -> int effect;;
  > 
  > let comp () = perform (Xchg 0) + perform (Xchg 1) 
  > let exp = try comp () with
  >  | (Xchg n) k -> continue k (n+1);;
  > EOF
  val Xchg : int -> int effect = <effect>
  val comp : unit -> int = <fun>
  val exp : int = 3
