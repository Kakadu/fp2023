# An simulated example that is intended only to demonstrate how you can work with effects with complex types.
# Doesn't carry any special meaning.
  $ dune exec demo << EOF
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
  val list_sum_with_effect : 'n -> int = <fun>
  val result1 : int = 15
  val result2 : int = 5
