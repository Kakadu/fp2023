  $ ./demo.exe << EOF
  > 
  > let rec fold_left f acc l = 
  >   match l with
  >   | [] -> acc
  >   | h :: tl -> fold_left f (f acc h) tl
  > ;;
  > 
  > let reverse l = fold_left (fun acc h -> h :: acc) [] l
  > 
  > let lst = [1; 2; 3; 4; 5]
  > let reverse_res = reverse lst
  > 
  > let abs_if_negative x = if x < 0 then -x else x
  > 
  > let replace_negatives lst =
  >   let replace acc elem =
  >     match elem > 0 with
  >     | false-> (abs_if_negative elem) :: acc
  >     | true -> elem :: acc
  >   in
  >   let reversed_result = fold_left replace [] lst in
  >   reverse reversed_result
  > ;;
  > 
  > let abs_result = replace_negatives [-1 ; 5 ; -4 ; 3 ; -6 ; -(-(+2)) ; 0]
  > 
  > EOF
  val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a = <fun>
  val reverse : 'a list -> 'a list = <fun>
  val lst : int list = [1; 2; 3; 4; 5]
  val reverse_res : int list = [5; 4; 3; 2; 1]
  val abs_if_negative : int -> int = <fun>
  val replace_negatives : int list -> int list = <fun>
  val abs_result : int list = [1; 5; 4; 3; 6; 2; 0]
