  $ cat << EOF | dune exec demo - 
  > let my_list =
  >   object (self)
  >     val t = []
  > 
  >     method get = t
  > 
  >     method add x = {<t = x :: t>}
  > 
  >     method rev =
  >       let rec helper acc list =
  >         match list with
  >           | [] -> acc
  >           | (h :: tl) -> helper (h :: acc) tl
  >         in
  >         let rev_list = helper [] t in
  >         {<t = rev_list>}
  > 
  >     method tail =
  >       match t with
  >         | [] -> {<t = []>}
  >         | (_ :: tl) -> {<t = tl>}
  > 
  >     method remove x =
  >       let rec helper acc list =
  >         match list with
  >           | [] -> acc
  >           | (h :: tl) -> if h = x then helper acc tl else helper (h :: acc) tl
  >       in
  >       {<t = helper [] t>} 
  >       
  >   end
  > 
  > 
  > let l = ((my_list#add 5)#add 4)#add 6
  > let l_v = l#get
  > 
  > let rev_l = l#rev
  > let rev_v = rev_l#get
  > 
  > let tail_rev_l = rev_l#tail
  > let tail_v = tail_rev_l#get
  > 
  > let sl = ((l#add 9)#add 10)#add 2
  > let sl_v = sl#get
  > let sl_remove_rev = sl#remove 6
  > let sl_remove = sl_remove_rev#rev
  > let sl_remove_v = sl_remove#get
  > EOF
  val l : < get : int list; add : int -> 'a; rev : 'a; tail : 'a; remove : int -> 'a > as 'a = <obj>
  val l_v : int list = [6; 4; 5]
  val my_list : < get : 'b list; add : 'b -> 'a; rev : 'a; tail : 'a; remove : 'b -> 'a > as 'a = <obj>
  val rev_l : < get : int list; add : int -> 'a; rev : 'a; tail : 'a; remove : int -> 'a > as 'a = <obj>
  val rev_v : int list = [5; 4; 6]
  val sl : < get : int list; add : int -> 'a; rev : 'a; tail : 'a; remove : int -> 'a > as 'a = <obj>
  val sl_remove : < get : int list; add : int -> 'a; rev : 'a; tail : 'a; remove : int -> 'a > as 'a = <obj>
  val sl_remove_rev : < get : int list; add : int -> 'a; rev : 'a; tail : 'a; remove : int -> 'a > as 'a = <obj>
  val sl_remove_v : int list = [2; 10; 9; 4; 5]
  val sl_v : int list = [2; 10; 9; 6; 4; 5]
  val tail_rev_l : < get : int list; add : int -> 'a; rev : 'a; tail : 'a; remove : int -> 'a > as 'a = <obj>
  val tail_v : int list = [4; 6]

