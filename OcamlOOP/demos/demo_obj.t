  $ cat << EOF | dune exec demo -
  > let number = object (self) val n = 0 method next = {<n = n + 1>} method get = n end
  > 
  > let point = 
  >   object (self) 
  >     val x = number
  >     val y = number
  >     method next_x = {< x = x#next>} 
  >     method next_y = {< y = y#next>} 
  >     method get = (x#get, y#get)
  >   end
  > 
  > let my_point = ((point#next_x)#next_y)#next_x
  > let my_point_v = my_point#get 
  > EOF
  val my_point : < next_x : 'a; next_y : 'a; get : int * int > as 'a = <obj>
  val my_point_v : int * int = (2, 1)
  val number : < next : 'a; get : int > as 'a = <obj>
  val point : < next_x : 'a; next_y : 'a; get : int * int > as 'a = <obj>
  $ cat << EOF | dune exec demo -
  > let cmp = (object val a = 1 val b = 1 method sum = a + b end) = (object val a = 1 val b = 1 method sum = a + b end)
  > 
  > let o =
  > object (self)
  >   val t = 5
  >   method myself = {<t = t + 1>}
  >   method id x = x
  >   method get = t
  > end
  > let c = (o#id o#myself)#get
  > EOF
  val c : int = 6
  val cmp : bool = false
  val o : < myself : 'a; id : 'b -> 'b; get : int > as 'a = <obj>
  $ cat << EOF | dune exec demo - 
  > let incr o = o#increase
  > 
  > let get_v o = (incr o)#get
  > 
  > let eval = (get_v (object (self) val t = 5 method increase = {<t = t + 1>} method get = t end))
  > EOF
  val eval : 'a = 6
  val get_v : < increase : < get : 'a; .. >; .. > -> 'a = <fun>
  val incr : < increase : 'a; .. > -> 'a = <fun>
