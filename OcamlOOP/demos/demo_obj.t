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
  > EOF
  val cmp : bool = false
