  $ cat << EOF | dune exec demo -
  > let (x, x) = (5, 5)
  Variable x is bound several times
  $ cat << EOF | dune exec demo - 
  > let rec (h::tl) = 5
  Only variables are allowed as left-side of 'let rec'
  $ cat << EOF | dune exec demo - 
  > let f x = x x 
  The type variable 'a occurs inside 'a -> 'b
  $ cat << EOF | dune exec demo - 
  > let o = object val a = 5 val a = 4 end
  The instance variable `a' has multiple definitions in object
In contrast to OCaml, it is forbidden to use an override expression without initializing object self-reference
  $ cat << EOF | dune exec demo - 
  > let o = object val n = 4 method add x = {<n = n + x>}  end
  Unbound value 'self'
  $ cat << EOF | dune exec demo - 
  > let o = object (self) val a = 5 method foo = {< a = 8>} end
  > let c = o#something
  This object has type < foo : 'a > as 'a 
   It has not method something
  $ cat << EOF | dune exec demo - 
  > let rec a = a
  This kind of expression is not allowed as right-hand side of `let rec a'
  $ cat << EOF | dune exec demo -
  > let is_eight x = match x with 8 -> true
  > let eval = is_eight 7
  Match failure
  $ cat << EOF | dune exec demo - 
  > let not_obj = let a = 4 in a#meth
  Expression is not an object; it has type int
