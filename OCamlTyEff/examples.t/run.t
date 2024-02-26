Red-black tree map
  $ ../bin/repl.exe <<EOF
  > :load 1rbmap.ml
  > let smap_add, smap_find = map_add string_compare, map_find string_compare;;
  > 
  Black : (('a, 'b) map * 'a * 'b * ('a, 'b) map) -> ('a, 'b) map
  Empty : ('a, 'b) map
  Red : (('a, 'b) map * 'a * 'b * ('a, 'b) map) -> ('a, 'b) map
  map_add : ('a -'e-> 'a -'e-> int) -> 'a -> 'b -> ('a, 'b) map -'e-> ('a, 'b) map = <fun>
  map_empty : ('a, 'b) map = Empty
  map_find : ('a -[exn _Not_found | 'e]-> 'b -[exn _Not_found | 'e]-> int) -> 'a -> ('b, 'c) map -[exn _Not_found | 'e]-> 'c = <fun>
  map_mem : ('a -'e-> 'b -'e-> int) -> 'a -> ('b, 'c) map -'e-> bool = <fun>
  map_remove : ('a -'e-> 'b -'e-> int) -> 'a -> ('b, 'c) map -'e-> ('b, 'c) map = <fun>
  
  smap_add : string -> 'a -> (string, 'a) map -> (string, 'a) map = <fun>
  smap_find : string -> (string, 'a) map -[exn _Not_found]-> 'a = <fun>
  

Simple parser-combinator library
  $ ../bin/repl.exe <<EOF
  > :load 2parser.ml
  > 
  Failed : 'a parse_result
  Parsed : ('a * char list) -> 'a parse_result
  fail : 'a -> 'b parse_result = <fun>
  return : 'a -> char list -> 'a parse_result = <fun>
  satisfy : (char -'e-> bool) -> char list -'e-> char parse_result = <fun>
  char : char -> char list -> char parse_result = <fun>
  >>= : ('a -'e-> 'b parse_result) -> ('b -'e-> char list -'e-> 'c parse_result) -> 'a -'e-> 'c parse_result = <fun>
  *> : ('a -'e-> 'b parse_result) -> (char list -'e-> 'c parse_result) -> 'a -'e-> 'c parse_result = <fun>
  <* : ('a -'e-> 'b parse_result) -> (char list -'e-> 'c parse_result) -> 'a -'e-> 'b parse_result = <fun>
  >>| : ('a -'e-> 'b parse_result) -> ('b -'e-> 'c) -> 'a -'e-> 'c parse_result = <fun>
  many : (char list -'e-> 'a parse_result) -> char list -'e-> 'a list parse_result = <fun>
  many1 : (char list -'e-> 'a parse_result) -> char list -'e-> 'a list parse_result = <fun>
  take_while : (char -'e-> bool) -> char list -'e-> string parse_result = <fun>
  take_while1 : (char -'e-> bool) -> char list -'e-> string parse_result = <fun>
  <|> : ('a -'e-> 'b parse_result) -> ('a -'e-> 'b parse_result) -> 'a -'e-> 'b parse_result = <fun>
  choice : ('a -'e-> 'b parse_result) list -> 'a -'e-> 'b parse_result = <fun>
  lift2 : ('a -'e-> 'b -'e-> 'c) -> ('d -'e-> 'a parse_result) -> (char list -'e-> 'b parse_result) -> 'd -'e-> 'c parse_result = <fun>
  chainl1 : (char list -'e-> 'a parse_result) -> (char list -'e-> ('a -'e-> 'a -'e-> 'a) parse_result) -> char list -'e-> 'a parse_result = <fun>
  fix : (('a -'e-> 'b) -'e-> 'a -'e-> 'b) -> 'a -'e-> 'b = <fun>
  run : (char list -'e-> 'a parse_result) -> string -'e-> 'a option = <fun>
  

Arithmetic expressions interpreter
  $ ../bin/repl.exe <<EOF
  > :load 1rbmap.ml
  > :load 2parser.ml
  > :load 3arith.ml
  > 
  Black : (('a, 'b) map * 'a * 'b * ('a, 'b) map) -> ('a, 'b) map
  Empty : ('a, 'b) map
  Red : (('a, 'b) map * 'a * 'b * ('a, 'b) map) -> ('a, 'b) map
  map_add : ('a -'e-> 'a -'e-> int) -> 'a -> 'b -> ('a, 'b) map -'e-> ('a, 'b) map = <fun>
  map_empty : ('a, 'b) map = Empty
  map_find : ('a -[exn _Not_found | 'e]-> 'b -[exn _Not_found | 'e]-> int) -> 'a -> ('b, 'c) map -[exn _Not_found | 'e]-> 'c = <fun>
  map_mem : ('a -'e-> 'b -'e-> int) -> 'a -> ('b, 'c) map -'e-> bool = <fun>
  map_remove : ('a -'e-> 'b -'e-> int) -> 'a -> ('b, 'c) map -'e-> ('b, 'c) map = <fun>
  
  Failed : 'a parse_result
  Parsed : ('a * char list) -> 'a parse_result
  fail : 'a -> 'b parse_result = <fun>
  return : 'a -> char list -> 'a parse_result = <fun>
  satisfy : (char -'e-> bool) -> char list -'e-> char parse_result = <fun>
  char : char -> char list -> char parse_result = <fun>
  >>= : ('a -'e-> 'b parse_result) -> ('b -'e-> char list -'e-> 'c parse_result) -> 'a -'e-> 'c parse_result = <fun>
  *> : ('a -'e-> 'b parse_result) -> (char list -'e-> 'c parse_result) -> 'a -'e-> 'c parse_result = <fun>
  <* : ('a -'e-> 'b parse_result) -> (char list -'e-> 'c parse_result) -> 'a -'e-> 'b parse_result = <fun>
  >>| : ('a -'e-> 'b parse_result) -> ('b -'e-> 'c) -> 'a -'e-> 'c parse_result = <fun>
  many : (char list -'e-> 'a parse_result) -> char list -'e-> 'a list parse_result = <fun>
  many1 : (char list -'e-> 'a parse_result) -> char list -'e-> 'a list parse_result = <fun>
  take_while : (char -'e-> bool) -> char list -'e-> string parse_result = <fun>
  take_while1 : (char -'e-> bool) -> char list -'e-> string parse_result = <fun>
  <|> : ('a -'e-> 'b parse_result) -> ('a -'e-> 'b parse_result) -> 'a -'e-> 'b parse_result = <fun>
  choice : ('a -'e-> 'b parse_result) list -> 'a -'e-> 'b parse_result = <fun>
  lift2 : ('a -'e-> 'b -'e-> 'c) -> ('d -'e-> 'a parse_result) -> (char list -'e-> 'b parse_result) -> 'd -'e-> 'c parse_result = <fun>
  chainl1 : (char list -'e-> 'a parse_result) -> (char list -'e-> ('a -'e-> 'a -'e-> 'a) parse_result) -> char list -'e-> 'a parse_result = <fun>
  fix : (('a -'e-> 'b) -'e-> 'a -'e-> 'b) -> 'a -'e-> 'b = <fun>
  run : (char list -'e-> 'a parse_result) -> string -'e-> 'a option = <fun>
  
  Asterisk : (expr * expr) -> expr
  Const : int -> expr
  Minus : (expr * expr) -> expr
  Plus : (expr * expr) -> expr
  Slash : (expr * expr) -> expr
  Var : string -> expr
  parse : string -> expr option = <fun>
  Div_by_zero : eval_err
  Syntax_error : eval_err
  Unbound_variable : string -> eval_err
  eval : expr -> (string, int) map -> (int, eval_err) result = <fun>
  interp : string -> (string, int) map -> (int, eval_err) result = <fun>
  

Memoization
  $ ../bin/repl.exe <<EOF
  > :load 1rbmap.ml
  > :load 4memo.ml
  > 
  Black : (('a, 'b) map * 'a * 'b * ('a, 'b) map) -> ('a, 'b) map
  Empty : ('a, 'b) map
  Red : (('a, 'b) map * 'a * 'b * ('a, 'b) map) -> ('a, 'b) map
  map_add : ('a -'e-> 'a -'e-> int) -> 'a -> 'b -> ('a, 'b) map -'e-> ('a, 'b) map = <fun>
  map_empty : ('a, 'b) map = Empty
  map_find : ('a -[exn _Not_found | 'e]-> 'b -[exn _Not_found | 'e]-> int) -> 'a -> ('b, 'c) map -[exn _Not_found | 'e]-> 'c = <fun>
  map_mem : ('a -'e-> 'b -'e-> int) -> 'a -> ('b, 'c) map -'e-> bool = <fun>
  map_remove : ('a -'e-> 'b -'e-> int) -> 'a -> ('b, 'c) map -'e-> ('b, 'c) map = <fun>
  
  fib : int -> int = <fun>
  fib_memo : int -[ref]-> int = <fun>
  

Continuation-passing style
  $ ../bin/repl.exe <<EOF
  > :load 5cps.ml
  > 
  map : ('a -'e-> 'b) -> 'a list -'e-> 'b list = <fun>
  map_cps : ('a -'e-> 'b) -> 'a list -'e-> 'b list = <fun>
  return : 'a -> ('a -'e-> 'b) -'e-> 'b = <fun>
  >>= : (('a -'e-> 'b) -'e1-> 'c) -> ('a -'e-> 'd -'e-> 'b) -> 'd -'e1-> 'c = <fun>
  run : (('a -> 'a) -'e-> 'b) -'e-> 'b = <fun>
  fold_right : ('a -'e-> 'b -'e-> 'b) -> 'b -> 'a list -'e-> 'b = <fun>
  fold_right_cps : ('a -'e-> 'b -'e-> 'b) -> 'b -> 'a list -'e-> 'b = <fun>
  

Hylomorphisms
  $ ../bin/repl.exe <<EOF
  > :rectypes
  > :load 6hylo.ml
  > 
  Recursive types on
  
  cata : (('a -'e-> 'b) -'e-> 'a -'e-> 'c) -> ('c -'e-> 'b) -> 'a -'e-> 'b = <fun>
  ana : (('a -'e-> 'b) -'e-> 'c -'e-> 'b) -> ('a -'e-> 'c) -> 'a -'e-> 'b = <fun>
  Cons : (int * 'a) -> 'a list_layer
  Nil : 'a list_layer
  map_layer : ('a -'e-> 'b) -> 'a list_layer -'e-> 'b list_layer = <fun>
  phi_list : int list_layer -> int = <fun>
  psi_list : int -> int list_layer = <fun>
  hylo : ('a list_layer -'e-> 'a) -> ('b -'e-> 'b list_layer) -> 'b -'e-> 'a = <fun>
  fact : int -> int = <fun>
  OfTwo : ('a * 'a) -> 'a inter
  One : 'a inter
  Zero : 'a inter
  map_inter : ('a -'e-> 'b) -> 'a inter -'e-> 'b inter = <fun>
  psi_fib : int -> int inter = <fun>
  phi_fib : int inter -> int = <fun>
  hylo : ('a inter -'e-> 'a) -> ('b -'e-> 'b inter) -> 'b -'e-> 'a = <fun>
  fib : int -> int = <fun>
  No : 'a inter
  One : 'a -> 'a inter
  Two : ('a * 'a) -> 'a inter
  map_inter : ('a -'e-> 'b) -> 'a inter -'e-> 'b inter = <fun>
  psi_bp : int -> int inter = <fun>
  phi_bp : int inter -> int = <fun>
  hylo : ('a inter -'e-> 'a) -> ('b -'e-> 'b inter) -> 'b -'e-> 'a = <fun>
  bp : int -> int = <fun>
  
