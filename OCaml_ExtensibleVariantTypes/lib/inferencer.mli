module R : sig
  type 'a t = int -> int * ('a, Typing.error) result

  val ( >>= )
    :  ('a -> 'b * ('c, 'd) result)
    -> ('c -> 'b -> 'b * ('e, 'd) result)
    -> 'a
    -> 'b * ('e, 'd) result

  val fail : 'a -> 'b -> 'b * ('c, 'a) result
  val return : 'a -> 'b -> 'b * ('a, 'c) result

  module Syntax : sig
    val ( let* )
      :  ('a -> 'b * ('c, 'd) result)
      -> ('c -> 'b -> 'b * ('e, 'd) result)
      -> 'a
      -> 'b * ('e, 'd) result
  end

  module RList : sig
    val fold_left
      :  ('a, 'b, 'c) Base.Map.t
      -> init:('d -> 'e * ('f, 'g) result)
      -> f:('a -> 'b -> 'f -> 'e -> 'e * ('f, 'g) result)
      -> 'd
      -> 'e * ('f, 'g) result
  end

  val fresh : int -> int * (int, 'a) result
  val run : (int -> 'a * 'b) -> 'b
end

type fresh = int

module Type : sig
  type t = Typing.typ

  val occurs_in : fresh -> Typing.typ -> bool
  val free_vars : Typing.typ -> (fresh, Base.Int.comparator_witness) Base.Set.t
end

module Subst : sig
  type t = (fresh, Typing.typ, Base.Int.comparator_witness) Base.Map.t

  val empty : (fresh, 'a, Base.Int.comparator_witness) Base.Map.t

  val mapping
    :  ?is_rec:bool
    -> fresh
    -> Typing.typ
    -> 'a
    -> 'a * (fresh * Typing.typ, Typing.error) result

  val singleton
    :  ?is_rec:bool
    -> fresh
    -> Typing.typ
    -> 'a
    -> 'a
       * ( (fresh, Typing.typ, Base.Int.comparator_witness) Base.Map.t
           , Typing.error )
           result

  val find : 'a -> ('a, 'b, 'c) Base.Map.t -> 'b option
  val remove : ('a, 'b, 'c) Base.Map.t -> 'a -> ('a, 'b, 'c) Base.Map.t
  val apply : (fresh, Typing.typ, 'a) Base.Map.t -> Typing.typ -> Typing.typ

  val unify
    :  ?is_rec:bool
    -> Typing.typ
    -> Typing.typ
    -> 'a
    -> 'a
       * ( (fresh, Typing.typ, Base.Int.comparator_witness) Base.Map.t
           , Typing.error )
           result

  val extend
    :  ?is_rec:bool
    -> fresh
    -> Typing.typ
    -> (fresh, Typing.typ, Base.Int.comparator_witness) Base.Map.t
    -> 'a
    -> 'a
       * ( (fresh, Typing.typ, Base.Int.comparator_witness) Base.Map.t
           , Typing.error )
           result

  val compose
    :  ?is_rec:bool
    -> (fresh, Typing.typ, Base.Int.comparator_witness) Base.Map.t
    -> (fresh, Typing.typ, Base.Int.comparator_witness) Base.Map.t
    -> 'a
    -> 'a
       * ( (fresh, Typing.typ, Base.Int.comparator_witness) Base.Map.t
           , Typing.error )
           result

  val compose_all
    :  ?is_rec:bool
    -> (fresh, Typing.typ, Base.Int.comparator_witness) Base.Map.t list
    -> 'a
    -> 'a
       * ( (fresh, Typing.typ, Base.Int.comparator_witness) Base.Map.t
           , Typing.error )
           result
end

type scheme = (fresh, Base.Int.comparator_witness) Base.Set.t * Typing.typ

module Scheme : sig
  type t = scheme

  val empty : (fresh, Base.Int.comparator_witness) Base.Set.t
  val occurs_in : fresh -> (fresh, 'a) Base.Set.t * Typing.typ -> bool

  val free_vars
    :  (fresh, Base.Int.comparator_witness) Base.Set.t * Typing.typ
    -> (fresh, Base.Int.comparator_witness) Base.Set.t

  val apply
    :  (fresh, Typing.typ, 'a) Base.Map.t
    -> (fresh, 'b) Base.Set.t * Typing.typ
    -> (fresh, 'b) Base.Set.t * Typing.typ
end

module TypeEnv : sig
  type t = (string, scheme, Base.String.comparator_witness) Base.Map.t

  val extend : ('a, 'b, 'c) Base.Map.t -> 'a -> 'b -> ('a, 'b, 'c) Base.Map.t
  val empty : (string, 'a, Base.String.comparator_witness) Base.Map.t
  val free_vars : t -> (fresh, Base.Int.comparator_witness) Base.Set.t

  val apply
    :  (fresh, Typing.typ, 'a) Base.Map.t
    -> ('b, (fresh, 'c) Base.Set.t * Typing.typ, 'd) Base.Map.t
    -> ('b, (fresh, 'c) Base.Set.t * Typing.typ, 'd) Base.Map.t
end

val unify
  :  ?is_rec:bool
  -> Typing.typ
  -> Typing.typ
  -> 'a
  -> 'a * ((int, Typing.typ, Base.Int.comparator_witness) Base.Map.t, Typing.error) result

val fresh_var : int -> int * (Typing.typ, 'a) result

val instantiate
  :  ?is_rec:bool
  -> (int, 'a) Base.Set.t * Typing.typ
  -> int
  -> int * (Typing.typ, Typing.error) result

val generalize
  :  TypeEnv.t
  -> Typing.typ
  -> (int, Base.Int.comparator_witness) Base.Set.t * Typing.typ

val lookup_env
  :  string
  -> (string, (int, 'a) Base.Set.t * Typing.typ, 'b) Base.Map.t
  -> int
  -> int
     * ( (int, 'c, Base.Int.comparator_witness) Base.Map.t * Typing.typ
         , Typing.error )
         result

val infer_ptrn
  :  ?env:
       ( string
         , (int, Base.Int.comparator_witness) Base.Set.t * Typing.typ
         , Base.String.comparator_witness )
         Base.Map.t
  -> Ast.pattern
  -> int
  -> int
     * ( ( string
           , (int, Base.Int.comparator_witness) Base.Set.t * Typing.typ
           , Base.String.comparator_witness )
           Base.Map.t
         * Typing.typ
         , Typing.error )
         result

val infer
  :  (string, scheme, Base.String.comparator_witness) Base.Map.t
  -> Ast.expr
  -> int
  -> int
     * ( (int, Typing.typ, Base.Int.comparator_witness) Base.Map.t * Typing.typ
         , Typing.error )
         result

val infer_decl
  :  (string, scheme, Base.String.comparator_witness) Base.Map.t
  -> Ast.decl
  -> int
  -> int
     * ( (int, Typing.typ, Base.Int.comparator_witness) Base.Map.t
         * (string, scheme, Base.String.comparator_witness) Base.Map.t
         , Typing.error )
         result

val init : (string * Typing.typ) list

val init_env
  : ( string
      , (int, Base.Int.comparator_witness) Base.Set.t * Typing.typ
      , Base.String.comparator_witness )
      Base.Map.t

val run_inference
  :  Ast.decl list
  -> ((string, scheme, Base.String.comparator_witness) Base.Map.t, Typing.error) result

val is_printable : string -> Typing.typ -> bool
val print_env : ((string, 'a * Typing.typ, 'b) Base.Map.t, Typing.error) result -> unit
