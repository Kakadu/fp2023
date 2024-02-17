module R : sig
  type ('a, 'e) t = int -> int * ('a, 'e) result

  val return : 'a -> ('a, 'e) t
  val fail : 'e -> ('a, 'e) t
  val ( >>= ) : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t
  val ( let* ) : ('a, 'b) t -> ('a -> ('c, 'b) t) -> ('c, 'b) t
end

val fresh : int -> int * (int, 'a) result
val run : (int -> 'a * 'b) -> 'b

module VarSet : sig
  val compare
    :  'a Base.Exported_for_specific_uses.Ppx_compare_lib.compare
    -> 'b Base.Exported_for_specific_uses.Ppx_compare_lib.compare
    -> ('a, 'b) Base.Set.t Base.Exported_for_specific_uses.Ppx_compare_lib.compare

  type ('k, 'cmp) comparator = ('k, 'cmp) Base.Comparator.Module.t

  val invariants : ('a, 'b) Base.Set.t -> bool
  val comparator_s : ('a, 'cmp) Base.Set.t -> ('a, 'cmp) Base.Comparator.Module.t
  val comparator : ('a, 'cmp) Base.Set.t -> ('a, 'cmp) Base.Comparator.t
  val singleton : ('a, 'cmp) Base.Comparator.Module.t -> 'a -> ('a, 'cmp) Base.Set.t
  val length : ('a, 'b) Base.Set.t -> int
  val is_empty : ('a, 'b) Base.Set.t -> bool
  val mem : ('a, 'b) Base.Set.t -> 'a -> bool
  val add : ('a, 'cmp) Base.Set.t -> 'a -> ('a, 'cmp) Base.Set.t
  val remove : ('a, 'cmp) Base.Set.t -> 'a -> ('a, 'cmp) Base.Set.t
  val union : ('a, 'cmp) Base.Set.t -> ('a, 'cmp) Base.Set.t -> ('a, 'cmp) Base.Set.t

  val union_list
    :  ('a, 'cmp) Base.Comparator.Module.t
    -> ('a, 'cmp) Base.Set.t list
    -> ('a, 'cmp) Base.Set.t

  val inter : ('a, 'cmp) Base.Set.t -> ('a, 'cmp) Base.Set.t -> ('a, 'cmp) Base.Set.t
  val diff : ('a, 'cmp) Base.Set.t -> ('a, 'cmp) Base.Set.t -> ('a, 'cmp) Base.Set.t

  val symmetric_diff
    :  ('a, 'cmp) Base.Set.t
    -> ('a, 'cmp) Base.Set.t
    -> ('a, 'a) Base.Either.t Base.Sequence.t

  val compare_direct : ('a, 'cmp) Base.Set.t -> ('a, 'cmp) Base.Set.t -> int
  val hash_fold_direct : 'a Base.Hash.folder -> ('a, 'cmp) Base.Set.t Base.Hash.folder
  val equal : ('a, 'cmp) Base.Set.t -> ('a, 'cmp) Base.Set.t -> bool
  val exists : ('a, 'b) Base.Set.t -> f:('a -> bool) -> bool
  val for_all : ('a, 'b) Base.Set.t -> f:('a -> bool) -> bool
  val count : ('a, 'b) Base.Set.t -> f:('a -> bool) -> int

  val sum
    :  (module Base.Container.Summable with type t = 'sum)
    -> ('a, 'b) Base.Set.t
    -> f:('a -> 'sum)
    -> 'sum

  val find : ('a, 'b) Base.Set.t -> f:('a -> bool) -> 'a option
  val find_map : ('a, 'c) Base.Set.t -> f:('a -> 'b option) -> 'b option
  val find_exn : ('a, 'b) Base.Set.t -> f:('a -> bool) -> 'a
  val nth : ('a, 'b) Base.Set.t -> int -> 'a option
  val remove_index : ('a, 'cmp) Base.Set.t -> int -> ('a, 'cmp) Base.Set.t
  val is_subset : ('a, 'cmp) Base.Set.t -> of_:('a, 'cmp) Base.Set.t -> bool
  val are_disjoint : ('a, 'cmp) Base.Set.t -> ('a, 'cmp) Base.Set.t -> bool

  module Named = Base.Set.Named

  val of_list : ('a, 'cmp) Base.Comparator.Module.t -> 'a list -> ('a, 'cmp) Base.Set.t

  val of_sequence
    :  ('a, 'cmp) Base.Comparator.Module.t
    -> 'a Base.Sequence.t
    -> ('a, 'cmp) Base.Set.t

  val of_array : ('a, 'cmp) Base.Comparator.Module.t -> 'a array -> ('a, 'cmp) Base.Set.t
  val to_list : ('a, 'b) Base.Set.t -> 'a list
  val to_array : ('a, 'b) Base.Set.t -> 'a array

  val of_sorted_array
    :  ('a, 'cmp) Base.Comparator.Module.t
    -> 'a array
    -> ('a, 'cmp) Base.Set.t Base.Or_error.t

  val of_sorted_array_unchecked
    :  ('a, 'cmp) Base.Comparator.Module.t
    -> 'a array
    -> ('a, 'cmp) Base.Set.t

  val of_increasing_iterator_unchecked
    :  ('a, 'cmp) Base.Comparator.Module.t
    -> len:int
    -> f:(int -> 'a)
    -> ('a, 'cmp) Base.Set.t

  val stable_dedup_list : ('a, 'b) Base.Comparator.Module.t -> 'a list -> 'a list

  val map
    :  ('b, 'cmp) Base.Comparator.Module.t
    -> ('a, 'c) Base.Set.t
    -> f:('a -> 'b)
    -> ('b, 'cmp) Base.Set.t

  val filter_map
    :  ('b, 'cmp) Base.Comparator.Module.t
    -> ('a, 'c) Base.Set.t
    -> f:('a -> 'b option)
    -> ('b, 'cmp) Base.Set.t

  val filter : ('a, 'cmp) Base.Set.t -> f:('a -> bool) -> ('a, 'cmp) Base.Set.t
  val fold : ('a, 'b) Base.Set.t -> init:'acc -> f:('acc -> 'a -> 'acc) -> 'acc

  val fold_result
    :  ('a, 'b) Base.Set.t
    -> init:'acc
    -> f:('acc -> 'a -> ('acc, 'e) result)
    -> ('acc, 'e) result

  val fold_until
    :  ('a, 'b) Base.Set.t
    -> init:'acc
    -> f:('acc -> 'a -> ('acc, 'final) Base.Continue_or_stop.t)
    -> finish:('acc -> 'final)
    -> 'final

  val fold_right : ('a, 'b) Base.Set.t -> init:'acc -> f:('a -> 'acc -> 'acc) -> 'acc
  val iter : ('a, 'b) Base.Set.t -> f:('a -> unit) -> unit

  val iter2
    :  ('a, 'cmp) Base.Set.t
    -> ('a, 'cmp) Base.Set.t
    -> f:([ `Both of 'a * 'a | `Left of 'a | `Right of 'a ] -> unit)
    -> unit

  val partition_tf
    :  ('a, 'cmp) Base.Set.t
    -> f:('a -> bool)
    -> ('a, 'cmp) Base.Set.t * ('a, 'cmp) Base.Set.t

  val elements : ('a, 'b) Base.Set.t -> 'a list
  val min_elt : ('a, 'b) Base.Set.t -> 'a option
  val min_elt_exn : ('a, 'b) Base.Set.t -> 'a
  val max_elt : ('a, 'b) Base.Set.t -> 'a option
  val max_elt_exn : ('a, 'b) Base.Set.t -> 'a
  val choose : ('a, 'b) Base.Set.t -> 'a option
  val choose_exn : ('a, 'b) Base.Set.t -> 'a

  val split
    :  ('a, 'cmp) Base.Set.t
    -> 'a
    -> ('a, 'cmp) Base.Set.t * 'a option * ('a, 'cmp) Base.Set.t

  val split_le_gt
    :  ('a, 'cmp) Base.Set.t
    -> 'a
    -> ('a, 'cmp) Base.Set.t * ('a, 'cmp) Base.Set.t

  val split_lt_ge
    :  ('a, 'cmp) Base.Set.t
    -> 'a
    -> ('a, 'cmp) Base.Set.t * ('a, 'cmp) Base.Set.t

  val group_by
    :  ('a, 'cmp) Base.Set.t
    -> equiv:('a -> 'a -> bool)
    -> ('a, 'cmp) Base.Set.t list

  val to_sequence
    :  ?order:[ `Decreasing | `Increasing ]
    -> ?greater_or_equal_to:'a
    -> ?less_or_equal_to:'a
    -> ('a, 'cmp) Base.Set.t
    -> 'a Base.Sequence.t

  val binary_search
    :  ('a, 'cmp) Base.Set.t
    -> compare:('a -> 'key -> int)
    -> [ `First_equal_to
       | `First_greater_than_or_equal_to
       | `First_strictly_greater_than
       | `Last_equal_to
       | `Last_less_than_or_equal_to
       | `Last_strictly_less_than
       ]
    -> 'key
    -> 'a option

  val binary_search_segmented
    :  ('a, 'cmp) Base.Set.t
    -> segment_of:('a -> [ `Left | `Right ])
    -> [ `First_on_right | `Last_on_left ]
    -> 'a option

  module Merge_to_sequence_element = Base.Set.Merge_to_sequence_element

  val merge_to_sequence
    :  ?order:[ `Decreasing | `Increasing ]
    -> ?greater_or_equal_to:'a
    -> ?less_or_equal_to:'a
    -> ('a, 'cmp) Base.Set.t
    -> ('a, 'cmp) Base.Set.t
    -> ('a, 'a) Merge_to_sequence_element.t Base.Sequence.t

  module M = Base.Set.M

  module type Sexp_of_m = Base.Set.Sexp_of_m
  module type M_of_sexp = Base.Set.M_of_sexp
  module type M_sexp_grammar = Base.Set.M_sexp_grammar
  module type Compare_m = Base.Set.Compare_m
  module type Equal_m = Base.Set.Equal_m
  module type Hash_fold_m = Base.Hasher.S

  val sexp_of_m__t
    :  (module Sexp_of_m with type t = 'elt)
    -> ('elt, 'cmp) Base.Set.t
    -> Base__Sexp.t

  val m__t_of_sexp
    :  (module M_of_sexp with type comparator_witness = 'cmp and type t = 'elt)
    -> Base__Sexp.t
    -> ('elt, 'cmp) Base.Set.t

  val m__t_sexp_grammar
    :  (module M_sexp_grammar with type t = 'elt)
    -> ('elt, 'cmp) Base.Set.t Sexplib0.Sexp_grammar.t

  val compare_m__t
    :  (module Compare_m)
    -> ('elt, 'cmp) Base.Set.t
    -> ('elt, 'cmp) Base.Set.t
    -> int

  val equal_m__t
    :  (module Equal_m)
    -> ('elt, 'cmp) Base.Set.t
    -> ('elt, 'cmp) Base.Set.t
    -> bool

  val hash_fold_m__t
    :  (module Hash_fold_m with type t = 'elt)
    -> Base_internalhash_types.state
    -> ('elt, 'a) Base.Set.t
    -> Base_internalhash_types.state

  val hash_m__t : (module Hash_fold_m with type t = 'elt) -> ('elt, 'a) Base.Set.t -> int

  module Poly = Base.Set.Poly
  module Using_comparator = Base.Set.Using_comparator

  val to_tree : ('a, 'cmp) Base.Set.t -> ('a, 'cmp) Using_comparator.Tree.t

  val of_tree
    :  ('a, 'cmp) Base.Comparator.Module.t
    -> ('a, 'cmp) Using_comparator.Tree.t
    -> ('a, 'cmp) Base.Set.t

  module With_comparator = Base.Map.With_comparator
  module With_first_class_module = Base.Map.With_first_class_module
  module Without_comparator = Base.Map.Without_comparator

  module type For_deriving = Base.Set.For_deriving
  module type S_poly = Base.Set.S_poly
  module type Accessors_generic = Base.Set.Accessors_generic
  module type Creators_generic = Base.Set.Creators_generic
  module type Creators_and_accessors_generic = Base.Set.Creators_and_accessors_generic
  module type Elt_plain = Base.Set.Elt_plain

  type t = (int, Base.Int.comparator_witness) Using_comparator.t

  val empty : (int, Base.Int.comparator_witness) Base.Set.t
end

module Type : sig
  val occurs_in : int -> Typedtree.typ -> bool
  val free_vars : Typedtree.typ -> (int, Base.Int.comparator_witness) Base.Set.t
end

module Subst : sig
  type t = (int, Typedtree.typ, Base.Int.comparator_witness) Base.Map.t

  val empty : (int, 'a, Base.Int.comparator_witness) Base.Map.t
  val singleton : int * 'a -> ((int, 'a, Base.Int.comparator_witness) Base.Map.t, 'b) R.t
  val apply : t -> Typedtree.typ -> Typedtree.typ

  val unify
    :  Typedtree.typ
    -> Typedtree.typ
    -> ((int, Typedtree.typ, Base.Int.comparator_witness) Base.Map.t, Errors.error) R.t

  val extend : t -> int * Typedtree.typ -> (t, Errors.error) R.t

  val compose
    :  (int, Typedtree.typ, Base.Int.comparator_witness) Base.Map.t
    -> (int, Typedtree.typ, Base.Int.comparator_witness) Base.Map.t
    -> ((int, Typedtree.typ, Base.Int.comparator_witness) Base.Map.t, Errors.error) R.t

  val compose_all
    :  (int, Typedtree.typ, Base.Int.comparator_witness) Base.Map.t list
    -> ((int, Typedtree.typ, Base.Int.comparator_witness) Base.Map.t, Errors.error) R.t
end

module Scheme : sig
  type t = S of VarSet.t * Typedtree.typ

  val free_vars : t -> (int, Base.Int.comparator_witness) Base.Set.t
  val apply : t -> (int, Typedtree.typ, Base.Int.comparator_witness) Base.Map.t -> t
end

module TypeEnv : sig
  type t = (string, Scheme.t, Base.String.comparator_witness) Base.Map.t

  val empty : (string, 'a, Base.String.comparator_witness) Base.Map.t

  val free_vars
    :  ('a, Scheme.t, 'b) Base.Map.t
    -> (int, Base.Int.comparator_witness) Base.Set.t

  val extend : ('a, 'b, 'c) Base.Map.t -> 'a * 'b -> ('a, 'b, 'c) Base.Map.t
end

val varf : (Typedtree.typ, 'a) R.t
val generalize : ('a, Scheme.t, 'b) Base.Map.t -> Typedtree.typ -> Scheme.t

val infer_pattern
  :  (string, Scheme.t, 'a) Base.Map.t
  -> Ast.pattern
  -> ((string, Scheme.t, 'a) Base.Map.t * Typedtree.typ, 'b) R.t

val lookup_env
  :  (string, Scheme.t, 'a) Base.Map.t
  -> string
  -> ((int, 'b, Base.Int.comparator_witness) Base.Map.t * Typedtree.typ, Errors.error) R.t

val inferencer
  :  (string, Scheme.t, 'a) Base.Map.t
  -> Ast.expression
  -> ( (int, Typedtree.typ, Base.Int.comparator_witness) Base.Map.t * Typedtree.typ
       , Errors.error )
       R.t

val infer
  :  (string, Scheme.t, 'a) Base.Map.t
  -> Ast.expression list
  -> ( (string, Scheme.t, 'a) Base.Map.t * (Ast.expression * Typedtree.typ) list
       , Errors.error )
       R.t

val infer_program
  :  ?env:(string, Scheme.t, Base.String.comparator_witness) Base.Map.t
  -> Ast.expression list
  -> ( (string, Scheme.t, Base.String.comparator_witness) Base.Map.t
       * (Ast.expression * Typedtree.typ) list
       , Errors.error )
       result
