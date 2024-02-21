type error =
  [ `No_variable of string
  | `Occurs_check
  | `Unexpected_expression
  | `Unexpected_pattern
  | `Unification_failed of Typedtree.ty * Typedtree.ty
  ]

val pp_error : Format.formatter -> error -> unit

module VarSet : sig
  type t = Set.Make(Base.Int).t
end

type scheme = S of VarSet.t * Typedtree.ty

val run_infer
  :  Ast.structure
  -> ((string, scheme, Base.String.comparator_witness) Base.Map.t, error) result

val test_infer : string -> unit
