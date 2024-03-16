type error =
  | Occurs_check
  | No_variable of string
  | Unification_failed of Typedtree.typ * Typedtree.typ

val pp_error : Format.formatter -> error -> unit
