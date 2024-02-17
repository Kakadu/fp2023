type error =
  | Occurs_check of int * Typedtree.typ
  | Unbound_value of string
  | Ivalid_format_concat of Typedtree.typ * Typedtree.typ

val pp_error : Format.formatter -> error -> unit
