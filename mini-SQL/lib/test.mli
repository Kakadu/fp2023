(** Run SQL-Parser on string *)
val run : 'a Angstrom.t -> string -> ('a, string) result

(** Compare parser output with example *)
val assert_equal : 'a Angstrom.t -> string -> 'a -> bool

(** Compare parser output with example and print result in case of error *)
val assert_eq_output :
  ('a -> string) -> 'a Angstrom.t -> string -> 'a -> bool

(** Return true if parser got error *)
val assert_raise : 'a Angstrom.t -> string -> bool
