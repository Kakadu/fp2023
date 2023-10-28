val pp_error : Format.formatter -> [< `ParsingError of string ] -> unit

type error = [ `ParsingError of string ]

val chainl1 : Ast.term Angstrom.t -> Ast.atom Angstrom.t -> Ast.term Angstrom.t
val is_space : char -> bool
val string_of_spase : string Angstrom.t
val token : string -> string Angstrom.t
val dot : string Angstrom.t
val bar : string Angstrom.t
val in_bracket : 'a Angstrom.t -> 'a Angstrom.t
val sqr_brackets : 'a Angstrom.t -> 'a Angstrom.t
val in_quotes : 'a Angstrom.t -> 'a Angstrom.t
val parens : 'a Angstrom.t -> 'a Angstrom.t
val underscore : string Angstrom.t
val name_c : string -> Ast.atom
val num_c : int -> Ast.const
val atom_c : Ast.atom -> Ast.const
val var_c : string -> Ast.term
val oper_c : string -> Ast.atom
val comma_token : Ast.atom Angstrom.t
val semicolon : Ast.atom Angstrom.t
val sign_rule : Ast.atom Angstrom.t
val not : Ast.atom Angstrom.t
val eqq : Ast.atom Angstrom.t
val assign : Ast.atom Angstrom.t
val aryth_oper : Ast.atom Angstrom.t
val is_letter_number : char -> bool
val is_letter_cap : char -> bool
val is_letter_lower : char -> bool
val is_number : char -> bool
val isnot_letter_number : char -> bool
val is_anything : char -> bool
val atom : Ast.atom Angstrom.t
val const : Ast.const Angstrom.t
val var : Ast.term Angstrom.t
val relation : Ast.term Angstrom.t
val term : Ast.term Angstrom.t
val many_term_c : Ast.term list -> Ast.many_term
val parse_prolog : Ast.many_term Angstrom.t
val parse_program : string -> (Ast.many_term, [> `ParsingError of string ]) result
