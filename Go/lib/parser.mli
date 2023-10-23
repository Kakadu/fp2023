val is_space : char -> bool
val spaces : unit Angstrom.t
val varname : char Angstrom.t
val conde : 'a Angstrom.t list -> 'a Angstrom.t

type dispatch =
  { apps : dispatch -> string Ast.t Angstrom.t
  ; single : dispatch -> string Ast.t Angstrom.t
  }

type error = [ `ParsingError of string ]

val pp_error : Format.formatter -> [< `ParsingError of string ] -> unit
val parse_lam : dispatch
val parse : string -> (string Ast.t, [> `ParsingError of string ]) result
val string_of_char : char -> string
val word_parser : string Angstrom.t
val parens : 'a Angstrom.t -> 'a Angstrom.t
val braces : 'a Angstrom.t -> 'a Angstrom.t
val keyword : string -> string Angstrom.t
val parse1 : 'a Angstrom.t -> string -> ('a, string) result
val parser_func_name : string Angstrom.t
val parse_type : Ast.constant -> Ast.type'
val parse_func_args : (string * Ast.type') Angstrom.t
val parse_func_ret_type : Ast.type' Angstrom.t
val parse_signature : Ast.signature Angstrom.t
val digit_c : char Angstrom.t
val digit : int Angstrom.t
val ident : Ast.expression Angstrom.t
val parse_cond : Ast.expression Angstrom.t
val parse_func_call : Ast.expression Angstrom.t
val parse_if : Ast.statements Angstrom.t
val parse_func_body : Ast.statements list Angstrom.t
val parse_func_declar : Ast.topLevelDeclaration Angstrom.t
