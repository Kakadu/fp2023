open! Base

val parse : string -> (Ast.structure, string) result

val parse_exn : string -> Ast.structure
(** Raises [Failure] exception if parsing fails *)
