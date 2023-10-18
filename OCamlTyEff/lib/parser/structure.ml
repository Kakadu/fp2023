open! Base
open Angstrom
open Ast
open Common
open Expr
open Pattern

(**
  [let P1 = E1 and P2 = E2 and ... and Pn = En]
  [let rec P1 PArg1 = E1 and P2 = E2 and ... and Pn = En]
*)
let parse_str_let =
  skip_let_keyword
  *> lift2
       (fun rec_flag bindings -> Str_value (rec_flag, bindings))
       parse_rec_flag
       (parse_bindings parse_expression parse_pattern)

let parse_structure : structure t =
  let parse_structure_item =
    parse_str_let <|> (parse_expression >>| fun e -> Str_eval e)
  in
  sep_by ws parse_structure_item <* ws
