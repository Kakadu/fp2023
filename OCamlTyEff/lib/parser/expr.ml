open! Base
open Angstrom
open Ast
open Common
open Pattern

let parse_exp_ident = parse_value_name >>| fun name -> Exp_ident (Ident name)

let parse_exp_const = parse_const >>| fun const -> Exp_constant const

(**
  [let P1 = E1 and P2 = E2 and ... and Pn = En in E]
  [let rec P1 PArg1 = E1 and P2 = E2 and ... and Pn = En in E]
*)
let parse_exp_let pexp =
  lift2
    (fun (rec_flag, bindings) exp -> Exp_let (rec_flag, bindings, exp))
    (parse_let_binding pexp parse_pattern)
    (ws *> string "in" *> pexp)

let parse_single_exp pexp =
  ws
  *> choice
       [ char '(' *> pexp <* ws <* char ')'
       ; parse_exp_let pexp
       ; parse_exp_ident
       ; parse_exp_const ]

let parse_expression =
  fix (fun pexp ->
      sep_by1 ws (parse_single_exp pexp)
      >>| function
      | h :: [] -> h | h :: tl -> Exp_apply (h, tl) | [] -> assert false )
