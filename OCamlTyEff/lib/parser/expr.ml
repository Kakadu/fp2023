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
  skip_let_keyword
  *> lift3
       (fun rec_flag bindings exp -> Exp_let (rec_flag, bindings, exp))
       parse_rec_flag
       (parse_bindings pexp parse_pattern)
       (ws *> string "in" *> pexp)

let parse_expression =
  fix (fun pexp ->
      ws *> choice [parse_exp_let pexp; parse_exp_ident; parse_exp_const] )
