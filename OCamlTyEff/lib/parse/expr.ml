(** Copyright 2023, Danil S, Andrei *)

(** SPDX-License-Identifier: MIT *)

open! Base
open Angstrom
open Ast
open Common
open Pattern

(* ======= Single expressions parsing ======= *)

let parse_exp_ident = parse_value_name >>| fun name -> Exp_ident (Ident name)

let parse_exp_const = parse_const >>| fun const -> Exp_constant const

let parse_exp_constr =
  parse_constr_name >>| fun name -> Exp_construct (Ident name, None)

(**
  [let P1 = E1 and P2 = E2 and ... in E]
  [let rec ValName1 PArg1 = E1 and P1 = E2 and ... in E]
*)
let parse_exp_let pexp =
  lift2
    (fun (rec_flag, bindings) exp -> Exp_let (rec_flag, bindings, exp))
    (parse_let_binding pexp parse_pattern)
    (ws *> string "in" *> pexp)

(** [if E1 then E2 else E3 <optional>] *)
let parse_exp_ite pexp_if pexp_thenelse =
  lift3
    (fun c t e -> Exp_ifthenelse (c, t, e))
    (string "if" *> pexp_if)
    (ws *> string "then" *> pexp_thenelse)
    (* None if else not found *)
    ( option None (ws *> string "else" >>| Option.some)
    >>= function None -> return None | Some _ -> pexp_thenelse >>| Option.some
    )

(** [a; b; c] *)
let parse_exp_list pexp =
  let parse_list =
    sep_by (ws *> char ';') pexp
    >>| fun list ->
    let rec helper = function
      | h :: tl ->
          Exp_construct (Ident "::", Some (Exp_tuple [h; helper tl]))
      | [] ->
          Exp_construct (Ident "[]", None)
    in
    helper list
  in
  char '[' *> parse_list <* ws <* char ']'

let parse_match_cases pexp =
  let parse_case =
    lift2
      (fun pat exp -> {left= pat; right= exp})
      (parse_pattern <* ws <* string "->" <* ws)
      pexp
  in
  option () (char '|' *> return ()) (* skip | if there *)
  *> sep_by1 (ws *> char '|' *> ws) parse_case

(** [match E0 with P1 -> E1 | ... | Pn -> En] *)
let parse_exp_match pexp_match pexp_with =
  lift2
    (fun exp cases -> Exp_match (exp, cases))
    (string "match" *> pexp_match)
    (ws *> string "with" *> ws *> parse_match_cases pexp_with)

(** [fun P1 ... Pn -> E] *)
let parse_exp_fun pexp =
  lift2
    (fun args exp -> Exp_fun (args, exp))
    (string "fun" *> sep_by1 ws parse_pattern)
    (ws *> string "->" *> pexp)

(** [function P1 -> E1 | ... | Pn -> En] *)
let parse_exp_function pexp =
  string "function" *> ws *> parse_match_cases pexp
  >>| fun cases -> Exp_function cases

(* ======= Operators parsing ======= *)

type expr_infix_op =
  | IOpSeq
  | IOpList
  | IOpTuple
  | IOpApply
  | IOpCustom of Ident.t
[@@deriving eq]

(**
   Try to peek infix operator. If nothing found return IOpApply.
   Fail if [disabled_op] found
*)
let peek_infix_op disabled_op =
  let peek_1char_op =
    peek_char_fail
    >>= function
    | ';' ->
        return {op= IOpSeq; op_length= 1}
    | ',' ->
        return {op= IOpTuple; op_length= 1}
    | _ ->
        fail "not an infix operator"
  in
  let peek_2chars_op =
    peek_string 2
    >>= function
    | "::" ->
        return {op= IOpList; op_length= 2}
    | _ ->
        fail "not an infix operator"
  in
  let peek_custom_op =
    peek_custom_infix_operator_name
    >>| fun name -> {op= IOpCustom (Ident name); op_length= String.length name}
  in
  option
    (* default to application operator which is 0 chars long *)
    {op= IOpApply; op_length= 0}
    ( choice [peek_custom_op; peek_1char_op; peek_2chars_op]
    >>= fun op ->
    (* fail if disabled_op was passed and found *)
    Option.value_map disabled_op ~default:(return op) ~f:(fun disabled_op ->
        if equal_expr_infix_op disabled_op op.op then fail "operator disabled"
        else return op ) )

(**
   Set precedence and associativity for infix operators
   https://v2.ocaml.org/manual/expr.html#ss:precedence-and-associativity
*)
let get_infix_binding_power = function
  | IOpApply ->
      (100, 101)
  | IOpList ->
      (81, 80)
  | IOpSeq ->
      (11, 10)
  | IOpTuple ->
      (51, 50)
  | IOpCustom (Ident id) ->
      let is_prefix prefix = String.is_prefix ~prefix id in
      let is_equal str = String.( = ) id str in
      if is_prefix "**" then (91, 90)
      else if is_prefix "*" || is_prefix "/" || is_prefix "%" then (85, 86)
      else if is_prefix "+" || is_prefix "-" then (83, 84)
      else if is_prefix "@" || is_prefix "^" then (79, 78)
      else if
        is_prefix "=" || is_prefix "<" || is_prefix ">" || is_prefix "|"
        || (is_prefix "&" && not (is_equal "&"))
        || is_prefix "$" || is_equal "!="
      then (75, 76)
      else if is_equal "&" || is_equal "&&" then (71, 70)
      else if is_equal "||" then (66, 65)
      else assert false

type expr_prefix_op = POpPlus | POpMinus | POpCustom of Ident.t

let parse_prefix_op =
  let parse_1char_op =
    char '+' *> return POpPlus <|> char '-' *> return POpMinus
  in
  let parse_custom_op =
    parse_custom_prefix_operator_name >>| fun name -> POpCustom (Ident name)
  in
  choice [parse_1char_op; parse_custom_op]

(** Set precedence and associativity for prefix operators *)
let get_prefix_binding_power = function
  | POpPlus | POpCustom _ ->
      500 (* highest precedence *)
  | POpMinus ->
      95 (* a bit lower than application precedence *)

let parse_single_exp pexp =
  ws
  *> choice
       [ parse_exp_ident
       ; parse_exp_const
       ; parse_exp_constr
       ; char '(' *> pexp None <* ws <* char ')'
       ; parse_exp_function (pexp (Some (IOpCustom (Ident "|"))))
         (* disable | as it's used as cases separator *)
       ; parse_exp_fun (pexp None)
       ; parse_exp_list (pexp (Some IOpSeq))
         (* disable ; as it's a separator in lists *)
       ; parse_exp_let (pexp None)
       ; parse_exp_ite (pexp None) (pexp (Some IOpSeq))
         (* disable ; in [then] and [else] blocks to maintain correct precedence *)
       ; parse_exp_match (pexp None) (pexp (Some (IOpCustom (Ident "|"))))
         (* disable | in [with] block as it's used as cases separator *) ]

let parse_expression =
  let rec pexp disabled_op =
    let fold_infix acc (op, rhs) =
      match op with
      | IOpApply -> (
        match acc with
        | Exp_construct (id, None) ->
            (* constructor application *)
            Exp_construct (id, Some rhs)
        | _ ->
            (* function application *)
            Exp_apply (acc, rhs) )
      | IOpList ->
          Exp_construct (Ident "::", Some (Exp_tuple [acc; rhs]))
      | IOpSeq ->
          Exp_sequence (acc, rhs)
      | IOpTuple -> (
        match rhs with
        | Exp_tuple l ->
            Exp_tuple (acc :: l)
        | _ ->
            Exp_tuple [acc; rhs] )
      | IOpCustom op ->
          Exp_apply (Exp_apply (Exp_ident op, acc), rhs)
    in
    let apply_prefix op exp =
      Exp_apply
        ( Exp_ident
            ( match op with
            | POpMinus ->
                Ident "~-"
            | POpPlus ->
                Ident "~+"
            | POpCustom id ->
                id )
        , exp )
    in
    fix (fun _ ->
        parse_operators
          ~infix:
            { peek= peek_infix_op disabled_op
            ; get_binding_power= get_infix_binding_power
            ; fold= fold_infix }
          ~prefix:
            { parse= parse_prefix_op
            ; get_binding_power= get_prefix_binding_power
            ; apply= apply_prefix }
          (parse_single_exp pexp) )
  in
  pexp None
