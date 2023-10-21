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

(** [if E1 then E2 else E3 <optional>] *)
let parse_exp_ite pexp =
  lift3
    (fun c t e -> Exp_ifthenelse (c, t, e))
    (string "if" *> pexp)
    (ws *> string "then" *> pexp)
    ( option None (ws *> string "else" >>| Option.some)
    >>= function None -> return None | Some _ -> pexp >>| Option.some )

let parse_single_exp pexp =
  ws
  *> choice
       [ parse_exp_ident
       ; parse_exp_const
       ; char '(' *> pexp <* ws <* char ')'
       ; parse_exp_let pexp
       ; parse_exp_ite pexp ]

type operator = OpList | OpApply | OpCustom of ident

let peek_custom_operator =
  let is_core_operator_char = function
    | '$' | '&' | '*' | '+' | '-' | '/' | '=' | '>' | '@' | '^' | '|' ->
        true
    | _ ->
        false
  in
  let is_operator_char x =
    match x with
    | '~' | '!' | '?' | '%' | '<' | ':' | '.' ->
        true
    | _ as x when is_core_operator_char x ->
        true
    | _ ->
        false
  in
  let peek_first =
    peek_char_fail
    >>= fun x ->
    if is_core_operator_char x || Char.equal x '%' || Char.equal x '<' then
      return (String.of_char x)
    else fail "not a infix-symbol"
  in
  let rec peek_rest acc index =
    peek_string index
    >>| (fun s -> String.get s (String.length s - 1)) (* get last char *)
    >>= fun c ->
    if is_operator_char c then peek_rest (acc ^ String.of_char c) (index + 1)
    else return acc
  in
  option
    (OpApply, 0) (* application operator is 0 chars long *)
    ( lift2 String.( ^ ) peek_first (peek_rest "" 2)
    >>| fun x -> (OpCustom (Ident x), String.length x) )

(** Returns operator and its length *)
let peek_operator =
  let peek_list_operator =
    peek_string 2
    >>= fun s ->
    if String.equal s "::" then return (OpList, 2)
    else fail "not a list operator"
  in
  peek_list_operator <|> peek_custom_operator

(** Set precedence and associativity for operators. Used in Pratt parsing method *)
let get_binding_power = function
  | OpApply ->
      (100, 101)
  | OpList ->
      (81, 80)
  | OpCustom (Ident id) ->
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

let parse_expression =
  (*
     Pratt parsing
     https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html
  *)
  let rec helper min_bp pexp =
    let* lhs = parse_single_exp pexp in
    many
      (let* op, op_len = ws *> peek_operator in
       let l_bp, r_bp = get_binding_power op in
       if l_bp < min_bp then fail "found op with lower binding power"
       else
         advance op_len
         *> let* rhs = helper r_bp pexp in
            return (rhs, op) )
    >>| fun results ->
    List.fold_left ~init:lhs
      ~f:(fun acc (rhs, op) ->
        match op with
        | OpApply ->
            Exp_apply (acc, rhs)
        | OpList ->
            Exp_construct (Ident "::", Some (Exp_tuple [acc; rhs]))
        | OpCustom op ->
            Exp_apply (Exp_apply (Exp_ident op, acc), rhs) )
      results
  in
  fix (fun pexp -> helper 0 pexp)

(* ======= Tests ======= *)

let%expect_test "parse_exp_let" =
  pp pp_expression
    (parse_exp_let parse_expression)
    "let rec a = 1 and b = 2 in let e = 3 in a" ;
  [%expect
    {|
    (Exp_let (Recursive,
       [{ pat = (Pat_var "a"); expr = (Exp_constant (Const_integer 1)) };
         { pat = (Pat_var "b"); expr = (Exp_constant (Const_integer 2)) }],
       (Exp_let (Nonrecursive,
          [{ pat = (Pat_var "e"); expr = (Exp_constant (Const_integer 3)) }],
          (Exp_ident (Ident "a"))))
       )) |}]

let%expect_test "parse_exp_ifthenelse" =
  pp pp_expression
    (parse_exp_ite parse_expression)
    "if a then (if b then c) else d" ;
  [%expect
    {|
    (Exp_ifthenelse ((Exp_ident (Ident "a")),
       (Exp_ifthenelse ((Exp_ident (Ident "b")), (Exp_ident (Ident "c")), None)),
       (Some (Exp_ident (Ident "d"))))) |}]

let%expect_test "parse_list_op" =
  pp pp_expression parse_expression "(a :: b) :: c :: d :: e" ;
  [%expect
    {|
    (Exp_construct ((Ident "::"),
       (Some (Exp_tuple
                [(Exp_construct ((Ident "::"),
                    (Some (Exp_tuple
                             [(Exp_ident (Ident "a")); (Exp_ident (Ident "b"))]))
                    ));
                  (Exp_construct ((Ident "::"),
                     (Some (Exp_tuple
                              [(Exp_ident (Ident "c"));
                                (Exp_construct ((Ident "::"),
                                   (Some (Exp_tuple
                                            [(Exp_ident (Ident "d"));
                                              (Exp_ident (Ident "e"))]))
                                   ))
                                ]))
                     ))
                  ]))
       )) |}]
