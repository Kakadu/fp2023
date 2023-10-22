open! Base
open Angstrom
open Ast
open Common
open Pattern

(* ======= Single expressions parsing ======= *)

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

(**
  [if E1 then E2 else E3 <optional>]

  todo: if precedence must be higher than ';'
*)
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

(* ======= Operators parsing ======= *)

let is_core_operator_char = function
  | '$' | '&' | '*' | '+' | '-' | '/' | '=' | '>' | '@' | '^' | '|' ->
      true
  | _ ->
      false

let is_operator_char = function
  | '~' | '!' | '?' | '%' | '<' | ':' | '.' ->
      true
  | _ as x when is_core_operator_char x ->
      true
  | _ ->
      false

type expr_infix_op =
  | IOpSeq
  | IOpList
  | IOpTuple
  | IOpApply
  | IOpCustom of ident

(** Try to peek infix operator. If nothing found return IOpApply *)
let peek_infix_op =
  let peek_list_operator =
    peek_string 2
    >>= fun s ->
    if String.equal s "::" then return {op= IOpList; op_length= 2}
    else fail "not a list operator"
  in
  let peek_seq_operator =
    peek_char_fail
    >>= fun c ->
    if Char.equal c ';' then return {op= IOpSeq; op_length= 1}
    else fail "not a seq operator"
  in
  let peek_tuple_operator =
    peek_char_fail
    >>= fun c ->
    if Char.equal c ',' then return {op= IOpTuple; op_length= 1}
    else fail "not a tuple operator"
  in
  let peek_custom_infix_op =
    (* (core-operator-char | % | <) { operator-char } *)
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
    lift2 String.( ^ ) peek_first (peek_rest "" 2)
    >>| fun id -> {op= IOpCustom (Ident id); op_length= String.length id}
  in
  option
    {op= IOpApply; op_length= 0} (* application operator is 0 chars long *)
    (choice
       [ peek_custom_infix_op
       ; peek_list_operator
       ; peek_seq_operator
       ; peek_tuple_operator ] )

(**
   Set precedence and associativity for operators.
   https://v2.ocaml.org/manual/expr.html#ss:precedence-and-associativity

   Used in Pratt parsing method
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

type expr_prefix_op = POpPlus | POpMinus | POpCustom of ident

let parse_prefix_op =
  let parse_prefix_plus = char '+' *> return POpPlus in
  let parse_prefix_minus = char '-' *> return POpMinus in
  let parse_custom_prefix_op =
    let parse_prefix1 =
      (* ! { operator-char } *)
      char '!' *> take_while is_operator_char >>| fun s -> "!" ^ s
    in
    let parse_prefix2 =
      (* (?|~) { operator-char }+ *)
      let parse_first =
        satisfy (fun c -> Char.equal '?' c || Char.equal '~' c)
        >>| String.of_char
      in
      let parse_rest = take_while1 is_operator_char in
      lift2 String.( ^ ) parse_first parse_rest
    in
    parse_prefix1 <|> parse_prefix2 >>| fun id -> POpCustom (Ident id)
  in
  parse_prefix_minus <|> parse_prefix_plus <|> parse_custom_prefix_op

let get_prefix_binding_power = function
  | POpPlus | POpCustom _ ->
      500 (* highest precedence *)
  | POpMinus ->
      95 (* a bit lower than application precedence *)

let parse_expression =
  let infix_fold_fun acc (op, rhs) =
    match op with
    | IOpApply ->
        Exp_apply (acc, rhs)
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
  let apply_prefix_op op exp =
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
  fix (fun pexp ->
      parse_infix_prefix ~parse_operand:(parse_single_exp pexp) ~peek_infix_op
        ~get_infix_binding_power ~infix_fold_fun ~parse_prefix_op
        ~get_prefix_binding_power ~apply_prefix_op )

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

let%expect_test "parse_seq_op" =
  pp pp_expression parse_expression "(a ; b) ; c ; d ; e" ;
  [%expect
    {|
    (Exp_sequence (
       (Exp_sequence ((Exp_ident (Ident "a")), (Exp_ident (Ident "b")))),
       (Exp_sequence ((Exp_ident (Ident "c")),
          (Exp_sequence ((Exp_ident (Ident "d")), (Exp_ident (Ident "e"))))))
       )) |}]

let%expect_test "parse_tuple_op" =
  pp pp_expression parse_expression "a, (b, c), d, e" ;
  [%expect
    {|
    (Exp_tuple
       [(Exp_ident (Ident "a"));
         (Exp_tuple [(Exp_ident (Ident "b")); (Exp_ident (Ident "c"))]);
         (Exp_ident (Ident "d")); (Exp_ident (Ident "e"))]) |}]

let%expect_test "parse_plus_minus_prefix_op" =
  pp pp_expression parse_expression "1 + - + + 3" ;
  [%expect
    {|
    (Exp_apply (
       (Exp_apply ((Exp_ident (Ident "+")), (Exp_constant (Const_integer 1)))),
       (Exp_apply ((Exp_ident (Ident "~-")),
          (Exp_apply ((Exp_ident (Ident "~+")),
             (Exp_apply ((Exp_ident (Ident "~+")),
                (Exp_constant (Const_integer 3))))
             ))
          ))
       )) |}]

let%expect_test "parse_custom_prefix_op" =
  pp pp_expression parse_expression "?!5; !%< 123; !0; ~-3" ;
  [%expect
    {|
    (Exp_sequence (
       (Exp_apply ((Exp_ident (Ident "?!")), (Exp_constant (Const_integer 5)))),
       (Exp_sequence (
          (Exp_apply ((Exp_ident (Ident "!%<")),
             (Exp_constant (Const_integer 123)))),
          (Exp_sequence (
             (Exp_apply ((Exp_ident (Ident "!")),
                (Exp_constant (Const_integer 0)))),
             (Exp_apply ((Exp_ident (Ident "~-")),
                (Exp_constant (Const_integer 3))))
             ))
          ))
       )) |}]
