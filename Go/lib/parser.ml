(** Copyright 2021-2023, Nikita Lukonenko *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(* TODO: implement parser here *)
open Angstrom
open Ast

let is_space = function
  | ' ' | '\t' | '\n' | '\r' -> true
  | _ -> false
;;

let spaces = take_while is_space
let space1 = take_while1 is_space

type error = [ `ParsingError of string ]

let pp_error ppf = function
  | `ParsingError s -> Format.fprintf ppf "%s" s
;;

let string_of_char c = String.make 1 c

let str_const =
  char '\"'
  *> take_till (function
    | '\"' -> true
    | _ -> false)
  <* char '\"'
  >>= fun res -> return (String res)
;;

let nil_const = string "nil" *> return (Const Nil) <|> return (Const Nil)
let true_const = string "true" *> return (Const (Bool true))
let false_const = string "false" *> return (Const (Bool false))
let bool_const = true_const <|> false_const
let parse_int_ident = string "int" *> return IntegerType
let parse_str_ident = string "string" *> return StringType
let parse_bool_ident = string "bool" *> return BooleanType
let parse_ident = parse_str_ident <|> parse_int_ident <|> parse_bool_ident
let semi = char ';'

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false
;;

let parse_int = take_while1 is_digit >>= fun r -> return (Integer (int_of_string r))

let identifier_parser =
  let is_ident_start = function
    | 'a' .. 'z' -> true
    | 'A' .. 'Z' -> true
    | '_' -> true
    | _ -> false
  in
  let is_ident_mid = function
    | 'a' .. 'z' -> true
    | '0' .. '9' | '_' -> true
    | _ -> false
  in
  satisfy is_ident_start
  >>= fun first ->
  take_while is_ident_mid >>= fun rest -> return (string_of_char first ^ rest)
;;

let parens p = char '(' *> spaces *> p <* spaces <* char ')'
let braces p = char '{' *> spaces *> p <* spaces <* char '}'
let keyword kw = string kw
let parse1 p s = parse_string ~consume:Prefix p s
let ident = identifier_parser >>= fun c -> return (Identifier c)
let spend_char c = option ' ' (char c) >>= fun _ -> return ()

let comma_separated p =
  sep_by (char ',' <* spaces) (p <* spaces)
  >>= fun ps -> spend_char ',' >>= fun _ -> return ps
;;

(* ***********************************TYPES*********************************** *)

let type_name_parse =
  identifier_parser
  >>= function
  | "int" -> return IntegerType
  | "string" -> return StringType
  | "bool" -> return BooleanType
  | _ -> return IntegerType
;;

let parse_typ = space1 *> type_name_parse >>= fun r -> return (Type r)
let parse_option_res = option Void parse_typ

let arg_decl =
  identifier_parser
  <* space1
  >>= fun name -> type_name_parse >>= fun typ -> return (name, typ)
;;

let parse_args = parens (comma_separated arg_decl)

let signature =
  parse_args >>= fun args -> parse_option_res >>= fun ret -> return { args; ret }
;;

type expr_declar_dispatch =
  { expression : expr_declar_dispatch -> expression t
  ; func_declaration : expr_declar_dispatch -> func_declaration t
  ; var_declaration : expr_declar_dispatch -> var_declaration t
  ; global_declaration : expr_declar_dispatch -> global_declaration t
  ; block : expr_declar_dispatch -> block t
  ; statements : expr_declar_dispatch -> statements t
  }

let chainl1 e op =
  let rec go acc = lift2 (fun f x -> f acc x) op e >>= go <|> return acc in
  e >>= fun init -> go init
;;

let chainl0 expr un_op = un_op >>= (fun op -> expr >>| fun exp -> op exp) <|> expr

type helper = Args of expression list

(* ***********************************EXPRESSIONS*********************************** *)
let ex_decl_disp =
  let parse_const_int = parse_int >>= fun r -> return (Const r) in
  let parse_const_string = str_const >>= fun r -> return (Const r) in
  (* let parse_const_nil = nil_value >>= fun r -> return(Const r) in *)
  let parse_anon_func d =
    keyword "func" *> spaces *> signature
    >>= fun signat ->
    spaces *> d.block d >>= fun block -> return (AnonFunc (signat, block))
  in
  let take_bin_op op x1 x2 = BinaryOp (x1, op, x2) in
  let take_un_op op x = UnaryOp (op, x) in
  let sub = spaces *> char '-' *> spaces *> return (take_bin_op MinusBin) in
  let add = spaces *> char '+' *> spaces *> return (take_bin_op Plus) in
  let div = spaces *> char '%' *> spaces *> return (take_bin_op Division) in
  let mul = spaces *> char '*' *> spaces *> return (take_bin_op Asterisk) in
  let eq = spaces *> string "==" *> spaces *> return (take_bin_op Equal) in
  let neq = spaces *> string "!=" *> spaces *> return (take_bin_op NotEqual) in
  let grt = spaces *> string ">" *> spaces *> return (take_bin_op Greater) in
  let grte = spaces *> string ">=" *> spaces *> return (take_bin_op GreaterEqual) in
  let less = spaces *> string "<" *> spaces *> return (take_bin_op Less) in
  let lesse = spaces *> string "<=" *> spaces *> return (take_bin_op LessEqual) in
  let and_ = spaces *> string "&&" *> spaces *> return (take_bin_op And) in
  let or_ = spaces *> string "||" *> spaces *> return (take_bin_op Or) in
  let not = spaces *> string "!" *> spaces *> return (take_un_op Not) in
  let minus = spaces *> string "-" *> spaces *> return (take_un_op MinusUn) in
  let expression d =
    fix (fun expression ->
      let parse_fun_call =
        ident
        >>= fun n ->
        match n with
        | Identifier "Print" ->
          parens (comma_separated expression) >>= fun a -> return (PrintCall a)
        | Identifier "len" ->
          parens parse_const_string >>= fun str -> return (LenCall str)
        | _ -> parens (comma_separated expression) >>= fun a -> return (FunCall (n, a))
      in
      let expr =
        parse_const_string
        <|> parse_const_int
        <|> parse_fun_call
        <|> parse_anon_func d
        <|> bool_const
        <|> ident
        <|> nil_const
        <|> parens expression
      in
      let unop = chainl0 expr (minus <|> not) in
      let binop_1 = chainl1 unop (div <|> mul) in
      let binop_2 = chainl1 binop_1 (add <|> sub) in
      let binop_3 = chainl1 binop_2 (eq <|> neq <|> grt <|> grte <|> less <|> lesse) in
      let binop_4 = chainl1 binop_3 and_ in
      chainl1 binop_4 or_)
  in
  (* ***********************************DECLARATIONS*********************************** *)
  let func_declaration d =
    keyword "func" *> spaces *> identifier_parser
    >>= fun n ->
    spaces *> signature >>= fun s -> spaces *> d.block d >>= fun b -> return (n, s, b)
  in
  let global_func_declaration d =
    fix @@ fun _ -> func_declaration d >>= fun r -> return (FuncTopLevelDeclaration r)
  in
  let var_declaration d =
    keyword "var" *> space1 *> identifier_parser
    >>= fun var_name ->
    spaces *> parse_ident
    >>= fun ident ->
    spaces
    *> (char '=' *> spaces *> expression d
        <* semi
        >>= fun val_ -> return (var_name, ident, val_))
    <|> (semi *> expression d >>= fun nil -> return (var_name, ident, nil))
  in
  let global_var_declaration d =
    fix @@ fun _ -> var_declaration d >>= fun r -> return (VarTopLevelDeclaration r)
  in
  let global_declaration d =
    fix @@ fun _ -> global_func_declaration d <|> global_var_declaration d
  in
  (* ***********************************STATEMENTS*********************************** *)
  let ret_stmt d =
    let ret_void = keyword "return" *> spaces *> semi *> return (ReturnStatement None) in
    let ret_expr =
      spaces *> keyword "return" *> space1 *> expression d
      <* spaces
      <* semi
      >>= fun expr -> return (ReturnStatement (Some expr))
    in
    ret_expr <|> ret_void <?> "ReturnStatement"
  in
  let block d = braces (many (spaces *> d.statements d <* spaces)) in
  let block_stmt d = block d >>= fun bl -> return (BlockStatement bl) in
  let if_stmt d =
    keyword "if" *> space1 *> expression d
    <* spaces
    >>= fun cond ->
    block d
    <* spaces
    >>= fun then_ ->
    peek_char_fail
    >>= fun char_ ->
    if char_ == 'e'
    then
      keyword "else" *> spaces *> block d
      >>= fun else_ -> return (IfElseStatement (cond, then_, else_))
    else return (IfElseStatement (cond, then_, []))
  in
  let for_stmt d =
    keyword "for" *> space1 *> expression d
    <* spaces
    >>= fun cond -> block d <* spaces >>= fun bl -> return (ForStatement (cond, bl))
  in
  let expr_stmt d =
    d.expression d >>= fun expr -> spaces *> semi *> return (ExpressinStatement expr)
  in
  let assign_stmt d =
    d.expression d
    <* spaces
    >>= fun l ->
    char '=' *> spaces *> d.expression d
    <* spaces
    <* semi
    >>= fun r -> return (AssignStatement (l, r))
  in
  let var_stmt d = var_declaration d >>= fun var -> return (VarStatement var) in
  let break_stmt = keyword "break" *> return BreakStatement in
  let continue_stmt = keyword "continue" *> return ContinueStatement in
  let statements d =
    fix
    @@ fun _ ->
    ret_stmt d
    <|> block_stmt d
    <|> expr_stmt d
    <|> var_stmt d
    <|> assign_stmt d
    <|> for_stmt d
    <|> if_stmt d
    <|> break_stmt
    <|> continue_stmt
  in
  { expression; func_declaration; var_declaration; global_declaration; block; statements }
;;

let expression = ex_decl_disp.expression ex_decl_disp
let func_decl = ex_decl_disp.func_declaration ex_decl_disp
let var_decl = ex_decl_disp.var_declaration ex_decl_disp
let global_declaration = ex_decl_disp.global_declaration ex_decl_disp
let block = ex_decl_disp.block ex_decl_disp
let stmt = ex_decl_disp.statements ex_decl_disp

let rec_top_level : file t =
  let global_decl_list =
    fix
    @@ fun top_level_decl_list ->
    at_end_of_input
    >>= fun finished ->
    if finished
    then return []
    else
      global_declaration
      <* spaces
      >>= fun d -> top_level_decl_list >>= fun tail -> return (d :: tail)
  in
  global_decl_list
;;

let full_parse s = parse1 rec_top_level s

let rec combine' f lst acc =
  match lst with
  | [] -> acc
  | h :: t -> f h (combine' f t acc)
;;

let sum lst = combine' ( + ) lst 0

let%test _ =
  Ok
    ( "fact"
    , { args = [ "x", IntegerType ]; ret = Type IntegerType }
    , [ IfElseStatement
          ( BinaryOp (Identifier "x", Equal, Const (Integer 0))
          , [ ReturnStatement (Some (Const (Integer 1))) ]
          , [] )
      ; ReturnStatement
          (Some
             (BinaryOp
                ( Identifier "x"
                , Asterisk
                , FunCall
                    ( Identifier "f"
                    , [ BinaryOp (Identifier "x", MinusBin, Const (Integer 1)) ] ) )))
      ] )
  = parse1
      func_decl
      {|func fact(x int) int {
           if x == 0 {
               return 1;
           }
           return x * f(x-1);}|}
;;

let test _ =
  parse1
    func_decl
    {|func fact(x int) int {
           if x == 0 {
               return 1;
           }
           else {return -1;}
           return x * f(x-1);}|}
  = Ok
      ( "fact"
      , { args = [ "x", IntegerType ]; ret = Type IntegerType }
      , [ IfElseStatement
            ( BinaryOp (Identifier "x", Equal, Const (Integer 0))
            , [ ReturnStatement (Some (Const (Integer 1))) ]
            , [ ReturnStatement (Some (UnaryOp (MinusUn, Const (Integer 1)))) ] )
        ; ReturnStatement
            (Some
               (BinaryOp
                  ( Identifier "x"
                  , Asterisk
                  , FunCall
                      ( Identifier "f"
                      , [ BinaryOp (Identifier "x", MinusBin, Const (Integer 1)) ] ) )))
        ] )
;;

let test _ =
  parse1
    func_decl
    {|func fact(x int) int {
           if x == 0 {
               for a < b {a = a + 1;}
               return 1;
           }
           return x * f(x-1);}|}
;;

Ok
  ( "fact"
  , { args = [ "x", IntegerType ]; ret = Type IntegerType }
  , [ IfElseStatement
        ( BinaryOp (Identifier "x", Equal, Const (Integer 0))
        , [ ForStatement
              ( BinaryOp (Identifier "a", Less, Identifier "b")
              , [ AssignStatement
                    (Identifier "a", BinaryOp (Identifier "a", Plus, Const (Integer 1)))
                ] )
          ; ReturnStatement (Some (Const (Integer 1)))
          ]
        , [] )
    ; ReturnStatement
        (Some
           (BinaryOp
              ( Identifier "x"
              , Asterisk
              , FunCall
                  ( Identifier "f"
                  , [ BinaryOp (Identifier "x", MinusBin, Const (Integer 1)) ] ) )))
    ] )
