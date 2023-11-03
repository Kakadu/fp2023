(** Copyright 2021-2023, Georgy Sichkar *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(* TODO: implement parser here *)

open Base
open Angstrom
open Ast

let chainl0 expr un_op = un_op >>= (fun op -> expr >>| fun exp -> op exp) <|> expr

let chainl1 expr bin_op =
  let rec epars e1 =
    lift2 (fun b_op e2 -> b_op e1 e2) bin_op expr >>= epars <|> return e1
  in
  expr >>= fun init -> epars init
;;

let chainr1 exp op =
  fix (fun foo ->
    lift2 (fun e1_op e2 -> e1_op e2) (lift2 (fun e1 bin_op -> bin_op e1) exp op) foo
    <|> exp)
;;

let is_type_as_keyword = function
  | "int" | "char" | "string" | "bool" -> true
  | _ -> false
;;

(** @see <https://learn.microsoft.com/en-us/dotnet/csharp/language-reference/keywords/>
      C# keywords *)
let is_keyword = function
  | "if"
  | "else"
  | "while"
  | "for"
  | "break"
  (*  *)
  | "class"
  | "new"
  | "return"
  | "base"
  (*  *)
  | "public"
  | "private"
  | "protected"
  | "static"
  | "override"
  | "const"
  (*  *)
  | "try"
  | "catch"
  | "finally"
  | "when"
  (*  *)
  | "null"
  | "void"
  | "true"
  | "false" -> true
  | tp -> is_type_as_keyword tp
;;

let is_space = function
  | ' ' | '\t' | '\n' | '\r' -> true
  | _ -> false
;;

let skip_spaces = take_while is_space
let skip_spaces1 = take_while1 is_space

let is_token = function
  | 'a' .. 'z' | '0' .. '9' | 'A' .. 'Z' | '_' -> true
  | _ -> false
;;

let if_value cond x =
  try cond x |> fun _ -> true with
  | Failure _ | Invalid_argument _ -> false
;;

let is_int = if_value int_of_string
let is_bool = if_value bool_of_string

let is_nullable = function
  | '?' -> true
  | _ -> false
;;

let s_token = take_while1 is_token

let read_as_token kw =
  s_token
  >>= function
  | x when String.( = ) x kw -> return kw
  | _ -> fail ("Not a " ^ kw)
;;

let s_string =
  char '\"'
  *> take_till (function
    | '\"' -> true
    | _ -> false)
  <* char '\"'
  <|> fail "Not a string"
;;

let p_string = s_string >>= fun str -> return (VString str)
let s_char = char '\'' *> any_char <* char '\'' <|> fail "Not a char"
let p_char = s_char >>= fun c -> return (VChar c)

let p_number =
  s_token
  >>= fun str ->
  match is_int str with
  | true -> return (VInt (int_of_string str))
  | false -> fail "Not a number"
;;

let p_bool =
  s_token
  >>= fun str ->
  match is_bool str with
  | true -> return (VBool (bool_of_string str))
  | false -> fail "Not a bool"
;;

let p_ident =
  s_token
  >>= fun str ->
  match not (is_keyword str) with
  | true when not (Char.is_digit str.[0]) -> return (Id str)
  | _ -> fail "Not an ident"
;;

let base_type_converter = function
  | "int" -> return TInt
  | "char" -> return TChar
  | "bool" -> return TBool
  | _ -> fail "Not a keyword type (e.g. int)"
;;

let s_keyword =
  s_token
  >>= fun tp ->
  match is_type_as_keyword tp with
  | true -> return tp
  | false -> fail "Not a keyword type (e.g. int)"
;;

let s_is_nullable = skip_spaces *> char '?' *> return true <|> return false

let p_keyword_type =
  s_keyword
  >>= function
  | "string" -> return (TNullable TString)
  | base_type ->
    s_is_nullable
    >>= (function
     | true -> base_type_converter base_type >>| fun x -> TNullable (TBase x)
     | false -> base_type_converter base_type >>| fun x -> TNot_Nullable x)
;;

let ( #~> ) str tp = read_as_token str *> return tp

let p_access_modifier =
  choice
    ?failure_msg:(Some "Not a access modifier")
    [ "public" #~> MPublic; "private" #~> MPrivate; "protected" #~> MProtected ]
;;

let p_method_modifier =
  choice
    ?failure_msg:(Some "Not a method modifier")
    [ "static" #~> MStatic; (p_access_modifier >>= fun x -> return (MAccess x)) ]
;;

let p_fild_modifier = p_access_modifier >>= fun x -> return (FAccess x)
let p_kvar_type = p_keyword_type >>= fun x -> return (TVar x)

let p_method_type =
  choice
    ?failure_msg:(Some "Not a method_type")
    [ (p_keyword_type >>= fun x -> return (TReturn x)); "void" #~> Void ]
;;

let ep_spaces prs = skip_spaces *> prs
let ep_parens prs = ep_spaces @@ (char '(' *> prs) <* ep_spaces @@ char ')'
let ep_figure_parens prs = ep_spaces @@ (char '{' *> prs) <* ep_spaces @@ char '}'
let convert_val_to_expr prs = ep_spaces prs >>| fun x -> EConst x
let ep_number = convert_val_to_expr p_number
let ep_char = convert_val_to_expr p_char
let ep_string = convert_val_to_expr p_string
let ep_bool = convert_val_to_expr p_bool
let ep_identifier = ep_spaces p_ident >>= fun x -> return (EIdentifier x)

let ep_value =
  choice ?failure_msg:(Some "Not a value") [ ep_bool; ep_char; ep_number; ep_string ]
;;

let ep_dot = ep_spaces @@ (char '.' *> return (fun e1 e2 -> EMember_ident (e1, e2)))
let ep_member_ident = chainr1 ep_identifier ep_dot

let ep_list_from_ ep_arg =
  let ep_args = ep_arg <* ep_spaces @@ char ',' <|> ep_arg in
  ep_parens @@ many ep_args
;;

let ep_invoke_ meth_ident ep_arg =
  ep_list_from_ ep_arg
  >>= (fun exp -> return (Params exp))
  >>| fun args -> EMethod_invoke (meth_ident, args)
;;

let ep_method_invoke_ ep_arg = ep_member_ident >>= fun id -> ep_invoke_ id ep_arg

let ep_method_fild_ ep_arg =
  ep_member_ident >>= fun id -> ep_invoke_ id ep_arg <|> return id
;;

let ep_var_decl_ tp = skip_spaces1 *> p_ident >>| fun id -> EDecl (TVariable tp, id)

let ep_var_type_ =
  ep_spaces
  @@ choice
       ?failure_msg:(Some "Not a var declaration")
       [ p_kvar_type; (p_ident >>| fun cl -> TVar (TNullable (TClass cl))) ]
;;

let ep_var_decl =
  ep_spaces
  @@ choice
       ?failure_msg:(Some "Not a var declaration")
       [ p_kvar_type; (p_ident >>| fun cl -> TVar (TNullable (TClass cl))) ]
  >>= ep_var_decl_
;;

let ( |-> ) op tp = ep_spaces @@ (string op *> return tp)
let ( =>| ) op tp = op |-> tp >>| fun tp a -> EUn_op (tp, a)
let ( ==>| ) op tp = op |-> tp >>| fun t a b -> EBin_op (t, a, b)
let ( +^ ) = "+" ==>| Plus
let ( *^ ) = "*" ==>| Asterisk
let ( -^ ) = "-" ==>| Minus
let ( /^ ) = "/" ==>| Division
let ( %^ ) = "%" ==>| Mod
let ( ==^ ) = "==" ==>| Equal
let ( !=^ ) = "!=" ==>| NotEqual
let ( <^ ) = "<" ==>| Less
let ( <=^ ) = "<=" ==>| LessOrEqual
let ( >^ ) = ">" ==>| More
let ( >=^ ) = ">=" ==>| MoreOrEqual
let ( &&^ ) = "&&" ==>| And
let ( ||^ ) = "||" ==>| Or
let ( =^ ) = "=" ==>| Assign
let ep_un_minus = "-" =>| UMinus
let ep_not = "!" =>| UNot
let ep_new = "new" =>| New
let ( >- ) lvl ps_list = chainl0 lvl (choice ps_list)
let ( >>- ) lvl ps_list = chainl1 lvl (choice ps_list)
let ( -<< ) lvl ps_list = chainr1 lvl (choice ps_list)

let ep_operation =
  fix (fun expr ->
    let lvl1 = choice [ ep_parens expr; ep_value; ep_method_fild_ expr ] in
    let lvl2 = lvl1 >- [ ep_un_minus; ep_new; ep_not ] in
    let lvl3 = lvl2 >>- [ ( *^ ); ( /^ ); ( %^ ) ] in
    let lvl4 = lvl3 >>- [ ( +^ ); ( -^ ) ] in
    let lvl5 = lvl4 >>- [ ( <=^ ); ( >=^ ); ( <^ ); ( >^ ) ] in
    let lvl6 = lvl5 >>- [ ( ==^ ); ( !=^ ) ] in
    let lvl7 = lvl6 >>- [ ( &&^ ) ] in
    let lvl8 = lvl7 >>- [ ( ||^ ) ] in
    lvl8 -<< [ ( =^ ) ])
;;

let ep_assign = lift3 (fun ident eq ex -> eq ident ex) ep_identifier ( =^ ) ep_operation
let ep_method_invoke = ep_method_invoke_ ep_operation

let ep_eAssign_eDecl =
  choice
    ?failure_msg:(Some "Not a declaration or assignment")
    [ lift2
        (fun decl value -> EAssign (decl, value))
        ep_var_decl
        (ep_spaces (char '=') *> ep_operation)
    ; ep_var_decl
    ]
;;

let ep_keyword_ kw = ep_spaces @@ read_as_token kw
let ep_break = ep_keyword_ "break" *> return EBreak

let ep_return =
  lift2
    (fun _ ex -> EReturn ex)
    (ep_keyword_ "return")
    (ep_operation >>= (fun x -> return (Some x)) <|> return None)
;;

let ep_is_ kw ~then_:ps = ep_keyword_ kw *> ps

let ep_if_cond_ =
  let p_cond = ep_parens ep_operation in
  ep_is_ "if" ~then_:p_cond
;;

let ep_else_cond_ ep_body ep_ifls =
  choice
    ?failure_msg:(Some "It isn't ELSE or ELSE IF")
    [ ep_is_ "else" ~then_:ep_ifls; ep_is_ "else" ~then_:ep_body ]
  >>= (fun else_ -> return (Some else_))
  <|> return None
;;

let ep_if_else_ ep_body =
  fix (fun if_else ->
    let else_ = ep_else_cond_ ep_body if_else in
    lift3 (fun cond body else_ -> EIf_else (cond, body, else_)) ep_if_cond_ ep_body else_)
;;

let ep_brunch_loop_ ep_body =
  choice ?failure_msg:(Some "It isn't IF or ...") [ ep_if_else_ ep_body ]
;;

let ep_skip_semicolon_ = ep_spaces @@ char ';'

let ep_semicolon1_ ps =
  ps <* ep_skip_semicolon_ *> fix (fun foo -> ep_skip_semicolon_ *> foo <|> return "")
;;

let ep_semicolon_ ps = ep_semicolon1_ ps <|> ps

let ep_steps =
  fix (fun steps ->
    let step = ep_semicolon1_ in
    let op_step = ep_semicolon_ in
    let body_step =
      choice
        [ step ep_eAssign_eDecl
        ; step ep_method_invoke
        ; step ep_assign
        ; step ep_break
        ; step ep_return
        ; op_step @@ ep_brunch_loop_ steps
        ]
    in
    ep_figure_parens @@ many body_step >>| fun bd -> Steps bd)
;;

let ep_brunch_loop = ep_brunch_loop_ ep_steps
let ep_args_ = ep_list_from_ ep_var_decl >>= fun exp -> return (Args exp)
let ep_modifier_ p_mod = option None (ep_spaces p_mod >>= fun x -> return (Some x))

let ep_method_sign =
  lift4
    (fun m_modif m_type m_id m_args -> { m_modif; m_type; m_id; m_args })
    (ep_modifier_ p_method_modifier)
    (ep_spaces p_method_type)
    (ep_spaces p_ident)
    ep_args_
;;

let ep_constructor_sign =
  let ep_base_cons =
    ep_spaces @@ (char ':' *> ep_is_ "base" ~then_:(ep_list_from_ ep_operation))
    >>| fun x -> Some (Params x)
  in
  lift4
    (fun con_modif con_id con_args base_params ->
      { con_modif; con_id; con_args; base_params })
    (ep_modifier_ p_access_modifier)
    (ep_spaces p_ident)
    ep_args_
    (option None ep_base_cons)
;;

let ep_fild_sign =
  let f_value = ep_spaces (char '=') *> ep_operation >>| fun x -> Some x in
  lift4
    (fun f_modif f_type f_id f_val -> { f_modif; f_type; f_id; f_val })
    (ep_modifier_ p_fild_modifier)
    (ep_spaces ep_var_type_)
    (ep_spaces p_ident)
    (option None f_value)
  <* ep_spaces @@ char ';'
;;

let is_main (m_sign : method_sign) =
  match m_sign.m_id with
  | Id "Main" -> true
  | _ -> false
;;

let ep_method_member =
  ep_method_sign
  >>= function
  | mt when is_main mt -> lift2 (fun mt bd -> Main (mt, bd)) (return mt) ep_steps
  | mt -> lift2 (fun mt bd -> Method (mt, bd)) (return mt) ep_steps
;;

let ep_constructor_member_ =
  lift2 (fun con_sign body_ -> Constructor (con_sign, body_)) ep_constructor_sign ep_steps
;;

let ep_fild_member_ = ep_fild_sign >>| fun x -> Fild x

let ep_class_members =
  let member = choice [ ep_method_member; ep_fild_member_; ep_constructor_member_ ] in
  ep_figure_parens @@ many member
;;

let ep_class =
  let p_parent = ep_spaces @@ (char ':' *> skip_spaces *> p_ident) >>| fun x -> Some x in
  let class_id = ep_spaces @@ ep_is_ "class" ~then_:(ep_spaces p_ident) in
  lift4
    (fun cl_modif cl_id parent cl_mems -> { cl_modif; cl_id; parent; cl_mems })
    (ep_modifier_ p_access_modifier)
    class_id
    (option None p_parent)
    ep_class_members
;;

let ep_classes : tast t = many ep_class

let parse_option str ~p =
  match parse_string p ~consume:Angstrom.Consume.All str with
  | Ok x -> Some x
  | Error _ -> None
;;

let parse_until_true p = parse_string p ~consume:Angstrom.Consume.Prefix
let parse_ast = parse_string ep_classes ~consume:Angstrom.Consume.All
