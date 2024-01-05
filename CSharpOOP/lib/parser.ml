(** Copyright 2021-2023, Julia Kononova *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Base
open Angstrom
open Ast

let is_token = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true
  | _ -> false
;;

let is_first_name_char = function
  | 'a' .. 'z' | 'A' .. 'Z' | '_' -> true
  | _ -> false
;;

let is_keyword = function
  | "abstract"
  | "base"
  | "bool"
  | "break"
  | "char"
  | "class"
  | "else"
  | "false"
  | "if"
  | "int"
  | "null"
  | "private"
  | "protected"
  | "public"
  | "return"
  | "string"
  | "true"
  | "void" -> true
  | _ -> false
;;

let space = take_while Char.is_whitespace
let p_space p = space *> p
let read_str p = space *> string p
let parens p = space *> char '(' *> p <* space <* char ')'
let parens2 p = space *> char '{' *> p <* space <* char '}'
let p_str = char '\"' *> take_till (Char.equal '\"') <* char '\"'
let p_token = take_while1 is_token
let read_word = p_space p_token

(* VALUES PARSE *)
let check test str =
  try test str |> fun _ -> true with
  | Failure _ -> false
  | Invalid_argument _ -> false
;;

let p_integer =
  let r_integer i = return (VInt i) in
  p_token
  >>= fun num ->
  match check int_of_string num with
  | true -> int_of_string num |> r_integer
  | false -> fail "This is not a number"
;;

let p_char = char '\'' *> any_char <* char '\'' >>| fun c -> VChar c
let p_string = p_str >>= fun s -> return (VString s)

let p_bool =
  let r_bool i = return (VBool i) in
  p_token
  >>= fun b ->
  match check bool_of_string b with
  | true -> bool_of_string b |> r_bool
  | false -> fail "This is not a bool"
;;

let p_null =
  read_word
  >>= function
  | "null" -> return NULL
  | _ -> fail "This is not a null"
;;

let p_val =
  space *> choice [ p_bool; p_char; p_integer; p_string; p_null ]
  >>= fun v -> return (Exp_Const v)
;;

let p_name =
  let r_name i = return (Name i) in
  space *> p_token
  >>= fun n ->
  match is_keyword n with
  | true -> fail "This is not a name"
  | false -> if is_first_name_char n.[0] then r_name n else fail "This is not a name"
;;

(* TYPES PARSE *)
let p_q = space *> char '?' *> return true <|> return false

let base_converter = function
  | "bool" -> return TBool
  | "int" -> return TInt
  | "char" -> return TChar
  | _ -> fail "This is not a base type"
;;

let p_base_t t =
  p_q
  >>= function
  | true -> return (TNullable_type t)
  | false -> return (TNot_nullable_type (TBase t))
;;

let t_base_q = read_word >>= fun str -> base_converter str >>= fun t -> p_base_t t

let t_string =
  read_word
  >>= function
  | "string" -> return (TNot_nullable_type (TRef TString))
  | _ -> fail "This is not a string type"
;;

let t_void =
  read_word
  >>= function
  | "void" -> return TVoid
  | _ -> fail "This is not a void"
;;

let t_type = choice [ t_base_q; t_string ]
let t_val = t_type >>| fun vt -> TVar vt
let t_method = choice [ t_void; (t_type >>| fun mt -> TRetrun mt) ]

(* EXPR PARSE *)
let return_bin_op str binop =
  read_str str *> return (fun expr1 expr2 -> Bin_op (binop, expr1, expr2))
;;

let return_un_op str unop = read_str str *> return (fun expr -> Un_op (unop, expr))
let add = return_bin_op "+" Plus
let sub = return_bin_op "-" Dash
let mul = return_bin_op "*" Asterisk
let div = return_bin_op "/" Slash
let bmod = return_bin_op "%" Mod
let eq = return_bin_op "==" Equal
let not_eq = return_bin_op "!=" NotEqual
let less = return_bin_op "<" Less
let less_or_eq = return_bin_op "<=" LessOrEqual
let more = return_bin_op ">" More
let more_or_eq = return_bin_op ">=" MoreOrEqual
let log_and = return_bin_op "&&" And
let log_or = return_bin_op "||" Or
let assign = return_bin_op "=" Assign
let minus = return_un_op "-" Minus
let u_not = return_un_op "!" Not

let chainl1 e op =
  let rec go acc = lift2 (fun f x -> f acc x) op e >>= go <|> return acc in
  e >>= fun init -> go init
;;

let chainr1 e op =
  fix (fun go -> e >>= fun x -> op >>= (fun f -> go >>| fun g -> f x g) <|> return x)
;;

let p_list e str =
  let rec args lst e str =
    e >>= fun a -> read_str str *> args (lst @ [ a ]) e str <|> return (lst @ [ a ])
  in
  args [] e str
;;

let p_list1 e str = p_list e str <|> return []
let e_name = p_name >>| fun n -> Exp_Name n
let p_dot = read_str "." *> return (fun expr1 expr2 -> Access_By_Point (expr1, expr2))
let e_access_by_point = chainr1 e_name p_dot
let e_args e = parens @@ p_list1 e "," >>| fun e_l -> Exp_Args e_l
let method_invoke e = lift2 (fun n a -> Method_invoke (n, a)) e_access_by_point (e_args e)

let e_op =
  fix (fun expr ->
    let expr = parens expr <|> p_val <|> method_invoke expr <|> e_name in
    let expr = chainl1 expr (mul <|> div <|> bmod) in
    let expr = chainl1 expr (add <|> sub) in
    let expr = chainl1 expr (less_or_eq <|> more_or_eq <|> less <|> more) in
    let expr = chainl1 expr (eq <|> not_eq) in
    let expr = chainl1 expr log_and in
    let expr = chainl1 expr log_or in
    chainr1 expr assign)
;;

let expression = choice [ method_invoke e_op; e_op ]
let e_op_assign = lift3 (fun f a x -> a f x) e_name assign expression
let method_expr = choice [ e_op_assign; method_invoke e_op ] >>| fun i -> Expr i

(* STATEMENTS PARSE*)
let var_declaration = lift2 (fun t n -> Var_Declaration (t, n)) t_val p_name
let skip_eq_run_op = read_str "=" *> e_op >>= (fun e -> return (Some e)) <|> return None
let s_declaration = lift2 (fun vd op -> Decl (vd, op)) var_declaration skip_eq_run_op

let p_return =
  read_word
  >>= function
  | "return" -> e_op >>= (fun e -> return (Return (Some e))) <|> return (Return None)
  | _ -> fail "This is not a return"
;;

let p_if_cond =
  read_word
  >>= function
  | "if" -> parens e_op
  | _ -> fail "This is not IF word"
;;

let p_else body s_ie =
  read_str "else" *> (choice [ s_ie; body ] >>= fun e -> return (Some e)) <|> return None
;;

let s_if_else body =
  fix (fun if_else ->
    lift3 (fun i t e -> If (i, t, e)) p_if_cond body (p_else body if_else))
;;

let semicolon = fix (fun sem -> read_str ";" *> sem <|> return ())
let semicolon1 = read_str ";" *> semicolon

let p_body =
  let read_body expr = parens2 (many expr >>| (fun e -> Body e) <|> return (Body [])) in
  let e_choice b =
    choice
      [ s_declaration <* semicolon1
      ; method_expr <* semicolon1
      ; p_return <* semicolon1
      ; s_if_else b <* semicolon
      ]
  in
  fix (fun body -> read_body @@ e_choice body <* space)
;;

let p_access_modifier = function
  | "public" -> return Public
  | "protected" -> return Protected
  | "private" -> return Private
  | _ -> fail "Is not an access modifier"
;;

let modifier_list p = many p >>= fun l -> return (Some l) <|> return None

let p_m_modifier_list =
  let p_m_modifier =
    read_word
    >>= fun str ->
    match str with
    | "static" -> return Static
    | "override" -> return Override
    | _ -> p_access_modifier str >>= fun m -> return (MAccess m)
  in
  modifier_list p_m_modifier
;;

let p_f_modifier_list =
  let p_f_modifier =
    read_word
    >>= fun str ->
    match str with
    | "new" -> return New
    | _ -> p_access_modifier str >>= fun m -> return (FAccess m)
  in
  modifier_list p_f_modifier
;;

let p_c_modifier_list =
  let p_c_modifier = read_word >>= p_access_modifier >>= fun m -> return (CAccess m) in
  modifier_list p_c_modifier
;;

let p_i_modifier_list =
  read_word >>= p_access_modifier >>= (fun m -> return (Some (CAccess m))) <|> return None
;;

let p_m_name_args =
  lift2
    (fun n l -> n, l)
    p_name
    (parens @@ p_list1 var_declaration "," >>| fun m -> Params m)
;;

let p_method =
  lift3 (fun p i (d, o) -> Method (p, i, d, o)) p_m_modifier_list t_method p_m_name_args
;;

let c_method = lift2 (fun m b -> Method_Sign (m, b)) p_method p_body

let assign_option =
  p_space @@ (read_str "=" *> expression) >>= (fun x -> return (Some x)) <|> return None
;;

let p_field = lift3 (fun m t n -> Field (m, t, n)) p_f_modifier_list t_val p_name
let c_field = lift2 (fun f e -> Field_Sign (f, e)) p_field assign_option

let read_class_name =
  read_word
  >>= function
  | "class" -> p_name
  | _ -> fail "This is not a class"
;;

let class_members =
  let member =
    choice
      [ c_field >>| (fun f -> CField f) <* semicolon1
      ; c_method >>| (fun m -> CMethod m) <* semicolon
      ]
  in
  parens2 @@ (many member <|> return []) <* space
;;

let p_class =
  lift3 (fun m n mem -> Class (m, n, mem)) p_c_modifier_list read_class_name class_members
;;

let read_interface_name =
  read_word
  >>= function
  | "interface" -> p_name
  | _ -> fail "This is not a class"
;;

let intrface_members =
  let member =
    choice
      [ p_field >>| (fun f -> IField f) <* semicolon1
      ; p_method >>| (fun m -> IMethod m) <* semicolon
      ]
  in
  parens2 @@ (many member <|> return []) <* space
;;

let p_interface =
  lift3
    (fun m n mem -> Interface (m, n, mem))
    p_i_modifier_list
    read_interface_name
    intrface_members
;;

let p_ast =
  let obj_choice = choice [ p_class; p_interface ] in
  many obj_choice >>| (fun o -> Ast (Some o)) <|> return (Ast None)
;;

(* PARSERS *)
let parse p str = Angstrom.parse_string p ~consume:Angstrom.Consume.All str

let parse_to_some p str =
  match parse p str with
  | Ok x -> Some x
  | Error _ -> None
;;
