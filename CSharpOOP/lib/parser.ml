(** Copyright 2023-2024, Julia Kononova *)

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

let p_val =
  space *> choice [ p_bool; p_char; p_integer; p_string ]
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
let type_converter = function
  | "bool" -> return TBool
  | "int" -> return TInt
  | "char" -> return TChar
  | "string" -> return TString
  | _ -> fail "This is not a type"
;;

let t_void =
  read_word
  >>= function
  | "void" -> return TVoid
  | _ -> fail "This is not a void"
;;

let t_type = read_word >>= fun str -> type_converter str
let t_val = choice [ (t_type >>| fun vt -> TVar vt); (p_name >>| fun n -> TVar (TObj n)) ]
let t_method = choice [ t_void; (t_type >>| fun mt -> TReturn mt) ]

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
let u_new = return_un_op "new" New
let chainl0 e op = lift2 (fun f x -> f x) op e <|> e

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
let e_name = p_name >>= fun n -> return (Exp_Name n)
let p_dot = read_str "." *> return (fun name expr -> Access_By_Point (name, expr))
let e_access_by_point = chainr1 e_name p_dot
let args e = parens @@ p_list1 e "," >>| fun e_l -> Args e_l
let method_invoke e = lift2 (fun n a -> Method_invoke (n, a)) e_access_by_point (args e)

(* TODO: приведение типов*)
(* TODO: new methods *)
(* мб as *)
let e_op =
  fix (fun expr ->
    let expr =
      parens expr <|> p_val <|> method_invoke expr <|> e_access_by_point <|> e_name
    in
    let expr = chainl0 expr (minus <|> u_not <|> u_new) in
    let expr = chainl1 expr (mul <|> div <|> bmod) in
    let expr = chainl1 expr (add <|> sub) in
    let expr = chainl1 expr (less_or_eq <|> more_or_eq <|> less <|> more) in
    let expr = chainl1 expr (eq <|> not_eq) in
    let expr = chainl1 expr log_and in
    let expr = chainl1 expr log_or in
    chainr1 expr assign)
;;

let e_op_assign = lift3 (fun f a x -> a f x) (e_access_by_point <|> e_name) assign e_op
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

let p_for_sections =
  let for_initializer = s_declaration >>| (fun init -> Some init) <|> return None in
  let for_cond_iter = e_op >>| (fun expr -> Some expr) <|> return None in
  lift3
    (fun init cond iter -> init, cond, iter)
    (for_initializer <* p_space (char ';'))
    (for_cond_iter <* p_space (char ';'))
    for_cond_iter
;;

let p_for =
  read_word
  >>= function
  | "for" -> parens p_for_sections
  | _ -> fail "This is not FOR word"
;;

let s_for body = lift2 (fun (init, cond, iter) b -> For (init, cond, iter, b)) p_for body

let p_while =
  read_word
  >>= function
  | "while" -> parens e_op
  | _ -> fail "This is not WHILE word"
;;

let s_while body = lift2 (fun e b -> While (e, b)) p_while body
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
      ; s_for b <* semicolon
      ; s_while b <* semicolon
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

let p_poly_modifier = function
  | "static" -> return Static
  | "override" -> return Override
  | "virtual" -> return Virtual
  | "new" -> return MNew
  | _ -> fail "Is not an polymorphism modifier"
;;

let modifier_opt f = read_word >>= f >>= (fun m -> return (Some m)) <|> return None
let access_modifier_opt = modifier_opt p_access_modifier

let p_meth_modifier =
  lift2 (fun a p -> a, p) access_modifier_opt (modifier_opt p_poly_modifier)
;;

let p_m_name_params =
  lift2
    (fun n l -> n, l)
    p_name
    (parens @@ p_list1 var_declaration "," >>| fun m -> Params m)
;;

let p_method =
  lift3
    (fun (m_acc_modifier, m_poly_modifier) m_type (m_name, m_params) ->
      { m_acc_modifier; m_poly_modifier; m_type; m_name; m_params })
    p_meth_modifier
    t_method
    p_m_name_params
;;

let c_method = lift2 (fun m b -> CMethod (m, b)) p_method p_body

let assign_option =
  p_space @@ (read_str "=" *> e_op) >>= (fun x -> return (Some x)) <|> return None
;;

let p_field =
  lift3
    (fun f_modifier f_type f_name -> { f_modifier; f_type; f_name })
    access_modifier_opt
    t_val
    p_name
;;

let c_field = lift2 (fun f e -> CField (f, e)) p_field assign_option

let base =
  read_str ":" *> read_str "base" *> args e_op
  >>= (fun b -> return (Some b))
  <|> return None
;;

let constructor =
  lift3
    (fun c_modifier (c_name, c_params) c_base -> { c_modifier; c_name; c_params; c_base })
    access_modifier_opt
    p_m_name_params
    base
;;

let c_constructor = lift2 (fun c b -> CConstructor (c, b)) constructor p_body

let read_class_name =
  read_word
  >>= function
  | "class" -> p_name
  | _ -> fail "This is not a class"
;;

let parent = read_str ":" *> p_name >>= (fun p -> return (Some p)) <|> return None

let class_members =
  let member =
    choice [ c_field <* semicolon1; c_method <* semicolon; c_constructor <* semicolon ]
  in
  parens2 @@ (many member <|> return []) <* space
;;

let p_class =
  lift4
    (fun cl_modifier cl_name cl_parent cl_body ->
      Class { cl_modifier; cl_name; cl_parent; cl_body })
    access_modifier_opt
    read_class_name
    parent
    class_members
;;

let read_interface_name =
  read_word
  >>= function
  | "interface" -> p_name
  | _ -> fail "This is not a interface"
;;

let interface_modifier =
  let p_public = function
    | "public" -> return Public
    | _ -> fail "Interface can only have public fields "
  in
  modifier_opt p_public
;;

let intrface_members =
  let member = p_method >>| (fun m -> IMethod m) <* semicolon in
  parens2 @@ (many member <|> return []) <* space
;;

let p_interface =
  lift4
    (fun i_modifier i_name i_parent i_body ->
      Interface { i_modifier; i_name; i_parent; i_body })
    interface_modifier
    read_interface_name
    parent
    intrface_members
;;

let p_ast =
  let obj_choice = choice [ p_class; p_interface ] in
  many obj_choice >>| fun o -> Ast o
;;

(* PARSERS *)
let parse p str = Angstrom.parse_string p ~consume:Angstrom.Consume.All str

let parse_to_some p str =
  match parse p str with
  | Ok x -> Some x
  | Error _ -> None
;;
