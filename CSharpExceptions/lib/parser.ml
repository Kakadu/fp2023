(** Copyright 2021-2023, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(* TODO: implement parser here *)

(* *_ - parser that don't fail *)
(* *' - parser that don't advance input *)

open Base
open Angstrom
open Ast

(** @see <https://learn.microsoft.com/en-us/dotnet/csharp/language-reference/keywords/>
      C# keywords *)

let p_list1 exp =
  fix (fun foo -> lift2 (fun x tl -> x :: tl) exp foo <|> (exp >>| fun x -> x :: []))
;;

let p_list exp = p_list1 exp <|> return []
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
  | _ -> false
;;

let is_int = if_value int_of_string
let is_bool = if_value bool_of_string

let is_nullable = function
  | '?' -> true
  | _ -> false
;;

let s_token = take_while1 is_token

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
  | true when not (Char.is_digit str.[0]) -> return (Name str)
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

let p_var_type = p_keyword_type >>= fun x -> return (TVariable x)
let ep_spaces prs = skip_spaces *> prs
let ep_parens prs = ep_spaces @@ (char '(' *> prs) <* ep_spaces @@ char ')'
let ep_figure_parens prs = ep_spaces @@ (char '{' *> prs) <* ep_spaces @@ char '}'
let convert_val_to_expr prs = ep_spaces prs >>| fun x -> EVal x
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

let ep_params ep_arg =
  let ep_args = ep_arg <* ep_spaces @@ char ',' <|> ep_arg in
  ep_parens @@ p_list ep_args >>= fun exp -> return (EParams exp)
;;

let ep_method_inv meth_ident ep_args =
  ep_params ep_args >>| fun args -> EMethod_invoke (meth_ident, args)
;;

let ep_method_fild ep_args =
  ep_member_ident >>= fun id -> ep_method_inv id ep_args <|> return id
;;

let _ep_var_decl tp = skip_spaces1 *> p_ident >>| fun id -> EVar_decl (tp, id)

let ep_var_decl =
  ep_spaces
  @@ choice
       ?failure_msg:(Some "Not a declaration")
       [ p_var_type >>= _ep_var_decl
       ; p_ident >>| (fun cl -> TVariable (TNullable (TClass cl))) >>= _ep_var_decl
       ]
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
let ( >- ) lvl p_list = chainl0 lvl (choice p_list)
let ( >>- ) lvl p_list = chainl1 lvl (choice p_list)
let ( -<< ) lvl p_list = chainr1 lvl (choice p_list)

let ep_operation =
  fix (fun expr ->
    let lvl1 = choice [ ep_parens expr; ep_value; ep_method_fild expr ] in
    let lvl2 = lvl1 >- [ ep_un_minus; ep_new; ep_not ] in
    let lvl3 = lvl2 >>- [ ( *^ ); ( /^ ); ( %^ ) ] in
    let lvl4 = lvl3 >>- [ ( +^ ); ( -^ ) ] in
    let lvl5 = lvl4 >>- [ ( <=^ ); ( >=^ ); ( <^ ); ( >^ ) ] in
    let lvl6 = lvl5 >>- [ ( ==^ ); ( !=^ ) ] in
    let lvl7 = lvl6 >>- [ ( &&^ ) ] in
    let lvl8 = lvl7 >>- [ ( ||^ ) ] in
    lvl8 -<< [ ( =^ ) ])
;;

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

let _ep_keyword kw =
  ep_spaces @@ s_token
  >>= function
  | x when String.( = ) x kw -> return kw
  | _ -> fail ("Not a " ^ kw)
;;

let ep_break = _ep_keyword "break" *> return EBreak
let ep_return = _ep_keyword "return" *> return EReturn
let ep_is kw ~then_:ps = _ep_keyword kw *> ps

let _ep_if_cond =
  let p_cond = ep_parens ep_operation in
  ep_is "if" ~then_:p_cond
;;

let _ep_else_cond ep_body ep_ifls =
  choice
    ?failure_msg:(Some "It isn't ELSE or ELSE IF")
    [ ep_is "else" ~then_:ep_ifls; ep_is "else" ~then_:ep_body ]
  >>= (fun else_ -> return (Some else_))
  <|> return None
;;

let ep_if_else ep_body =
  fix (fun if_else ->
    let else_ = _ep_else_cond ep_body if_else in
    lift3 (fun cond body else_ -> EIf_else (cond, body, else_)) _ep_if_cond ep_body else_)
;;

let _ep_brunch_loop ep_body =
  choice ?failure_msg:(Some "It isn't IF or ...") [ ep_if_else ep_body ]
;;

let ep_steps =
  fix (fun steps ->
    let body_step =
      let p_step ep = ep <* ep_spaces @@ char ';' in
      choice
        [ p_step ep_eAssign_eDecl
        ; p_step ep_operation
        ; p_step ep_break
        ; p_step ep_return
        ; _ep_brunch_loop steps
        ]
    in
    ep_figure_parens @@ p_list body_step >>| fun bd -> Steps bd)
;;

let ep_brunch_loop = _ep_brunch_loop ep_steps

let parse str ~p =
  match parse_string p ~consume:Angstrom.Consume.All str with
  | Ok x -> Some x
  | Error _ -> None
;;

let n_parse p = parse_string p ~consume:Angstrom.Consume.Prefix

(* *******************->  tests  <-******************* *)
let eq_wrap ~eq ans = function
  | Some x when eq x ans -> true
  | _ -> false
;;

let show_wrap form = function
  | Some x -> Format.printf "%a@\n" form x
  | _ -> Format.print_string "Some error during parsing\n"
;;

let test_pars ps eq str ans = eq_wrap ~eq ans (parse ~p:ps str)
let print_pars ps form str = show_wrap form (parse ~p:ps str)

(* let%test_unit _ = print_pars func *)

(* p_num tests: *)
let test_num = test_pars p_number equal_value_

(* true *)
let%test _ = test_num "666" (VInt 666)
let%test _ = test_num "6__6_6" (VInt 666)
let%test _ = test_num "0" (VInt 0)

(* false *)
let%test _ = not (test_num "-0" (VInt 0))
let%test _ = not (test_num "adas" (VInt 0))
let%test _ = not (test_num "_123" (VInt 0))

(* p_str tests: *)
let test_str = test_pars p_string equal_value_

(* true *)
let%test _ = test_str "\"666\"" (VString "666")
let%test _ = test_str "\"6\n6\"" (VString "6\n6")

(* false *)
let%test _ = not (test_str "\n6\"" (VString "6\n6"))
let%test _ = not (test_str "\"6\n" (VString "6\n6"))
let%test _ = not (test_str "6" (VString "6\n6"))

(* p_str tests: *)
let test_chr = test_pars p_char equal_value_

(* true *)
let%test _ = test_chr "\'\"\'" (VChar '\"')
let%test _ = test_chr "\'\'\'" (VChar '\'')
let%test _ = test_chr "\'\n\'" (VChar '\n')
let%test _ = test_chr "\'@\'" (VChar '@')

(* false *)
let%test _ = not (test_chr "\n\'" (VChar '\n'))
let%test _ = not (test_chr "\'@" (VChar '@'))
let%test _ = not (test_chr "@" (VChar '@'))

(* p_bool tests: *)
let test_bool = test_pars p_bool equal_value_

(* true *)
let%test _ = test_bool "true" (VBool true)
let%test _ = test_bool "false" (VBool false)

(* false *)
let%test _ = not (test_bool "@das" (VBool false))
let%test _ = not (test_bool "das" (VBool false))

(* ep_operation *)
let test_operation = test_pars ep_operation equal_expr

(* true *)
let%test _ =
  test_operation
    "-(!(a +  2 - (    t.a.b     /     t.r)*2))"
    (EUn_op
       ( UMinus
       , EUn_op
           ( UNot
           , EBin_op
               ( Minus
               , EBin_op (Plus, EIdentifier (Name "a"), EVal (VInt 2))
               , EBin_op
                   ( Asterisk
                   , EBin_op
                       ( Division
                       , EMember_ident
                           ( EIdentifier (Name "t")
                           , EMember_ident (EIdentifier (Name "a"), EIdentifier (Name "b"))
                           )
                       , EMember_ident (EIdentifier (Name "t"), EIdentifier (Name "r")) )
                   , EVal (VInt 2) ) ) ) ))
;;

let%test _ =
  test_operation
    "a = b= c"
    (EBin_op
       ( Assign
       , EIdentifier (Name "a")
       , EBin_op (Assign, EIdentifier (Name "b"), EIdentifier (Name "c")) ))
;;

let%test _ =
  test_operation
    "a (1+2,  d  , \"qwe\") + 100000"
    (EBin_op
       ( Plus
       , EMethod_invoke
           ( EIdentifier (Name "a")
           , EParams
               [ EBin_op (Plus, EVal (VInt 1), EVal (VInt 2))
               ; EIdentifier (Name "d")
               ; EVal (VString "qwe")
               ] )
       , EVal (VInt 100000) ))
;;

(* ep_eAssign_eDecl *)
let test_eAssign_eDecl = test_pars ep_eAssign_eDecl equal_expr

let%test _ =
  test_eAssign_eDecl
    "a         egor    =    a (1+2,  d  , \"qwe\") + 100000"
    (EAssign
       ( EVar_decl (TVariable (TNullable (TClass (Name "a"))), Name "egor")
       , EBin_op
           ( Plus
           , EMethod_invoke
               ( EIdentifier (Name "a")
               , EParams
                   [ EBin_op (Plus, EVal (VInt 1), EVal (VInt 2))
                   ; EIdentifier (Name "d")
                   ; EVal (VString "qwe")
                   ] )
           , EVal (VInt 100000) ) ))
;;

(* ep_steps *)

let test_steps = test_pars ep_steps equal_expr

let%test _ =
  test_steps
    "{if (true) \n\
    \      { q; \n\
    \        a(); \n\
    \        1+2; \n\
    \        if(false) \n\
    \          {\n\
    \            e;\n\
    \          } else \n\
    \            {\n\
    \              int  ?   exmp = 243 + 1;\n\
    \            }\n\
    \          } \n\
    \          a(1+2 , cl); \n\
    \          if (1+ run()) \n\
    \                {\n\
    \                  first(1);\n\
    \                } else if (true) {}\n\
    \          }"
    (Steps
       [ EIf_else
           ( EVal (VBool true)
           , Steps
               [ EIdentifier (Name "q")
               ; EMethod_invoke (EIdentifier (Name "a"), EParams [])
               ; EBin_op (Plus, EVal (VInt 1), EVal (VInt 2))
               ; EIf_else
                   ( EVal (VBool false)
                   , Steps [ EIdentifier (Name "e") ]
                   , Some
                       (Steps
                          [ EAssign
                              ( EVar_decl (TVariable (TNullable (TBase TInt)), Name "exmp")
                              , EBin_op (Plus, EVal (VInt 243), EVal (VInt 1)) )
                          ]) )
               ]
           , None )
       ; EMethod_invoke
           ( EIdentifier (Name "a")
           , EParams
               [ EBin_op (Plus, EVal (VInt 1), EVal (VInt 2)); EIdentifier (Name "cl") ]
           )
       ; EIf_else
           ( EBin_op
               (Plus, EVal (VInt 1), EMethod_invoke (EIdentifier (Name "run"), EParams []))
           , Steps
               [ EMethod_invoke (EIdentifier (Name "first"), EParams [ EVal (VInt 1) ]) ]
           , Some (EIf_else (EVal (VBool true), Steps [], None)) )
       ])
;;
