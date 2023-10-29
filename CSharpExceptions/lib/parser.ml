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
  | _ -> false
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

let _ep_list_from ep_arg =
  let ep_args = ep_arg <* ep_spaces @@ char ',' <|> ep_arg in
  ep_parens @@ many ep_args
;;

let _ep_invoke meth_ident ep_arg =
  _ep_list_from ep_arg
  >>= (fun exp -> return (EParams exp))
  >>| fun args -> EMethod_invoke (meth_ident, args)
;;

let _ep_method_invoke ep_arg = ep_member_ident >>= fun id -> _ep_invoke id ep_arg

let _ep_method_fild ep_arg =
  ep_member_ident >>= fun id -> _ep_invoke id ep_arg <|> return id
;;

let _ep_var_decl tp = skip_spaces1 *> p_ident >>| fun id -> EDecl (TVariable tp, id)

let _ep_var_type =
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
  >>= _ep_var_decl
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
    let lvl1 = choice [ ep_parens expr; ep_value; _ep_method_fild expr ] in
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
let ep_method_invoke = _ep_method_invoke ep_operation

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

let _ep_keyword kw = ep_spaces @@ read_as_token kw
let ep_break = _ep_keyword "break" *> return EBreak

let ep_return =
  lift2
    (fun _ ex -> EReturn ex)
    (_ep_keyword "return")
    (ep_operation >>= (fun x -> return (Some x)) <|> return None)
;;

let _ep_is kw ~then_:ps = _ep_keyword kw *> ps

let _ep_if_cond =
  let p_cond = ep_parens ep_operation in
  _ep_is "if" ~then_:p_cond
;;

let _ep_else_cond ep_body ep_ifls =
  choice
    ?failure_msg:(Some "It isn't ELSE or ELSE IF")
    [ _ep_is "else" ~then_:ep_ifls; _ep_is "else" ~then_:ep_body ]
  >>= (fun else_ -> return (Some else_))
  <|> return None
;;

let _ep_if_else ep_body =
  fix (fun if_else ->
    let else_ = _ep_else_cond ep_body if_else in
    lift3 (fun cond body else_ -> EIf_else (cond, body, else_)) _ep_if_cond ep_body else_)
;;

let _ep_brunch_loop ep_body =
  choice ?failure_msg:(Some "It isn't IF or ...") [ _ep_if_else ep_body ]
;;

let _ep_skip_semicolon = ep_spaces @@ char ';'

let _ep_semicolon1 ps =
  ps <* _ep_skip_semicolon *> fix (fun foo -> _ep_skip_semicolon *> foo <|> return "")
;;

let _ep_semicolon ps = _ep_semicolon1 ps <|> ps

let ep_steps =
  fix (fun steps ->
    let step = _ep_semicolon1 in
    let op_step = _ep_semicolon in
    let body_step =
      choice
        [ step ep_eAssign_eDecl
        ; step ep_method_invoke
        ; step ep_assign
        ; step ep_break
        ; step ep_return
        ; op_step @@ _ep_brunch_loop steps
        ]
    in
    ep_figure_parens @@ many body_step >>| fun bd -> Steps bd)
;;

let ep_brunch_loop = _ep_brunch_loop ep_steps
let _ep_args = _ep_list_from ep_var_decl >>= fun exp -> return (Args exp)
let _ep_modifier p_mod = option None (ep_spaces @@ p_mod >>= fun x -> return (Some x))

let ep_method_sign =
  lift4
    (fun m_modif m_type m_id m_args -> { m_modif; m_type; m_id; m_args })
    (_ep_modifier p_method_modifier)
    (ep_spaces p_method_type)
    (ep_spaces p_ident)
    _ep_args
;;

let ep_constructor_sign =
  let ep_base_cons =
    ep_spaces @@ (char ':' *> _ep_is "base" ~then_:(_ep_list_from ep_operation))
    >>| fun x -> Some (EParams x)
  in
  lift4
    (fun con_modif con_id con_args base_params ->
      { con_modif; con_id; con_args; base_params })
    (_ep_modifier p_access_modifier)
    (ep_spaces p_ident)
    _ep_args
    (option None ep_base_cons)
;;

let ep_fild_sign =
  let f_value = ep_spaces (char '=') *> ep_operation >>| fun x -> Some x in
  lift4
    (fun f_modif f_type f_id f_val -> { f_modif; f_type; f_id; f_val })
    (_ep_modifier p_fild_modifier)
    (ep_spaces _ep_var_type)
    (ep_spaces p_ident)
    (option None f_value)
  <* ep_spaces @@ char ';'
;;

let is_main (m_sign : method_sign) =
  match m_sign.m_id with
  | Id "Main" -> true
  | _ -> false
;;

let _ep_method_member =
  ep_method_sign
  >>= function
  | mt when is_main mt -> lift2 (fun mt bd -> Main (mt, bd)) (return mt) ep_steps
  | mt -> lift2 (fun mt bd -> Method (mt, bd)) (return mt) ep_steps
;;

let _ep_constructor_member =
  lift2 (fun con_sign body_ -> Constructor (con_sign, body_)) ep_constructor_sign ep_steps
;;

let _ep_fild_member = ep_fild_sign >>| fun x -> Fild x

let ep_class_members =
  let member = choice [ _ep_method_member; _ep_fild_member; _ep_constructor_member ] in
  ep_figure_parens @@ many member
;;

let ep_class =
  let p_parent = ep_spaces @@ (char ':' *> skip_spaces *> p_ident) >>| fun x -> Some x in
  let class_id = ep_spaces @@ _ep_is "class" ~then_:(ep_spaces p_ident) in
  lift4
    (fun cl_modif cl_id parent cl_mems -> { cl_modif; cl_id; parent; cl_mems })
    (_ep_modifier p_access_modifier)
    class_id
    (option None p_parent)
    ep_class_members
;;

let ep_classes : tast t = many ep_class

let _parse str ~p =
  match parse_string p ~consume:Angstrom.Consume.All str with
  | Ok x -> Some x
  | Error _ -> None
;;

let parse = parse_string ep_classes ~consume:Angstrom.Consume.All
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

let test_pars ps eq str ans = eq_wrap ~eq ans (_parse ~p:ps str)
let print_pars ps form str = show_wrap form (_parse ~p:ps str)

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

(* ep_operation tests: *)
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
               , EBin_op (Plus, EIdentifier (Id "a"), EVal (VInt 2))
               , EBin_op
                   ( Asterisk
                   , EBin_op
                       ( Division
                       , EMember_ident
                           ( EIdentifier (Id "t")
                           , EMember_ident (EIdentifier (Id "a"), EIdentifier (Id "b")) )
                       , EMember_ident (EIdentifier (Id "t"), EIdentifier (Id "r")) )
                   , EVal (VInt 2) ) ) ) ))
;;

let%test _ =
  test_operation
    "a = b= c"
    (EBin_op
       ( Assign
       , EIdentifier (Id "a")
       , EBin_op (Assign, EIdentifier (Id "b"), EIdentifier (Id "c")) ))
;;

let%test _ =
  test_operation
    "a (1+2,  d  , \"qwe\") + 100000"
    (EBin_op
       ( Plus
       , EMethod_invoke
           ( EIdentifier (Id "a")
           , EParams
               [ EBin_op (Plus, EVal (VInt 1), EVal (VInt 2))
               ; EIdentifier (Id "d")
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
       ( EDecl (TVariable (TVar (TNullable (TClass (Id "a")))), Id "egor")
       , EBin_op
           ( Plus
           , EMethod_invoke
               ( EIdentifier (Id "a")
               , EParams
                   [ EBin_op (Plus, EVal (VInt 1), EVal (VInt 2))
                   ; EIdentifier (Id "d")
                   ; EVal (VString "qwe")
                   ] )
           , EVal (VInt 100000) ) ))
;;

(* ep_steps *)

let test_steps = test_pars ep_steps equal_expr

let%test _ =
  test_steps
    "   {if (true) \n\
    \          { a(); \n\
    \            if(false) \n\
    \              {\n\
    \                e    = b; \n\
    \                return;\n\
    \              } else \n\
    \                {\n\
    \                  int  ?   exmp = 243 + 1;\n\
    \                }\n\
    \              }; ; ;     ; \n\
    \              a(1+2 , cl)  ; ;   ; \n\
    \              if (1+ run()) \n\
    \                    {\n\
    \                      first(1);\n\
    \                    } else if (true) {} \n\
    \              return 1+1; ; ;\n\
    \      }"
    (Steps
       [ EIf_else
           ( EVal (VBool true)
           , Steps
               [ EMethod_invoke (EIdentifier (Id "a"), EParams [])
               ; EIf_else
                   ( EVal (VBool false)
                   , Steps
                       [ EBin_op (Assign, EIdentifier (Id "e"), EIdentifier (Id "b"))
                       ; EReturn None
                       ]
                   , Some
                       (Steps
                          [ EAssign
                              ( EDecl
                                  (TVariable (TVar (TNullable (TBase TInt))), Id "exmp")
                              , EBin_op (Plus, EVal (VInt 243), EVal (VInt 1)) )
                          ]) )
               ]
           , None )
       ; EMethod_invoke
           ( EIdentifier (Id "a")
           , EParams
               [ EBin_op (Plus, EVal (VInt 1), EVal (VInt 2)); EIdentifier (Id "cl") ] )
       ; EIf_else
           ( EBin_op
               (Plus, EVal (VInt 1), EMethod_invoke (EIdentifier (Id "run"), EParams []))
           , Steps
               [ EMethod_invoke (EIdentifier (Id "first"), EParams [ EVal (VInt 1) ]) ]
           , Some (EIf_else (EVal (VBool true), Steps [], None)) )
       ; EReturn (Some (EBin_op (Plus, EVal (VInt 1), EVal (VInt 1))))
       ])
;;

let test_pp_steps = print_pars ep_steps pp_expr

let%expect_test _ =
  test_pp_steps
    "   {if (true) \n\
    \          { a(); \n\
    \            if(false) \n\
    \              {\n\
    \                e    = b; \n\
    \                return;\n\
    \              } else \n\
    \                {\n\
    \                  int  ?   exmp = 243 + 1;\n\
    \                }\n\
    \              }; ; ;     ; \n\
    \              a(1+2 , cl)  ; ;   ; \n\
    \              if (1+ run()) \n\
    \                    {\n\
    \                      first(1);\n\
    \                    } else if (true) {} \n\
    \              return 1+1; ; ;\n\
    \      }";
  [%expect
    {|
      (Steps
         [(EIf_else ((EVal (VBool true)),
             (Steps
                [(EMethod_invoke ((EIdentifier (Id "a")), (EParams [])));
                  (EIf_else ((EVal (VBool false)),
                     (Steps
                        [(EBin_op (Assign, (EIdentifier (Id "e")),
                            (EIdentifier (Id "b"))));
                          (EReturn None)]),
                     (Some (Steps
                              [(EAssign (
                                  (EDecl (
                                     (TVariable (TVar (TNullable (TBase TInt)))),
                                     (Id "exmp"))),
                                  (EBin_op (Plus, (EVal (VInt 243)),
                                     (EVal (VInt 1))))
                                  ))
                                ]))
                     ))
                  ]),
             None));
           (EMethod_invoke ((EIdentifier (Id "a")),
              (EParams
                 [(EBin_op (Plus, (EVal (VInt 1)), (EVal (VInt 2))));
                   (EIdentifier (Id "cl"))])
              ));
           (EIf_else (
              (EBin_op (Plus, (EVal (VInt 1)),
                 (EMethod_invoke ((EIdentifier (Id "run")), (EParams []))))),
              (Steps
                 [(EMethod_invoke ((EIdentifier (Id "first")),
                     (EParams [(EVal (VInt 1))])))
                   ]),
              (Some (EIf_else ((EVal (VBool true)), (Steps []), None)))));
           (EReturn (Some (EBin_op (Plus, (EVal (VInt 1)), (EVal (VInt 1))))))])

     |}]
;;

let test_pp_fuc = print_pars _ep_method_member pp_class_member

let%expect_test _ =
  test_pp_fuc
    "static int Fac(int num)\n\
    \    {\n\
    \        if (num == 1)\n\
    \        {\n\
    \            return 1;\n\
    \        }\n\
    \        else \n\
    \        {\n\
    \            return num * Fac(num - 1);\n\
    \        }\n\
    \    }";
  [%expect
    {|
    (Method (
       { m_modif = (Some MStatic); m_type = (TReturn (TNot_Nullable TInt));
         m_id = (Id "Fac");
         m_args =
         (Args [(EDecl ((TVariable (TVar (TNot_Nullable TInt))), (Id "num")))]) },
       (Steps
          [(EIf_else (
              (EBin_op (Equal, (EIdentifier (Id "num")), (EVal (VInt 1)))),
              (Steps [(EReturn (Some (EVal (VInt 1))))]),
              (Some (Steps
                       [(EReturn
                           (Some (EBin_op (Asterisk, (EIdentifier (Id "num")),
                                    (EMethod_invoke ((EIdentifier (Id "Fac")),
                                       (EParams
                                          [(EBin_op (Minus,
                                              (EIdentifier (Id "num")),
                                              (EVal (VInt 1))))
                                            ])
                                       ))
                                    ))))
                         ]))
              ))
            ])
       )) |}]
;;

let test_pp_class = print_pars ep_class pp_class_sign

let%expect_test _ =
  test_pp_class
    "class Program : Exception\n\
     {\n\
    \    int A1 = 0;\n\
    \    public MyClass A2;\n\
    \    static int Fac(int num)\n\
    \    {\n\
    \        if (num == 1)\n\
    \        {\n\
    \            return 1;\n\
    \        }\n\
    \        else \n\
    \        {\n\
    \            return num * Fac(num - 1);\n\
    \        }\n\
    \    }\n\
     }";
  [%expect
    {|
   { cl_modif = None; cl_id = (Id "Program"); parent = (Some (Id "Exception"));
     cl_mems =
     [(Fild
         { f_modif = None; f_type = (TVar (TNot_Nullable TInt));
           f_id = (Id "A1"); f_val = (Some (EVal (VInt 0))) });
       (Fild
          { f_modif = (Some (FAccess MPublic));
            f_type = (TVar (TNullable (TClass (Id "MyClass"))));
            f_id = (Id "A2"); f_val = None });
       (Method (
          { m_modif = (Some MStatic); m_type = (TReturn (TNot_Nullable TInt));
            m_id = (Id "Fac");
            m_args =
            (Args
               [(EDecl ((TVariable (TVar (TNot_Nullable TInt))), (Id "num")))])
            },
          (Steps
             [(EIf_else (
                 (EBin_op (Equal, (EIdentifier (Id "num")), (EVal (VInt 1)))),
                 (Steps [(EReturn (Some (EVal (VInt 1))))]),
                 (Some (Steps
                          [(EReturn
                              (Some (EBin_op (Asterisk,
                                       (EIdentifier (Id "num")),
                                       (EMethod_invoke (
                                          (EIdentifier (Id "Fac")),
                                          (EParams
                                             [(EBin_op (Minus,
                                                 (EIdentifier (Id "num")),
                                                 (EVal (VInt 1))))
                                               ])
                                          ))
                                       ))))
                            ]))
                 ))
               ])
          ))
       ]
     } |}]
;;

let test_pp_classes = print_pars ep_classes pp_tast

let%expect_test _ =
  test_pp_classes
    "class Program : Exception\n\
     {\n\
    \    int A1 = 0;\n\
    \    public MyClass A2;\n\
    \    static int Fac(int num)\n\
    \    {\n\
    \        if (num == 1)\n\
    \        {\n\
    \            return 1;\n\
    \        }\n\
    \        else \n\
    \        {\n\
    \            return num * Fac(num - 1);\n\
    \        }\n\
    \    }\n\
     }\n\n\
     class trueF {}";
  [%expect
    {|
  [{ cl_modif = None; cl_id = (Id "Program"); parent = (Some (Id "Exception"));
     cl_mems =
     [(Fild
         { f_modif = None; f_type = (TVar (TNot_Nullable TInt));
           f_id = (Id "A1"); f_val = (Some (EVal (VInt 0))) });
       (Fild
          { f_modif = (Some (FAccess MPublic));
            f_type = (TVar (TNullable (TClass (Id "MyClass"))));
            f_id = (Id "A2"); f_val = None });
       (Method (
          { m_modif = (Some MStatic); m_type = (TReturn (TNot_Nullable TInt));
            m_id = (Id "Fac");
            m_args =
            (Args
               [(EDecl ((TVariable (TVar (TNot_Nullable TInt))), (Id "num")))])
            },
          (Steps
             [(EIf_else (
                 (EBin_op (Equal, (EIdentifier (Id "num")), (EVal (VInt 1)))),
                 (Steps [(EReturn (Some (EVal (VInt 1))))]),
                 (Some (Steps
                          [(EReturn
                              (Some (EBin_op (Asterisk,
                                       (EIdentifier (Id "num")),
                                       (EMethod_invoke (
                                          (EIdentifier (Id "Fac")),
                                          (EParams
                                             [(EBin_op (Minus,
                                                 (EIdentifier (Id "num")),
                                                 (EVal (VInt 1))))
                                               ])
                                          ))
                                       ))))
                            ]))
                 ))
               ])
          ))
       ]
     };
    { cl_modif = None; cl_id = (Id "trueF"); parent = None; cl_mems = [] }] |}]
;;
