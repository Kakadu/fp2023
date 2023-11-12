(** Copyright 2021-2023, Georgy Sichkar *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Csharp_Exc_Lib.Parser
open Csharp_Exc_Lib.Ast

(* *******************->  tests for Parser and Ast  <-******************* *)

let eq_wrap ~eq ans = function
  | Some x when eq x ans -> true
  | _ -> false
;;

let show_wrap form = function
  | Some x -> Format.printf "%a@\n" form x
  | _ -> Format.print_string "Some error during parsing\n"
;;

let test_pars ps eq str ans = eq_wrap ~eq ans (parse_option ~p:ps str)
let print_pars ps form str = show_wrap form (parse_option ~p:ps str)

(* ep_operation tests: *)
let test_pp_operation = print_pars ep_operation pp_expr

let%expect_test "Operations - arithmetic" =
  test_pp_operation
    {|-(!(a +  2 - (    t.a.b     /     (a%b))*2))   + some(new myClass) + (-3)|};
  [%expect
    {|
    (EBin_op (Plus,
       (EBin_op (Plus,
          (EUn_op (UMinus,
             (EUn_op (UNot,
                (EBin_op (Minus,
                   (EBin_op (Plus, (EIdentifier (Id "a")), (EConst (VInt 2)))),
                   (EBin_op (Asterisk,
                      (EBin_op (Division,
                         (EPoint_access ((EIdentifier (Id "t")),
                            (EPoint_access ((EIdentifier (Id "a")),
                               (EIdentifier (Id "b"))))
                            )),
                         (EBin_op (Mod, (EIdentifier (Id "a")),
                            (EIdentifier (Id "b"))))
                         )),
                      (EConst (VInt 2))))
                   ))
                ))
             )),
          (EMethod_invoke ((EIdentifier (Id "some")),
             (Params [(EUn_op (New, (EIdentifier (Id "myClass"))))])))
          )),
       (EUn_op (UMinus, (EConst (VInt 3)))))) |}]
;;

let%expect_test "Operations - predicates" =
  test_pp_operation {|!(a > b) && (c < b) || 1 + 3 >= 1 == true != false|};
  [%expect
    {|
  (EBin_op (Or,
     (EBin_op (And,
        (EUn_op (UNot,
           (EBin_op (More, (EIdentifier (Id "a")), (EIdentifier (Id "b")))))),
        (EBin_op (Less, (EIdentifier (Id "c")), (EIdentifier (Id "b")))))),
     (EBin_op (NotEqual,
        (EBin_op (Equal,
           (EBin_op (MoreOrEqual,
              (EBin_op (Plus, (EConst (VInt 1)), (EConst (VInt 3)))),
              (EConst (VInt 1)))),
           (EConst (VBool true)))),
        (EConst (VBool false))))
     )) |}]
;;

let%expect_test "Multiple assign" =
  test_pp_operation {|a = b= c|};
  [%expect
    {|
    (EBin_op (Assign, (EIdentifier (Id "a")),
       (EBin_op (Assign, (EIdentifier (Id "b")), (EIdentifier (Id "c")))))) |}]
;;

let%expect_test "Method invocation" =
  test_pp_operation {|a.b.c   (1, qwert_cl, "qwert_s", true, false)|};
  [%expect
    {|
    (EMethod_invoke (
       (EPoint_access ((EIdentifier (Id "a")),
          (EPoint_access ((EIdentifier (Id "b")), (EIdentifier (Id "c")))))),
       (Params
          [(EConst (VInt 1)); (EIdentifier (Id "qwert_cl"));
            (EConst (VString "qwert_s")); (EConst (VBool true));
            (EConst (VBool false))])
       )) |}]
;;

(* ep_decl *)
let test_pp_ep_decl = print_pars ep_decl pp_statement

let%expect_test "Declaration + assign" =
  test_pp_ep_decl {|a         egor    =    a (1+2,  d  , "qwe") + 100000|};
  [%expect
    {|
    (SDecl (
       (Var_decl ((TVariable (TVar (TNullable (TClass (Id "a"))))), (Id "egor"))),
       (Some (EBin_op (Plus,
                (EMethod_invoke ((EIdentifier (Id "a")),
                   (Params
                      [(EBin_op (Plus, (EConst (VInt 1)), (EConst (VInt 2))));
                        (EIdentifier (Id "d")); (EConst (VString "qwe"))])
                   )),
                (EConst (VInt 100000)))))
       )) |}]
;;

let%expect_test "Declaration only" =
  test_pp_ep_decl {|string        egor|};
  [%expect
    {|
    (SDecl ((Var_decl ((TVariable (TVar (TNullable TString))), (Id "egor"))),
       None)) |}]
;;

(* ep_steps *)

let test_pp_steps = print_pars ep_steps pp_statement

let%expect_test "Body with conditions" =
  test_pp_steps
    {|   {if (true) 
                  { a(); 
                    if(false) 
                      {
                        e    = b; 
                        return;
                      } else 
                        {
                          int  ?   exmp = 243 + 1;
                        }
                      }; ; ;     ; 
                      a(1+2 , cl)  ; ;   ; 
                      if (1+ run()) 
                            {
                              first(1);
                            } else if (true) {} 
                      return 1+1; ; ;
              }|};
  [%expect
    {|
      (Steps
         [(SIf_else ((EConst (VBool true)),
             (Steps
                [(SExpr (EMethod_invoke ((EIdentifier (Id "a")), (Params []))));
                  (SIf_else ((EConst (VBool false)),
                     (Steps
                        [(SExpr
                            (EBin_op (Assign, (EIdentifier (Id "e")),
                               (EIdentifier (Id "b")))));
                          (SReturn None)]),
                     (Some (Steps
                              [(SDecl (
                                  (Var_decl (
                                     (TVariable (TVar (TNullable (TBase TInt)))),
                                     (Id "exmp"))),
                                  (Some (EBin_op (Plus, (EConst (VInt 243)),
                                           (EConst (VInt 1)))))
                                  ))
                                ]))
                     ))
                  ]),
             None));
           (SExpr
              (EMethod_invoke ((EIdentifier (Id "a")),
                 (Params
                    [(EBin_op (Plus, (EConst (VInt 1)), (EConst (VInt 2))));
                      (EIdentifier (Id "cl"))])
                 )));
           (SIf_else (
              (EBin_op (Plus, (EConst (VInt 1)),
                 (EMethod_invoke ((EIdentifier (Id "run")), (Params []))))),
              (Steps
                 [(SExpr
                     (EMethod_invoke ((EIdentifier (Id "first")),
                        (Params [(EConst (VInt 1))]))))
                   ]),
              (Some (SIf_else ((EConst (VBool true)), (Steps []), None)))));
           (SReturn (Some (EBin_op (Plus, (EConst (VInt 1)), (EConst (VInt 1))))))])

     |}]
;;

let test_pp_fuc = print_pars ep_method_member pp_class_member

let%expect_test "Method parsing" =
  test_pp_fuc
    {|static int Fac(int num)
        {
            if (num == 1)
            {
                return 1;
            }
            else 
            {
                return num * Fac(num - 1);
            }
        }|};
  [%expect
    {|
    (Method (
       { m_modif = (Some MStatic); m_type = (TReturn (TNot_Nullable TInt));
         m_id = (Id "Fac");
         m_args =
         (Args [(Var_decl ((TVariable (TVar (TNot_Nullable TInt))), (Id "num")))])
         },
       (Steps
          [(SIf_else (
              (EBin_op (Equal, (EIdentifier (Id "num")), (EConst (VInt 1)))),
              (Steps [(SReturn (Some (EConst (VInt 1))))]),
              (Some (Steps
                       [(SReturn
                           (Some (EBin_op (Asterisk, (EIdentifier (Id "num")),
                                    (EMethod_invoke ((EIdentifier (Id "Fac")),
                                       (Params
                                          [(EBin_op (Minus,
                                              (EIdentifier (Id "num")),
                                              (EConst (VInt 1))))
                                            ])
                                       ))
                                    ))))
                         ]))
              ))
            ])
       )) |}]
;;

let test_pp_class = print_pars ep_class pp_class_decl

let%expect_test _ =
  test_pp_class
    {|class Program : Exception
     {
        int A1 = 0;
        public MyClass A2;
        static int Fac(int num)
        {
            if (num == 1)
            {
                return 1;
            }
            else 
            {
                return num * Fac(num - 1);
            }
        }
     }|};
  [%expect
    {|
   { cl_modif = None; cl_id = (Id "Program"); parent = (Some (Id "Exception"));
     cl_mems =
     [(Fild
         { f_modif = None; f_type = (TVar (TNot_Nullable TInt));
           f_id = (Id "A1"); f_val = (Some (EConst (VInt 0))) });
       (Fild
          { f_modif = (Some (FAccess MPublic));
            f_type = (TVar (TNullable (TClass (Id "MyClass"))));
            f_id = (Id "A2"); f_val = None });
       (Method (
          { m_modif = (Some MStatic); m_type = (TReturn (TNot_Nullable TInt));
            m_id = (Id "Fac");
            m_args =
            (Args
               [(Var_decl ((TVariable (TVar (TNot_Nullable TInt))), (Id "num")))
                 ])
            },
          (Steps
             [(SIf_else (
                 (EBin_op (Equal, (EIdentifier (Id "num")), (EConst (VInt 1)))),
                 (Steps [(SReturn (Some (EConst (VInt 1))))]),
                 (Some (Steps
                          [(SReturn
                              (Some (EBin_op (Asterisk,
                                       (EIdentifier (Id "num")),
                                       (EMethod_invoke (
                                          (EIdentifier (Id "Fac")),
                                          (Params
                                             [(EBin_op (Minus,
                                                 (EIdentifier (Id "num")),
                                                 (EConst (VInt 1))))
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
    {| class Program : Exception
     {
        int A1 = 0;
        public MyClass A2;
        static int Fac(int num)
        {
            if (num == 1)
            {
                return 1;
            }
            else 
            {
                return num * Fac(num - 1);
            }
        }
     }
         class trueF {}|};
  [%expect
    {|
  (Ast
     [{ cl_modif = None; cl_id = (Id "Program");
        parent = (Some (Id "Exception"));
        cl_mems =
        [(Fild
            { f_modif = None; f_type = (TVar (TNot_Nullable TInt));
              f_id = (Id "A1"); f_val = (Some (EConst (VInt 0))) });
          (Fild
             { f_modif = (Some (FAccess MPublic));
               f_type = (TVar (TNullable (TClass (Id "MyClass"))));
               f_id = (Id "A2"); f_val = None });
          (Method (
             { m_modif = (Some MStatic);
               m_type = (TReturn (TNot_Nullable TInt)); m_id = (Id "Fac");
               m_args =
               (Args
                  [(Var_decl ((TVariable (TVar (TNot_Nullable TInt))),
                      (Id "num")))
                    ])
               },
             (Steps
                [(SIf_else (
                    (EBin_op (Equal, (EIdentifier (Id "num")),
                       (EConst (VInt 1)))),
                    (Steps [(SReturn (Some (EConst (VInt 1))))]),
                    (Some (Steps
                             [(SReturn
                                 (Some (EBin_op (Asterisk,
                                          (EIdentifier (Id "num")),
                                          (EMethod_invoke (
                                             (EIdentifier (Id "Fac")),
                                             (Params
                                                [(EBin_op (Minus,
                                                    (EIdentifier (Id "num")),
                                                    (EConst (VInt 1))))
                                                  ])
                                             ))
                                          ))))
                               ]))
                    ))
                  ])
             ))
          ]
        };
       { cl_modif = None; cl_id = (Id "trueF"); parent = None; cl_mems = [] }]) |}]
;;
