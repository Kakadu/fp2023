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
               , EBin_op (Plus, EIdentifier (Id "a"), EConst (VInt 2))
               , EBin_op
                   ( Asterisk
                   , EBin_op
                       ( Division
                       , EMember_ident
                           ( EIdentifier (Id "t")
                           , EMember_ident (EIdentifier (Id "a"), EIdentifier (Id "b")) )
                       , EMember_ident (EIdentifier (Id "t"), EIdentifier (Id "r")) )
                   , EConst (VInt 2) ) ) ) ))
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
    {|a (1+2,  d  , "qwe") + 100000|}
    (EBin_op
       ( Plus
       , EMethod_invoke
           ( EIdentifier (Id "a")
           , Params
               [ EBin_op (Plus, EConst (VInt 1), EConst (VInt 2))
               ; EIdentifier (Id "d")
               ; EConst (VString "qwe")
               ] )
       , EConst (VInt 100000) ))
;;

(* ep_eAssign_eDecl *)
let test_eAssign_eDecl = test_pars ep_eAssign_eDecl equal_expr

let%test _ =
  test_eAssign_eDecl
    {|a         egor    =    a (1+2,  d  , "qwe") + 100000|}
    (EAssign
       ( EDecl (TVariable (TVar (TNullable (TClass (Id "a")))), Id "egor")
       , EBin_op
           ( Plus
           , EMethod_invoke
               ( EIdentifier (Id "a")
               , Params
                   [ EBin_op (Plus, EConst (VInt 1), EConst (VInt 2))
                   ; EIdentifier (Id "d")
                   ; EConst (VString "qwe")
                   ] )
           , EConst (VInt 100000) ) ))
;;

(* ep_steps *)

let test_steps = test_pars ep_steps equal_expr

let%test _ =
  test_steps
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
          }|}
    (Steps
       [ EIf_else
           ( EConst (VBool true)
           , Steps
               [ EMethod_invoke (EIdentifier (Id "a"), Params [])
               ; EIf_else
                   ( EConst (VBool false)
                   , Steps
                       [ EBin_op (Assign, EIdentifier (Id "e"), EIdentifier (Id "b"))
                       ; EReturn None
                       ]
                   , Some
                       (Steps
                          [ EAssign
                              ( EDecl
                                  (TVariable (TVar (TNullable (TBase TInt))), Id "exmp")
                              , EBin_op (Plus, EConst (VInt 243), EConst (VInt 1)) )
                          ]) )
               ]
           , None )
       ; EMethod_invoke
           ( EIdentifier (Id "a")
           , Params
               [ EBin_op (Plus, EConst (VInt 1), EConst (VInt 2)); EIdentifier (Id "cl") ]
           )
       ; EIf_else
           ( EBin_op
               (Plus, EConst (VInt 1), EMethod_invoke (EIdentifier (Id "run"), Params []))
           , Steps
               [ EMethod_invoke (EIdentifier (Id "first"), Params [ EConst (VInt 1) ]) ]
           , Some (EIf_else (EConst (VBool true), Steps [], None)) )
       ; EReturn (Some (EBin_op (Plus, EConst (VInt 1), EConst (VInt 1))))
       ])
;;

let test_pp_steps = print_pars ep_steps pp_expr

let%expect_test _ =
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
         [(EIf_else ((EConst (VBool true)),
             (Steps
                [(EMethod_invoke ((EIdentifier (Id "a")), (Params [])));
                  (EIf_else ((EConst (VBool false)),
                     (Steps
                        [(EBin_op (Assign, (EIdentifier (Id "e")),
                            (EIdentifier (Id "b"))));
                          (EReturn None)]),
                     (Some (Steps
                              [(EAssign (
                                  (EDecl (
                                     (TVariable (TVar (TNullable (TBase TInt)))),
                                     (Id "exmp"))),
                                  (EBin_op (Plus, (EConst (VInt 243)),
                                     (EConst (VInt 1))))
                                  ))
                                ]))
                     ))
                  ]),
             None));
           (EMethod_invoke ((EIdentifier (Id "a")),
              (Params
                 [(EBin_op (Plus, (EConst (VInt 1)), (EConst (VInt 2))));
                   (EIdentifier (Id "cl"))])
              ));
           (EIf_else (
              (EBin_op (Plus, (EConst (VInt 1)),
                 (EMethod_invoke ((EIdentifier (Id "run")), (Params []))))),
              (Steps
                 [(EMethod_invoke ((EIdentifier (Id "first")),
                     (Params [(EConst (VInt 1))])))
                   ]),
              (Some (EIf_else ((EConst (VBool true)), (Steps []), None)))));
           (EReturn (Some (EBin_op (Plus, (EConst (VInt 1)), (EConst (VInt 1))))))])

     |}]
;;

let test_pp_fuc = print_pars ep_method_member pp_class_member

let%expect_test _ =
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
         (Args [(EDecl ((TVariable (TVar (TNot_Nullable TInt))), (Id "num")))]) },
       (Steps
          [(EIf_else (
              (EBin_op (Equal, (EIdentifier (Id "num")), (EConst (VInt 1)))),
              (Steps [(EReturn (Some (EConst (VInt 1))))]),
              (Some (Steps
                       [(EReturn
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

let test_pp_class = print_pars ep_class pp_class_sign

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
               [(EDecl ((TVariable (TVar (TNot_Nullable TInt))), (Id "num")))])
            },
          (Steps
             [(EIf_else (
                 (EBin_op (Equal, (EIdentifier (Id "num")), (EConst (VInt 1)))),
                 (Steps [(EReturn (Some (EConst (VInt 1))))]),
                 (Some (Steps
                          [(EReturn
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
  [{ cl_modif = None; cl_id = (Id "Program"); parent = (Some (Id "Exception"));
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
               [(EDecl ((TVariable (TVar (TNot_Nullable TInt))), (Id "num")))])
            },
          (Steps
             [(EIf_else (
                 (EBin_op (Equal, (EIdentifier (Id "num")), (EConst (VInt 1)))),
                 (Steps [(EReturn (Some (EConst (VInt 1))))]),
                 (Some (Steps
                          [(EReturn
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
    { cl_modif = None; cl_id = (Id "trueF"); parent = None; cl_mems = [] }] |}]
;;
