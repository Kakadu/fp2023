(** Copyright 2021-2023, Julia Kononova *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Csharp_lib.Parser
open Csharp_lib.Ast

(* PARSERS *)
let show_wrap form = function
  | Some x -> Format.printf "%a@\n" form x
  | _ -> Format.print_string "Some error\n"
;;

let print_pars ps pp str = show_wrap pp (parse_to_some ps str)

(* TESTS *)

(* statements tests*)
let test_decl = print_pars s_declaration pp_statement

let%expect_test _ =
  test_decl "   int  ?  name = (25 + 9) *  7";
  [%expect
    {|
      (Decl ((Var_Declaration ((TVar (TNullable_type TInt)), (Name "name"))),
         (Some (Bin_op (Asterisk,
                  (Bin_op (Plus, (Exp_Const (VInt 25)), (Exp_Const (VInt 9)))),
                  (Exp_Const (VInt 7)))))
         )) |}]
;;

let test_body = print_pars p_body pp_statement

let%expect_test _ =
  test_body
    "{if (true) \n\
    \      { a(); \n\
    \        if(false) \n\
    \          {\n\
    \            e    = b; \n\
    \            return;\n\
    \          } else \n\
    \            {\n\
    \              int  ?   exmp = 243 + 1;\n\
    \            }\n\
    \          }; ; ;     ; \n\
    \          a(1+2 , cl)  ; ;   ; \n\
    \          if (1+ run()) \n\
    \                {\n\
    \                  first(1);\n\
    \                } else if (true) {} \n\
    \          return 1+1; ; ;\n\
    \  }";
  [%expect
    {|
    (Body
       [(If ((Exp_Const (VBool true)),
           (Body
              [(Expr (Method_invoke ((Exp_Name (Name "a")), (Exp_Args []))));
                (If ((Exp_Const (VBool false)),
                   (Body
                      [(Expr
                          (Bin_op (Assign, (Exp_Name (Name "e")),
                             (Exp_Name (Name "b")))));
                        (Return None)]),
                   (Some (Body
                            [(Decl (
                                (Var_Declaration ((TVar (TNullable_type TInt)),
                                   (Name "exmp"))),
                                (Some (Bin_op (Plus, (Exp_Const (VInt 243)),
                                         (Exp_Const (VInt 1)))))
                                ))
                              ]))
                   ))
                ]),
           None));
         (Expr
            (Method_invoke ((Exp_Name (Name "a")),
               (Exp_Args
                  [(Bin_op (Plus, (Exp_Const (VInt 1)), (Exp_Const (VInt 2))));
                    (Exp_Name (Name "cl"))])
               )));
         (If (
            (Bin_op (Plus, (Exp_Const (VInt 1)),
               (Method_invoke ((Exp_Name (Name "run")), (Exp_Args []))))),
            (Body
               [(Expr
                   (Method_invoke ((Exp_Name (Name "first")),
                      (Exp_Args [(Exp_Const (VInt 1))]))))
                 ]),
            (Some (If ((Exp_Const (VBool true)), (Body []), None)))));
         (Return
            (Some (Bin_op (Plus, (Exp_Const (VInt 1)), (Exp_Const (VInt 1))))))
         ]) |}]
;;

let test_fac = print_pars c_method pp_method_sign

let%expect_test _ =
  test_fac
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
      (Method_Sign (
         (Method ((Some [Static]), (TRetrun (TNot_nullable_type (TBase TInt))),
            (Name "Fac"),
            (Params
               [(Var_Declaration ((TVar (TNot_nullable_type (TBase TInt))),
                   (Name "num")))
                 ])
            )),
         (Body
            [(If ((Bin_op (Equal, (Exp_Name (Name "num")), (Exp_Const (VInt 1)))),
                (Body [(Return (Some (Exp_Const (VInt 1))))]),
                (Some (Body
                         [(Return
                             (Some (Bin_op (Asterisk, (Exp_Name (Name "num")),
                                      (Method_invoke ((Exp_Name (Name "Fac")),
                                         (Exp_Args
                                            [(Bin_op (Dash,
                                                (Exp_Name (Name "num")),
                                                (Exp_Const (VInt 1))))
                                              ])
                                         ))
                                      ))))
                           ]))
                ))
              ])
         )) |}]
;;

(* class tests *)
let test_class = print_pars p_class pp_objects

let%expect_test _ =
  test_class
    "class Test {\n\
    \  static int Fac(int num) {\n\
    \    if (num == 1)\n\
    \    {\n\
    \        return 1;\n\
    \    }\n\
    \    else\n\
    \    {\n\
    \        return num * Fac(num - 1);\n\
    \    }\n\
    \  }\n\
     }";
  [%expect
    {|
  (Class ((Some []), (Name "Test"),
     [(CMethod
         (Method_Sign (
            (Method ((Some [Static]),
               (TRetrun (TNot_nullable_type (TBase TInt))), (Name "Fac"),
               (Params
                  [(Var_Declaration ((TVar (TNot_nullable_type (TBase TInt))),
                      (Name "num")))
                    ])
               )),
            (Body
               [(If (
                   (Bin_op (Equal, (Exp_Name (Name "num")),
                      (Exp_Const (VInt 1)))),
                   (Body [(Return (Some (Exp_Const (VInt 1))))]),
                   (Some (Body
                            [(Return
                                (Some (Bin_op (Asterisk,
                                         (Exp_Name (Name "num")),
                                         (Method_invoke (
                                            (Exp_Name (Name "Fac")),
                                            (Exp_Args
                                               [(Bin_op (Dash,
                                                   (Exp_Name (Name "num")),
                                                   (Exp_Const (VInt 1))))
                                                 ])
                                            ))
                                         ))))
                              ]))
                   ))
                 ])
            )))
       ]
     )) |}]
;;

let%expect_test _ =
  test_class
    "class Dog {\n\
    \    public string name = \"Undefined\";\n\
    \    public int age;\n\
    \    string breed;\n\n\
    \    public void bark() {\n\
    \      Console.WriteLine(\"Bark Bark !!\");\n\
    \    }\n\
    \    }";
  [%expect
    {|
      (Class ((Some []), (Name "Dog"),
         [(CField
             (Field_Sign (
                (Field ((Some [(FAccess Public)]),
                   (TVar (TNot_nullable_type (TRef TString))), (Name "name"))),
                (Some (Exp_Const (VString "Undefined"))))));
           (CField
              (Field_Sign (
                 (Field ((Some [(FAccess Public)]),
                    (TVar (TNot_nullable_type (TBase TInt))), (Name "age"))),
                 None)));
           (CField
              (Field_Sign (
                 (Field ((Some []), (TVar (TNot_nullable_type (TRef TString))),
                    (Name "breed"))),
                 None)));
           (CMethod
              (Method_Sign (
                 (Method ((Some [(MAccess Public)]), TVoid, (Name "bark"),
                    (Params []))),
                 (Body
                    [(Expr
                        (Method_invoke (
                           (Access_By_Point ((Exp_Name (Name "Console")),
                              (Exp_Name (Name "WriteLine")))),
                           (Exp_Args [(Exp_Const (VString "Bark Bark !!"))]))))
                      ])
                 )))
           ]
         )) |}]
;;

(* interface test *)
let test_interface = print_pars p_interface pp_objects

let%expect_test _ =
  test_interface
    "public interface Animal {\n\
    \  string name;\n\
    \  int age;\n\
    \  void animalSound();\n\
    \  void run();\n\
     }";
  [%expect
    {|
    (Interface ((Some (CAccess Public)), (Name "Animal"),
       [(IField
           (Field ((Some []), (TVar (TNot_nullable_type (TRef TString))),
              (Name "name"))));
         (IField
            (Field ((Some []), (TVar (TNot_nullable_type (TBase TInt))),
               (Name "age"))));
         (IMethod (Method ((Some []), TVoid, (Name "animalSound"), (Params []))));
         (IMethod (Method ((Some []), TVoid, (Name "run"), (Params []))))]
       )) |}]
;;

(* ast tests*)
let test_ast = print_pars p_ast pp_program

let%expect_test _ =
  test_ast
    "class Dog {\n\
    \    public string name = \"Undefined\";\n\
    \    public int age;\n\
    \    string breed;\n\n\
    \    public void bark() {\n\
    \      Console.WriteLine(\"Bark Bark !!\");\n\
    \    }\n\
    \    }";
  [%expect
    {|
      (Ast
         (Some [(Class ((Some []), (Name "Dog"),
                   [(CField
                       (Field_Sign (
                          (Field ((Some [(FAccess Public)]),
                             (TVar (TNot_nullable_type (TRef TString))),
                             (Name "name"))),
                          (Some (Exp_Const (VString "Undefined"))))));
                     (CField
                        (Field_Sign (
                           (Field ((Some [(FAccess Public)]),
                              (TVar (TNot_nullable_type (TBase TInt))),
                              (Name "age"))),
                           None)));
                     (CField
                        (Field_Sign (
                           (Field ((Some []),
                              (TVar (TNot_nullable_type (TRef TString))),
                              (Name "breed"))),
                           None)));
                     (CMethod
                        (Method_Sign (
                           (Method ((Some [(MAccess Public)]), TVoid,
                              (Name "bark"), (Params []))),
                           (Body
                              [(Expr
                                  (Method_invoke (
                                     (Access_By_Point ((Exp_Name (Name "Console")),
                                        (Exp_Name (Name "WriteLine")))),
                                     (Exp_Args
                                        [(Exp_Const (VString "Bark Bark !!"))])
                                     )))
                                ])
                           )))
                     ]
                   ))
                 ])) |}]
;;

let%expect_test _ =
  test_ast
    "class Pig {\n\
    \    public string name = \"Undefined\";\n\
    \    public int age;\n\
    \    public void animalSound() {\n\
    \      Console.WriteLine(\"The pig says: wee wee\");\n\
    \    }\n\
    \  }\n\
    \  class Dog {\n\
    \    public string name = \"Undefined\";\n\
    \    public int age;\n\
    \    string breed;\n\n\
    \    public void bark() {\n\
    \      Console.WriteLine(\"Bark Bark !!\");\n\
    \    }\n\
    \    }\n\
    \  ";
  [%expect
    {|
    (Ast
       (Some [(Class ((Some []), (Name "Pig"),
                 [(CField
                     (Field_Sign (
                        (Field ((Some [(FAccess Public)]),
                           (TVar (TNot_nullable_type (TRef TString))),
                           (Name "name"))),
                        (Some (Exp_Const (VString "Undefined"))))));
                   (CField
                      (Field_Sign (
                         (Field ((Some [(FAccess Public)]),
                            (TVar (TNot_nullable_type (TBase TInt))),
                            (Name "age"))),
                         None)));
                   (CMethod
                      (Method_Sign (
                         (Method ((Some [(MAccess Public)]), TVoid,
                            (Name "animalSound"), (Params []))),
                         (Body
                            [(Expr
                                (Method_invoke (
                                   (Access_By_Point ((Exp_Name (Name "Console")),
                                      (Exp_Name (Name "WriteLine")))),
                                   (Exp_Args
                                      [(Exp_Const
                                          (VString "The pig says: wee wee"))
                                        ])
                                   )))
                              ])
                         )))
                   ]
                 ));
               (Class ((Some []), (Name "Dog"),
                  [(CField
                      (Field_Sign (
                         (Field ((Some [(FAccess Public)]),
                            (TVar (TNot_nullable_type (TRef TString))),
                            (Name "name"))),
                         (Some (Exp_Const (VString "Undefined"))))));
                    (CField
                       (Field_Sign (
                          (Field ((Some [(FAccess Public)]),
                             (TVar (TNot_nullable_type (TBase TInt))),
                             (Name "age"))),
                          None)));
                    (CField
                       (Field_Sign (
                          (Field ((Some []),
                             (TVar (TNot_nullable_type (TRef TString))),
                             (Name "breed"))),
                          None)));
                    (CMethod
                       (Method_Sign (
                          (Method ((Some [(MAccess Public)]), TVoid,
                             (Name "bark"), (Params []))),
                          (Body
                             [(Expr
                                 (Method_invoke (
                                    (Access_By_Point (
                                       (Exp_Name (Name "Console")),
                                       (Exp_Name (Name "WriteLine")))),
                                    (Exp_Args
                                       [(Exp_Const (VString "Bark Bark !!"))])
                                    )))
                               ])
                          )))
                    ]
                  ))
               ])) |}]
;;
