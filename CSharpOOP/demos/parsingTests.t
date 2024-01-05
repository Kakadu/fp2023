(** Copyright 2021-2023, Julia Kononova *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

  $ ./demoParse.exe <<-EOF
  > class Test {
  >   static int Fac(int num) 
  >   {
  >      if (num == 1)
  >      {
  >         return 1;
  >      }
  >      else
  >      {
  >         return num * Fac(num - 1);
  >      }
  >   }
  > }
  > EOF
  (Ast
     (Some [(Class ((Some []), (Name "Test"),
               [(CMethod
                   (Method_Sign (
                      (Method ((Some [Static]),
                         (TRetrun (TNot_nullable_type (TBase TInt))),
                         (Name "Fac"),
                         (Params
                            [(Var_Declaration (
                                (TVar (TNot_nullable_type (TBase TInt))),
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
                                                             (Exp_Name
                                                                (Name "num")),
                                                             (Exp_Const
                                                                (VInt 1))
                                                             ))
                                                           ])
                                                      ))
                                                   ))))
                                        ]))
                             ))
                           ])
                      )))
                 ]
               ))
             ]))
