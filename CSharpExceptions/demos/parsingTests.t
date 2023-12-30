Copyright 2021-2023, Georgy Sichkar
SPDX-License-Identifier: CC0-1.0

  $ ./demoParse.exe << HATETESTS
  > class Program
  > {
  >    int Fac(int num)
  >    {  
  >        if (num == 1)
  >        {
  >            return 1;
  >        }
  >        else 
  >        {
  >            return num * Fac(num - 1);
  >        }
  >    }
  > 
  >    static void Main() 
  >     {
  >       int number;
  >       number = 20;
  >       return Fac(number);
  >     }
  > }
  > HATETESTS
  (Ast
     [{ cl_modif = None; cl_id = (Id "Program"); parent = None;
        cl_mems =
        [(Method (
            { m_modif = None; m_type = (TReturn (TNot_Nullable TInt));
              m_id = (Id "Fac");
              m_args =
              (Args [(Var_decl ((TVar (TNot_Nullable TInt)), (Id "num")))]) },
            (Steps
               [(SIf_else (
                   (EBin_op (Equal, (EIdentifier (Id "num")), (EConst (VInt 1))
                      )),
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
            ));
          (Main
             (Steps
                [(SDecl (
                    (Var_decl ((TVar (TNot_Nullable TInt)), (Id "number"))),
                    None));
                  (SExpr
                     (EBin_op (Assign, (EIdentifier (Id "number")),
                        (EConst (VInt 20)))));
                  (SReturn
                     (Some (EMethod_invoke ((EIdentifier (Id "Fac")),
                              (Params [(EIdentifier (Id "number"))])))))
                  ]))
          ]
        }
       ])
