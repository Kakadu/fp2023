Copyright 2021-2023, Georgy Sichkar
SPDX-License-Identifier: CC0-1.0

  $ ./demoParse.exe < factorial.cs
  [{ cl_modif = None; cl_id = (Id "Program"); parent = None;
     cl_mems =
     [(Method (
         { m_modif = None; m_type = (TReturn (TNot_Nullable TInt));
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
         ));
       (Main (
          { m_modif = (Some MStatic); m_type = Void; m_id = (Id "Main");
            m_args = (Args []) },
          (Steps [])))
       ]
     }
    ]
