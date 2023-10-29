  $ dune exec demoFact
  (My_programm
     [(Func_def ((Func_decl (ID_int, "factorial", [(Arg (ID_int, "n"))])),
         (Compound
            [(If_else (
                (Bin_expr (GrowOrEqual, (Var_name "n"), (Const (V_int 1)))),
                (Compound
                   [(Return
                       (Bin_expr (Mul, (Var_name "n"),
                          (Func_call ("factorial",
                             [(Bin_expr (Sub, (Var_name "n"), (Const (V_int 1))
                                 ))
                               ]
                             ))
                          )))
                     ]),
                (Compound [(Return (Const (V_int 1)))])))
              ])
         ));
       (Func_def ((Func_decl (ID_int, "main", [])),
          (Compound
             [(Var_decl (ID_int, "n", (Some (Const (V_int 5)))));
               (Return (Func_call ("factorial", [(Var_name "n")])))])
          ))
       ])
