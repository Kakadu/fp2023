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
             [(Var_decl (ID_int, "n", (Some (Expression (Const (V_int 5))))));
               (Return (Func_call ("factorial", [(Var_name "n")])))])
          ))
       ])

  $ dune exec demoBinarySearch
  (My_programm
     [(Func_def (
         (Func_decl (ID_int, "binarySearch",
            [(Arg (ID_int, "a")); (Arg ((Pointer ID_int), "array"));
              (Arg (ID_int, "n"))]
            )),
         (Compound
            [(Var_decl (ID_int, "low", (Some (Expression (Const (V_int 0))))));
              (Var_decl (ID_int, "high",
                 (Some (Expression
                          (Bin_expr (Sub, (Var_name "n"), (Const (V_int 1))))))
                 ));
              (Var_decl (ID_int, "middle", None));
              (While (
                 (Bin_expr (LessOrEqual, (Var_name "low"), (Var_name "high"))),
                 (Compound
                    [(Assign ((Var_name "middle"),
                        (Expression
                           (Bin_expr (Div,
                              (Bin_expr (Add, (Var_name "low"),
                                 (Var_name "high"))),
                              (Const (V_int 2)))))
                        ));
                      (If_else (
                         (Bin_expr (Or,
                            (Bin_expr (Less, (Var_name "a"),
                               (Index ((Var_name "array"), (Var_name "middle")
                                  ))
                               )),
                            (Bin_expr (Grow, (Var_name "a"),
                               (Index ((Var_name "array"), (Var_name "middle")
                                  ))
                               ))
                            )),
                         (Compound
                            [(If_else (
                                (Bin_expr (Less, (Var_name "a"),
                                   (Index ((Var_name "array"),
                                      (Var_name "middle")))
                                   )),
                                (Compound
                                   [(Assign ((Var_name "high"),
                                       (Expression
                                          (Bin_expr (Sub, (Var_name "middle"),
                                             (Const (V_int 1)))))
                                       ))
                                     ]),
                                (Compound
                                   [(Assign ((Var_name "low"),
                                       (Expression
                                          (Bin_expr (Add, (Var_name "middle"),
                                             (Const (V_int 1)))))
                                       ))
                                     ])
                                ))
                              ]),
                         (Compound [(Return (Var_name "middle"))])))
                      ])
                 ));
              (Return (Unary_expr (Minus, (Const (V_int 1)))))])
         ));
       (Func_def ((Func_decl (ID_int, "main", [])),
          (Compound
             [(Var_decl ((Array ((Some 5), ID_int)), "array",
                 (Some (Expression
                          (Array_value
                             [(Const (V_int 3)); (Const (V_int 7));
                               (Const (V_int 10)); (Const (V_int 23));
                               (Const (V_int 100))])))
                 ));
               (Return
                  (Func_call ("binarySearch",
                     [(Const (V_int 7)); (Var_name "array"); (Const (V_int 5))]
                     )))
               ])
          ))
       ])
