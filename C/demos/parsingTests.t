  $ ./demoParse.exe << EOF
  > int factorial(int n) {
  >  if (n >= 1) {
  >   return n * factorial(n - 1);
  >  }
  >  else {
  >   return 1;
  >  }
  > }
  > int main() { 
  >  int n = 5;
  >  return factorial(n);
  > }
  > EOF
  [(Func_decl (ID_int32, "factorial", [(Arg (ID_int32, "n"))],
      (Compound
         [(If_else (
             (Bin_expr (GrowOrEqual, (Var_name "n"), (Const (V_int 1)))),
             (Compound
                [(Return
                    (Bin_expr (Mul, (Var_name "n"),
                       (Func_call ("factorial",
                          [(Bin_expr (Sub, (Var_name "n"), (Const (V_int 1))))]
                          ))
                       )))
                  ]),
             (Some (Compound [(Return (Const (V_int 1)))]))))
           ])
      ));
    (Func_decl (ID_int32, "main", [],
       (Compound
          [(Var_decl (ID_int32, "n", (Some (Expression (Const (V_int 5))))));
            (Return (Func_call ("factorial", [(Var_name "n")])))])
       ))
    ]

  $ ./demoParse.exe << EOF
  >  int binarySearch(int a, int *array, int n) {
  >   int low = 0;
  >   int high = n - 1;
  >   int middle;
  >   while (low <= high) {
  >     middle = (low + high) / 2;
  >     if (a < array[middle] || a > array[middle]) {
  >       if (a < array[middle]) {
  >         high = middle - 1;
  >       } 
  >       else {
  >         low = middle + 1;
  >       }
  >     } 
  >     else {
  >       return middle;
  >     } 
  >   }
  >   return -1;
  > }
  > 
  > int main() {
  >   int array[5] = {3, 7, 10, 23, 100};
  >   return binarySearch(7, array, 5);
  > }
  > EOF
  [(Func_decl (ID_int32, "binarySearch",
      [(Arg (ID_int32, "a")); (Arg ((Pointer ID_int32), "array"));
        (Arg (ID_int32, "n"))],
      (Compound
         [(Var_decl (ID_int32, "low", (Some (Expression (Const (V_int 0))))));
           (Var_decl (ID_int32, "high",
              (Some (Expression
                       (Bin_expr (Sub, (Var_name "n"), (Const (V_int 1))))))
              ));
           (Var_decl (ID_int32, "middle", None));
           (While (
              (Bin_expr (LessOrEqual, (Var_name "low"), (Var_name "high"))),
              (Compound
                 [(Assign ((Var_name "middle"),
                     (Expression
                        (Bin_expr (Div,
                           (Bin_expr (Add, (Var_name "low"), (Var_name "high")
                              )),
                           (Const (V_int 2)))))
                     ));
                   (If_else (
                      (Bin_expr (Or,
                         (Bin_expr (Less, (Var_name "a"),
                            (Index ((Var_name "array"), (Var_name "middle"))))),
                         (Bin_expr (Grow, (Var_name "a"),
                            (Index ((Var_name "array"), (Var_name "middle")))))
                         )),
                      (Compound
                         [(If_else (
                             (Bin_expr (Less, (Var_name "a"),
                                (Index ((Var_name "array"), (Var_name "middle")
                                   ))
                                )),
                             (Compound
                                [(Assign ((Var_name "high"),
                                    (Expression
                                       (Bin_expr (Sub, (Var_name "middle"),
                                          (Const (V_int 1)))))
                                    ))
                                  ]),
                             (Some (Compound
                                      [(Assign ((Var_name "low"),
                                          (Expression
                                             (Bin_expr (Add,
                                                (Var_name "middle"),
                                                (Const (V_int 1)))))
                                          ))
                                        ]))
                             ))
                           ]),
                      (Some (Compound [(Return (Var_name "middle"))]))))
                   ])
              ));
           (Return (Unary_expr (Minus, (Const (V_int 1)))))])
      ));
    (Func_decl (ID_int32, "main", [],
       (Compound
          [(Var_decl ((Array ((Some 5), ID_int32)), "array",
              (Some (Expression
                       (Array_value
                          [(Const (V_int 3)); (Const (V_int 7));
                            (Const (V_int 10)); (Const (V_int 23));
                            (Const (V_int 100))])))
              ));
            (Return
               (Func_call ("binarySearch",
                  [(Const (V_int 7)); (Var_name "array"); (Const (V_int 5))])))
            ])
       ))
    ]

  $ ./demoParse.exe << EOF
  > int main() {
  >   float b = c = 10.2;
  >   uint8_t aray[2][2][2] = {{{1, 2}, {3, 4}}, {{5, 6}, {7, 8}}};
  >   return array[1][0][0];
  > }
  > EOF
  [(Func_decl (ID_int32, "main", [],
      (Compound
         [(Var_decl (ID_float, "b",
             (Some (Assign ((Var_name "c"), (Expression (Const (V_float 10.2)))
                      )))
             ));
           (Var_decl (
              (Array ((Some 2),
                 (Array ((Some 2), (Array ((Some 2), ID_uint8)))))),
              "aray",
              (Some (Expression
                       (Array_value
                          [(Array_value
                              [(Array_value
                                  [(Const (V_int 1)); (Const (V_int 2))]);
                                (Array_value
                                   [(Const (V_int 3)); (Const (V_int 4))])
                                ]);
                            (Array_value
                               [(Array_value
                                   [(Const (V_int 5)); (Const (V_int 6))]);
                                 (Array_value
                                    [(Const (V_int 7)); (Const (V_int 8))])
                                 ])
                            ])))
              ));
           (Return
              (Index (
                 (Index ((Index ((Var_name "array"), (Const (V_int 1)))),
                    (Const (V_int 0)))),
                 (Const (V_int 0)))))
           ])
      ))
    ]
