  $ dune exec demo_fact
  [(Str_value
      { d_rec = Recursive; d_pat = (Pat_var (Id "fact"));
        d_expr =
        (Exp_function ((Pat_var (Id "x")),
           (Exp_ifthenelse (
              (Exp_bin_op (Eq, (Exp_ident (Id "x")),
                 (Exp_constant (Const_int 0)))),
              (Exp_constant (Const_int 1)),
              (Exp_bin_op (Asterisk, (Exp_ident (Id "x")),
                 (Exp_apply ((Exp_ident (Id "fact")),
                    (Exp_bin_op (Sub, (Exp_ident (Id "x")),
                       (Exp_constant (Const_int 1))))
                    ))
                 ))
              ))
           ))
        })
    ]
  $ dune exec demo_obj
  [(Str_value
      { d_rec = Nonrecursive; d_pat = (Pat_var (Id "minmax"));
        d_expr =
        (Exp_function ((Pat_var (Id "x")),
           (Exp_function ((Pat_var (Id "y")),
              (Exp_ifthenelse (
                 (Exp_bin_op (Lt, (Exp_ident (Id "x")), (Exp_ident (Id "y")))),
                 (Exp_object
                    { o_self = Pat_any;
                      o_fields =
                      [(Obj_method (Public, (Id "min"), (Exp_ident (Id "x"))));
                        (Obj_method (Public, (Id "max"), (Exp_ident (Id "y"))))
                        ]
                      }),
                 (Exp_object
                    { o_self = Pat_any;
                      o_fields =
                      [(Obj_method (Public, (Id "min"), (Exp_ident (Id "y"))));
                        (Obj_method (Public, (Id "max"), (Exp_ident (Id "x"))))
                        ]
                      })
                 ))
              ))
           ))
        })
    ]
  $ dune exec demo_obj1
  [(Str_value
      { d_rec = Nonrecursive; d_pat = (Pat_var (Id "p"));
        d_expr =
        (Exp_object
           { o_self = (Pat_var (Id "s"));
             o_fields =
             [(Obj_val ((Id "x"), (Exp_constant (Const_int 5))));
               (Obj_method (Private, (Id "get_x"), (Exp_ident (Id "x"))));
               (Obj_method (Public, (Id "print"),
                  (Exp_apply ((Exp_ident (Id "print_int")),
                     (Exp_send ((Exp_ident (Id "s")), (Id "get_x")))))
                  ))
               ]
             })
        })
    ]

  $ cat << EOF | dune exec demo - 
  > let sum x y = x + y ;;
  > 
  > let is_ten n =
  >   match n with
  >   | 10 -> true
  >   | _ -> false
  > ;; 
  > 
  > let incr x = x + 1 ;;
  > EOF
  (Str_value
     { d_rec = Nonrecursive; d_pat = (Pat_var (Id "sum"));
       d_expr =
       (Exp_function ((Pat_var (Id "x")),
          (Exp_function ((Pat_var (Id "y")),
             (Exp_bin_op (Plus, (Exp_ident (Id "x")), (Exp_ident (Id "y"))))))
          ))
       })
  (Str_value
     { d_rec = Nonrecursive; d_pat = (Pat_var (Id "is_ten"));
       d_expr =
       (Exp_function ((Pat_var (Id "n")),
          (Exp_match ((Exp_ident (Id "n")),
             [((Pat_const (Const_int 10)), (Exp_constant (Const_bool true)));
               (Pat_any, (Exp_constant (Const_bool false)))]
             ))
          ))
       })
  (Str_value
     { d_rec = Nonrecursive; d_pat = (Pat_var (Id "incr"));
       d_expr =
       (Exp_function ((Pat_var (Id "x")),
          (Exp_bin_op (Plus, (Exp_ident (Id "x")), (Exp_constant (Const_int 1))
             ))
          ))
       })

