  $ ./interpret.exe <<EOF
  > let rec fact n = if n < 2 then 1 else n * fact (n - 1)
  [(Str_value (Recursive,
      [{ pat = (Pat_var "fact");
         expr =
         (Exp_function ([(Pat_var "n")],
            (Function_body
               (Exp_ifthenelse (
                  (Exp_apply (
                     (Exp_apply ((Exp_ident (Ident "<")),
                        (Exp_ident (Ident "n")))),
                     (Exp_constant (Const_integer 2)))),
                  (Exp_constant (Const_integer 1)),
                  (Some (Exp_apply (
                           (Exp_apply ((Exp_ident (Ident "*")),
                              (Exp_ident (Ident "n")))),
                           (Exp_apply ((Exp_ident (Ident "fact")),
                              (Exp_apply (
                                 (Exp_apply ((Exp_ident (Ident "-")),
                                    (Exp_ident (Ident "n")))),
                                 (Exp_constant (Const_integer 1))))
                              ))
                           )))
                  )))
            ))
         }
        ]
      ))
    ]
