(Many_term                          
   [Relation {atom = (Name "factorial");
      terms = [(Const (Num 0)); (Const (Num 1))]};
     Relation {atom = (Oper ":-");
       terms =
       [Relation {atom = (Name "factorial");
          terms = [(Var "N"); (Var "Fact")]};
         Relation {atom = (Oper ",");
           terms =
           [Relation {atom = (Oper ",");
              terms =
              [Relation {atom = (Oper ",");
                 terms =
                 [Relation {atom = (Oper ">");
                    terms = [(Var "N"); (Const (Num 0))]};
                   Relation {atom = (Oper "is");
                     terms =
                     [(Var "N1");
                       Relation {atom = (Oper "-");
                         terms = [(Var "N"); (Const (Num 1))]}
                       ]}
                   ]};
                Relation {atom = (Name "factorial");
                  terms = [(Var "N1"); (Var "Fact1")]}
                ]};
             Relation {atom = (Oper "is");
               terms =
               [(Var "Fact");
                 Relation {atom = (Oper "*");
                   terms = [(Var "N"); (Var "Fact1")]}
                 ]}
             ]}
         ]}
     ])
