  $ dune exec demoParse << EOF
  > let rec factorial_recursive = fun n -> if n <= 1 then 1 else n * factorial_recursive (n - 1)
  > EOF
  [(DLet
      ((DRec true), (LName "factorial_recursive"),
       (EFun ((PVar (LName "n")),
          (EIf ((EBinop (Leq, (EVar (LName "n")), (EInt 1))), (EInt 1),
             (EBinop (Mul, (EVar (LName "n")),
                (EApp ((EVar (LName "factorial_recursive")),
                   (EBinop (Sub, (EVar (LName "n")), (EInt 1)))))
                ))
             ))
          ))))
    ]
  $ dune exec demoParse << EOF
  > let rec fix = fun f -> (fun x -> f (fix f) x)
  > let fac = fix (fun self -> (fun n -> if n <= 1 then 1 else n * self (n - 1)))
  > EOF
  [(DLet
      ((DRec true), (LName "fix"),
       (EFun ((PVar (LName "f")),
          (EFun ((PVar (LName "x")),
             (EApp (
                (EApp ((EVar (LName "f")),
                   (EApp ((EVar (LName "fix")), (EVar (LName "f")))))),
                (EVar (LName "x"))))
             ))
          ))));
    (DLet
       ((DRec false), (LName "fac"),
        (EApp ((EVar (LName "fix")),
           (EFun ((PVar (LName "self")),
              (EFun ((PVar (LName "n")),
                 (EIf ((EBinop (Leq, (EVar (LName "n")), (EInt 1))), (EInt 1),
                    (EBinop (Mul, (EVar (LName "n")),
                       (EApp ((EVar (LName "self")),
                          (EBinop (Sub, (EVar (LName "n")), (EInt 1)))))
                       ))
                    ))
                 ))
              ))
           ))))
    ]
  $ dune exec demoParse << EOF
  > type node = | Red of int | Black of int
  > let is_black = fun x -> match x with | Black _ -> true | Red _ -> false
  > let a = is_black (Black 5)
  > EOF
  [(DType
      ((LName "node"),
       [((UName "Red"), (DType TInt)); ((UName "Black"), (DType TInt))]));
    (DLet
       ((DRec false), (LName "is_black"),
        (EFun ((PVar (LName "x")),
           (EMatch ((EVar (LName "x")),
              [((PAdt ((UName "Black"), (Some PWild))), (EBool true));
                ((PAdt ((UName "Red"), (Some PWild))), (EBool false))]
              ))
           ))));
    (DLet
       ((DRec false), (LName "a"),
        (EApp ((EVar (LName "is_black")),
           (EConstr ((UName "Black"), (Some (EInt 5))))))))
    ]
  $ dune exec demoParse << EOF
  > type color = | White | Green | Yellow | Blue | Red | Black
  > type eatable = | Yes | No
  > type thing = | Apple of color * eatable | Chair of color * eatable | Potato of color * eatable
  > let a = Apple (Yellow, Yes)
  > EOF
  [(DType
      ((LName "color"),
       [((UName "White"), (DType TEmptyType));
         ((UName "Green"), (DType TEmptyType));
         ((UName "Yellow"), (DType TEmptyType));
         ((UName "Blue"), (DType TEmptyType));
         ((UName "Red"), (DType TEmptyType));
         ((UName "Black"), (DType TEmptyType))]));
    (DType
       ((LName "eatable"),
        [((UName "Yes"), (DType TEmptyType));
          ((UName "No"), (DType TEmptyType))]));
    (DType
       ((LName "thing"),
        [((UName "Apple"),
          (DType (TTuple [(TVar (LName "color")); (TVar (LName "eatable"))])));
          ((UName "Chair"),
           (DType (TTuple [(TVar (LName "color")); (TVar (LName "eatable"))])));
          ((UName "Potato"),
           (DType (TTuple [(TVar (LName "color")); (TVar (LName "eatable"))])))
          ]));
    (DLet
       ((DRec false), (LName "a"),
        (EConstr ((UName "Apple"),
           (Some (ETuple
                    [(EConstr ((UName "Yellow"), None));
                      (EConstr ((UName "Yes"), None))]))
           ))))
    ]
  $ dune exec demoParse << EOF
  > type color = | Red | Black
  > 
  > type rbtree =
  > | Empty
  > | Node of color * int * rbtree * rbtree
  > 
  > let rec member = fun x -> 
  > (fun n -> 
  >  match n with 
  >  | Empty -> false
  >  | Node (_, y, left, right) -> if x == y then true else if x < y then member x left else member x right)
  > 
  > let node_left_left = Node(Black, 3, Empty, Empty)
  > 
  > let node_left = Node (Red, 4, node_left_left, Empty)
  > 
  > let node_right = Node(Red, 10, Empty, Empty) 
  > 
  > let node = Node (Black, 5, node_left, node_right)
  > 
  > let is_member = member 4 node
  > 
  > let is_member2 = member 52 node
  [(DType
      ((LName "color"),
       [((UName "Red"), (DType TEmptyType));
         ((UName "Black"), (DType TEmptyType))]));
    (DType
       ((LName "rbtree"),
        [((UName "Empty"), (DType TEmptyType));
          ((UName "Node"),
           (DType
              (TTuple
                 [(TVar (LName "color")); TInt; (TVar (LName "rbtree"));
                   (TVar (LName "rbtree"))])))
          ]));
    (DLet
       ((DRec true), (LName "member"),
        (EFun ((PVar (LName "x")),
           (EFun ((PVar (LName "n")),
              (EMatch ((EVar (LName "n")),
                 [((PAdt ((UName "Empty"), None)), (EBool false));
                   ((PAdt ((UName "Node"),
                       (Some (PTuple
                                [PWild; (PVar (LName "y"));
                                  (PVar (LName "left")); (PVar (LName "right"))
                                  ]))
                       )),
                    (EIf (
                       (EBinop (Eq, (EVar (LName "x")), (EVar (LName "y")))),
                       (EBool true),
                       (EIf (
                          (EBinop (Les, (EVar (LName "x")), (EVar (LName "y"))
                             )),
                          (EApp (
                             (EApp ((EVar (LName "member")), (EVar (LName "x"))
                                )),
                             (EVar (LName "left")))),
                          (EApp (
                             (EApp ((EVar (LName "member")), (EVar (LName "x"))
                                )),
                             (EVar (LName "right"))))
                          ))
                       )))
                   ]
                 ))
              ))
           ))));
    (DLet
       ((DRec false), (LName "node_left_left"),
        (EConstr ((UName "Node"),
           (Some (ETuple
                    [(EConstr ((UName "Black"), None)); (EInt 3);
                      (EConstr ((UName "Empty"), None));
                      (EConstr ((UName "Empty"), None))]))
           ))));
    (DLet
       ((DRec false), (LName "node_left"),
        (EConstr ((UName "Node"),
           (Some (ETuple
                    [(EConstr ((UName "Red"), None)); (EInt 4);
                      (EVar (LName "node_left_left"));
                      (EConstr ((UName "Empty"), None))]))
           ))));
    (DLet
       ((DRec false), (LName "node_right"),
        (EConstr ((UName "Node"),
           (Some (ETuple
                    [(EConstr ((UName "Red"), None)); (EInt 10);
                      (EConstr ((UName "Empty"), None));
                      (EConstr ((UName "Empty"), None))]))
           ))));
    (DLet
       ((DRec false), (LName "node"),
        (EConstr ((UName "Node"),
           (Some (ETuple
                    [(EConstr ((UName "Black"), None)); (EInt 5);
                      (EVar (LName "node_left")); (EVar (LName "node_right"))]))
           ))));
    (DLet
       ((DRec false), (LName "is_member"),
        (EApp ((EApp ((EVar (LName "member")), (EInt 4))),
           (EVar (LName "node"))))));
    (DLet
       ((DRec false), (LName "is_member2"),
        (EApp ((EApp ((EVar (LName "member")), (EInt 52))),
           (EVar (LName "node"))))))
    ]
