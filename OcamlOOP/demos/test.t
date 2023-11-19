  $ dune exec demo_fact
  LetRec
  (Pdecl
   (PVal (Id ("fact")),
    Fun
    (PVal (Id ("x")),
     IfThenElse
     (BinOp (Eq, EVal (Id ("x")), EConst (Int (0))), EConst (Int (1)),
      BinOp
      (Asterisk, EVal (Id ("x")),
       App (EVal (Id ("fact")), BinOp (Sub, EVal (Id ("x")), EConst (Int (1)))))))),
   EConst (Unit))
  $ dune exec demo_obj
  Let
  (Pdecl
   (PVal (Id ("minmax")),
    Fun
    (PVal (Id ("x")),
     Fun
     (PVal (Id ("y")),
      IfThenElse
      (BinOp (Lt, EVal (Id ("x")), EVal (Id ("y"))),
       Eobject
       (Pany,
        [Omethod (Public, PVal (Id ("min")), EVal (Id ("x")));
         Omethod (Public, PVal (Id ("max")), EVal (Id ("y")))]),
       Eobject
       (Pany,
        [Omethod (Public, PVal (Id ("min")), EVal (Id ("y")));
         Omethod (Public, PVal (Id ("max")), EVal (Id ("x")))]))))),
   EConst (Unit))
  $ dune exec demo_obj1
  Let
  (Pdecl
   (PVal (Id ("p")),
    Eobject
    (PVal (Id ("s")),
     [Oval (PVal (Id ("x")), EConst (Int (5)));
      Omethod (Private, PVal (Id ("get_x")), EVal (Id ("x")));
      Omethod
      (Public, PVal (Id ("print")),
       App
       (EVal (Id ("print_int")), Esend (EVal (Id ("s")), PVal (Id ("get_x")))))])),
   EConst (Unit))

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
  Let
  (Pdecl
   (PVal (Id ("sum")),
    Fun
    (PVal (Id ("x")),
     Fun (PVal (Id ("y")), BinOp (Plus, EVal (Id ("x")), EVal (Id ("y")))))),
   EConst (Unit))
  Let
  (Pdecl
   (PVal (Id ("is_ten")),
    Fun
    (PVal (Id ("n")),
     Match
     (EVal (Id ("n")),
      [(PConst (Int (10)), EConst (Bool (true)));
       (PVal (Id ("_")), EConst (Bool (false)))]))),
   EConst (Unit))
  Let
  (Pdecl
   (PVal (Id ("incr")),
    Fun (PVal (Id ("x")), BinOp (Plus, EVal (Id ("x")), EConst (Int (1))))),
   EConst (Unit))

