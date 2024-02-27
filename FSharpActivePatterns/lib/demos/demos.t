  $ ./demo.exe <<- EOF
  > 777;;
  (VInt 777)

  $ ./demo.exe <<- EOF
  > fun x -> x * x ;;
  (VFun ((Var "x"), (BinExpr (Mul, (VarExpr "x"), (VarExpr "x"))), []))

  $ ./demo.exe <<- EOF
  > "itsastring";;
  (VString "itsastring")

  $ ./demo.exe <<- EOF
  > 951753;;
  (VInt 951753)

  $ ./demo.exe <<- EOF
  > (1,(2),3,[1;2;3]);;
  (VTuple
     [(VInt 1); (VInt 2); (VInt 3); (VList [(VInt 1); (VInt 2); (VInt 3)])])

  $ ./demo.exe <<- EOF
  > [((1));2;3];;
  (VList [(VInt 1); (VInt 2); (VInt 3)])

  $ ./demo.exe <<- EOF
  > let f x = x+x;;
  > f 50;;
  (VInt 100)

  $ ./demo.exe <<- EOF
  > 7 + 10 + 4 * 50 + 19 / 3 + (10 - 5);;
  (VInt 228)

  $ ./demo.exe <<- EOF
  > 5/0;;
  (Error while interpreting): Exception: Division_by_zero.

  $ ./demo.exe <<- EOF
  > let rec fact n = if n = 1 then 1 else n * (fact (n - 1));;
  > fact 5;;
  (VInt 120)

  $ ./demo.exe <<- EOF
  > let check input = match input with | 2 -> 5 | _ -> 10;;
  > check 2;;
  (VInt 5)

  $ ./demo.exe <<- EOF
  > let check input = match input with | 2 -> 5 | _ -> 10;;
  > check 5;;
  (VInt 10)

  $ ./demo.exe <<- EOF
  > let (|Even|Odd|) value = if value % 2 = 0 then Even else Odd;;
  > let check value =
  >   match value with
  >     | Even -> 25
  >     | Odd -> 53;;
  > check 14;;
  (VInt 25)
 
  $ ./demo.exe <<- EOF
  > let (|Even|Odd|) value = if value % 2 = 0 then Even else Odd;;
  > let check value =
  >   match value with
  >     | Even -> 25
  >     | Odd -> 53;;
  > check 15;;
  (VInt 53)

  $ ./demo.exe <<- EOF
  > (53*12 - 11 / 1 + 3 - 7 ) > (65 + 52 * 99 - 1000*2 + 11 );;
  (VBool false)

  $ ./demo.exe <<- EOF
  > (fun x b -> x * b) 105 10;;
  (VInt 1050)

  $ ./demo.exe <<- EOF
  > (15,12,2);;
  (VTuple [(VInt 15); (VInt 12); (VInt 2)])

  $ ./demo.exe <<- EOF
  > let z = 5;;
  > let x = 20;; 
  > let v = x * z;;
  (VInt 100)

  $ ./demo.exe <<- EOF
  > let check x = if x % 2 = 0 then 10 else 1000;;
  > check 54;; 
  (VInt 10)
