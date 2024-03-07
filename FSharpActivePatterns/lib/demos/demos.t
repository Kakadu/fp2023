  $ ./demos.exe <<- EOF
  > 777;;
  (VInt 777)

  $ ./demos.exe <<- EOF
  > fun x -> x * x ;;
  (VFun ((Var "x"), (BinExpr (Mul, (VarExpr "x"), (VarExpr "x"))), []))

  $ ./demos.exe <<- EOF
  > "itsastring";;
  (VString "itsastring")

  $ ./demos.exe <<- EOF
  > 951753;;
  (VInt 951753)

  $ ./demos.exe <<- EOF
  > (1,(2),3,[1;2;3]);;
  (VTuple
     [(VInt 1); (VInt 2); (VInt 3); (VList [(VInt 1); (VInt 2); (VInt 3)])])

  $ ./demos.exe <<- EOF
  > [((1));2;3];;
  (VList [(VInt 1); (VInt 2); (VInt 3)])

  $ ./demos.exe <<- EOF
  > let f x = x+x;;
  > f 50;;
  (VInt 100)

  $ ./demos.exe <<- EOF
  > 7 + 10 + 4 * 50 + 19 / 3 + (10 - 5);;
  (VInt 228)

  $ ./demos.exe <<- EOF
  > 5/0;;
  (Error while interpreting): Exception: Division_by_zero.

  $ ./demos.exe <<- EOF
  > let rec fact n = if n = 1 then 1 else n * (fact (n - 1));;
  > fact 5;;
  (VInt 120)

  $ ./demos.exe <<- EOF
  > let check input = match input with | 2 -> 5 | _ -> 10;;
  > check 2;;
  (VInt 5)

  $ ./demos.exe <<- EOF
  > let (|Even|Odd|) value = if value % 2 = 0 then Even else Odd;;
  > let check value =
  >   match value with
  >     | Even -> 25
  >     | Odd -> 53;;
  > check 13;;
  (VInt 53)
 
  $ ./demos.exe <<- EOF
  > let (|Even|Odd|) value = if value % 2 = 0 then Even else Odd;;
  > let check value =
  >   match value with
  >     | Even -> 25
  >     | Odd -> 53;;
  > check 15;;
  (VInt 53)

  $ ./demos.exe <<- EOF
  > (53*12 - 11 / 1 + 3 - 7 ) > (65 + 52 * 99 - 1000*2 + 11 );;
  (VBool false)

  $ ./demos.exe <<- EOF
  > (fun x b -> x * b) 105 10;;
  (VInt 1050)

  $ ./demos.exe <<- EOF
  > (15,12,2);;
  (VTuple [(VInt 15); (VInt 12); (VInt 2)])

  $ ./demos.exe <<- EOF
  > let z = 5;;
  > let x = 20;; 
  > let v = x * z;;
  (VInt 100)

  $ ./demos.exe <<- EOF
  > let check x = if x % 2 = 0 then 10 else 1000;;
  > check 54;; 
  (VInt 10)

  $ ./demos.exe <<- EOF
  > let (|Default|) value =
  >  match value with
  >  | value -> (value * value);;
  > let greet (Default value) = value;;
  > greet 100;;
  (VInt 10000)

  $ ./demos.exe <<- EOF
  >  let (|Even|_|) v = if v % 2 = 0 then Even else None;;
  >  let (|Odd|_|) v = if v % 2 <> 0 then Odd else None;;
  >  let myfunc v =
  >  match v with
  >  | Even -> 50
  >  | Odd -> 25
  >  | _ -> 6;;
  >  myfunc 8;;
  (VInt 50)
