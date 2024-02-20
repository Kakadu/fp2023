  $ ./demo.exe <<- EOF
  > 777;;
  (VInt 777)

  $ ./demo.exe <<- EOF
  > -777;;
  (VInt -777)

  $ ./demo.exe <<- EOF
  > 777.777;;
  (VFloat 777.777)

  $ ./demo.exe <<- EOF
  > -777.777;;
  (VFloat -777.777)

  $ ./demo.exe <<- EOF
  > "Test to string";;
  (VString "Test to string")

  $ ./demo.exe <<- EOF
  > true;;
  (VBool true)

  $ ./demo.exe <<- EOF
  > false;;
  (VBool false)

  $ ./demo.exe <<- EOF
  > [];;
  VNil

  $ ./demo.exe <<- EOF
  > ();;
  VUnit

  $ ./demo.exe <<- EOF
  > [7;7;7];;
  (VList [(VInt 7); (VInt 7); (VInt 7)])

  $ ./demo.exe <<- EOF
  > (777, -777, 7);;
  (VTuple [(VInt 777); (VInt -777); (VInt 7)])

  $ ./demo.exe <<- EOF
  > (24 / 8) * (6 - 3) * -5;;
  (VInt -45)

  $ ./demo.exe <<- EOF
  > -7.77 + 7.73;;
  (VFloat -0.04)

  $ ./demo.exe <<- EOF
  > 7.77 / 0.0;;
  (VFloat infinity)

  $ ./demo.exe <<- EOF
  > ((fun z v -> z * v)4 )5;;
  (VInt 20)

  $ ./demo.exe <<- EOF
  > ((fun z v -> z / v)4 )5;;
  (VInt 0)

  $ ./demo.exe <<- EOF
  > 777 % 2;;
  (VInt 1)

  $ ./demo.exe <<- EOF
  > 777 < 666;;
  (VBool false)

  $ ./demo.exe <<- EOF
  > 777 > 666;;
  (VBool true)

  $ ./demo.exe <<- EOF
  > true && false;;
  (VBool false)

  $ ./demo.exe <<- EOF
  > true || false;;
  (VBool true)

  $ ./demo.exe <<- EOF
  > (1, 2, 3) = (1, 2, 3);;
  (VBool true)

  $ ./demo.exe <<- EOF
  > [1; 2; 3] <> [1; 2; 3];;
  (VBool false)

  $ ./demo.exe <<- EOF
  > let num x = 
  >  match x with 
  >    | 1 -> 1
  >    | _ -> 2;;
  > num 10;;
  (VInt 2)

  $ ./demo.exe <<- EOF
  > (fun x -> (fun y -> x * y) 5) 5;;
  (VInt 25)

  $ ./demo.exe <<- EOF
  > (((((fun a b c d e -> a + b + c + d + e) 1) 2) 3) 4) 5;;
  (VInt 15)

  $ ./demo.exe <<- EOF
  > let num = 5;;
  (VInt 5)

  $ ./demo.exe <<- EOF
  > let sum x = fun y -> x + y;;
  > sum (sum 7 70) 700;;
  (VInt 777)

  $ ./demo.exe <<- EOF
  > let x = 7.77;; 
  > x;;
  (VFloat 7.77)

  $ ./demo.exe <<- EOF
  > (fun z v -> z % v) 777 5;;
  (VInt 2)

  $ ./demo.exe <<- EOF
  > let rec fact n = if n = 1 then 1 else n * fact (n - 1);;
  > fact 6;;
  (VInt 720)

  $ ./demo.exe <<- EOF
  > let rec fib n = if n <= 1 then n else fib (n - 1) + fib (n - 2);;
  > fib 9;;
  (VInt 34)

Units of Measure

  $ ./demo.exe <<- EOF
  > [<Measure>] type m;;
  (VMeasureList [("m", ["m"])])

  $ ./demo.exe <<- EOF
  > [<Measure>] type m^3;;
  Interpretation error. Unexpected infix operator in type definition

  $ ./demo.exe <<- EOF
  > [<Measure>] type m;;
  > 7.77<m^3> + 7.73<m^2>;;
  Interpretation error. The unit of measure 'm^3' does not match the unit of measure 'm^2'

  $ ./demo.exe <<- EOF
  > [<Measure>] type m;;
  > 7.77<m^3> + 7.73<m^3>;;
  (VFloatMeasure ((VFloat 15.5), ["m^3"]))

  $ ./demo.exe <<- EOF
  > [<Measure>] type m;;
  > [<Measure>] type m;;
  > [<Measure>] type m;;
  > [<Measure>] type m;;
  (VMeasureList [("m", ["m"])])

  $ ./demo.exe <<- EOF
  > [<Measure>] type m;;
  > [<Measure>] type sec;;
  > [<Measure>] type speed = m^2;;
  > [<Measure>] type speed = m * sec;;
  > [<Measure>] type speed = m / sec;;
  (VMeasureList [("speed", ["m"; "/"; "sec"]); ("sec", ["sec"]); ("m", ["m"])])

  $ ./demo.exe <<- EOF
  > [<Measure>] type m;;
  > [<Measure>] type sec;;
  > 7.77<m/sec> + 7.73<m/sec>;;
  (VFloatMeasure ((VFloat 15.5), ["m"; "/"; "sec"]))

  $ ./demo.exe <<- EOF
  > [<Measure>] type m;;
  > [<Measure>] type sec;;
  > [<Measure>] type sp = m^3 / sec^-1;;
  (VMeasureList
     [("sp", ["m^3"; "/"; "sec^-1"]); ("sec", ["sec"]); ("m", ["m"])])

  $ ./demo.exe <<- EOF
  > [<Measure>] type m;;
  > [<Measure>] type sec;;
  > [<Measure>] type sp = m / sec * dm;;
  Interpretation error. The type 'dm' is not defined

  $ ./demo.exe <<- EOF
  > [<Measure>] type m;;
  > [<Measure>] type sec;;
  > [<Measure>] type dm;;
  > [<Measure>] type speed = m / sec;;
  > [<Measure>] type sp = speed * dm;;
  > 7.<m / sec * dm> + 7.<sp>;;
  (VFloatMeasure ((VFloat 14.), ["m"; "/"; "sec"; "*"; "dm"]))

  $ ./demo.exe <<- EOF
  > [<Measure>] type m;;
  > [<Measure>] type sec;;
  > [<Measure>] type dm;;
  > [<Measure>] type speed = m / sec;;
  > [<Measure>] type sp = speed * dm;;
  > (fun x y -> x + y) 7.77<sp> 7.73<speed * dm>;;
  (VFloatMeasure ((VFloat 15.5), ["sp"]))

multiplication float + measure

  $ ./demo.exe <<- EOF
  > [<Measure>] type m;;
  > 7.77<m> * 2.<m>;;
  (VFloatMeasure ((VFloat 15.54), ["m^2"]))

  $ ./demo.exe <<- EOF
  > [<Measure>] type m;;
  > [<Measure>] type n;;
  > 7.75<m> * 2.<n>;;
  (VFloatMeasure ((VFloat 15.5), ["m"; "*"; "n"]))

  $ ./demo.exe <<- EOF
  > [<Measure>] type m;;
  > [<Measure>] type n;;
  > 7.77<m * n> * 2.<n * m>;;
  (VFloatMeasure ((VFloat 15.54), ["m^2"; "*"; "n^2"]))

  $ ./demo.exe <<- EOF
  > [<Measure>] type m;;
  > [<Measure>] type n;;
  > 7.77<m^2 / n> * 2.<n^2 / m>;;
  (VFloatMeasure ((VFloat 15.54), ["m^1"; "/"; "n^-1"]))

  $ ./demo.exe <<- EOF
  > [<Measure>] type m;;
  > [<Measure>] type n;;
  > [<Measure>] type sec;;
  > [<Measure>] type dm;;
  > [<Measure>] type speed;;
  > 7.77<m / n> * 2.<n / m>;;
  (VFloatMeasure ((VFloat 15.54), ["m^0"; "/"; "n^0"]))
