  $ ./demos.exe <<- EOF
  > 777;;
  (VInt 777)

  $ ./demos.exe <<- EOF
  > -777;;
  (VInt -777)

  $ ./demos.exe <<- EOF
  > 777.777;;
  (VFloat 777.777)

  $ ./demos.exe <<- EOF
  > -777.777;;
  (VFloat -777.777)

  $ ./demos.exe <<- EOF
  > "Test to string";;
  (VString "Test to string")

  $ ./demos.exe <<- EOF
  > true;;
  (VBool true)

  $ ./demos.exe <<- EOF
  > false;;
  (VBool false)

  $ ./demos.exe <<- EOF
  > [];;
  VNil

  $ ./demos.exe <<- EOF
  > ();;
  VUnit

  $ ./demos.exe <<- EOF
  > [7;7;7];;
  (VList [(VInt 7); (VInt 7); (VInt 7)])

  $ ./demos.exe <<- EOF
  > (777, -777, 7);;
  (VTuple [(VInt 777); (VInt -777); (VInt 7)])

  $ ./demos.exe <<- EOF
  > (24 / 8) * (6 - 3) * -5;;
  (VInt -45)

  $ ./demos.exe <<- EOF
  > 5 / 0;;
  Interpretation error. Attempted to divide by zero

  $ ./demos.exe <<- EOF
  > -7.77 + 7.73;;
  (VFloat -0.04)

  $ ./demos.exe <<- EOF
  > 7.77 / 0.0;;
  (VFloat infinity)

  $ ./demos.exe <<- EOF
  > ((fun z v -> z * v)4 )5;;
  (VInt 20)

  $ ./demos.exe <<- EOF
  > ((fun z v -> z / v)4 )5;;
  (VInt 0)

  $ ./demos.exe <<- EOF
  > 777 % 2;;
  (VInt 1)

  $ ./demos.exe <<- EOF
  > 777 < 666;;
  (VBool false)

  $ ./demos.exe <<- EOF
  > 777 > 666;;
  (VBool true)

  $ ./demos.exe <<- EOF
  > true && false;;
  (VBool false)

  $ ./demos.exe <<- EOF
  > true || false;;
  (VBool true)

  $ ./demos.exe <<- EOF
  > (1, 2, 3) = (1, 2, 3);;
  (VBool true)

  $ ./demos.exe <<- EOF
  > [1; 2; 3] <> [1; 2; 3];;
  (VBool false)

  $ ./demos.exe <<- EOF
  > let num x = 
  >  match x with 
  >    | 1 -> 1
  >    | _ -> 2;;
  > num 10;;
  (VInt 2)

  $ ./demos.exe <<- EOF
  > (fun x -> (fun y -> x * y) 5) 5;;
  (VInt 25)

  $ ./demos.exe <<- EOF
  > (((((fun a b c d e -> a + b + c + d + e) 1) 2) 3) 4) 5;;
  (VInt 15)

  $ ./demos.exe <<- EOF
  > if (2 + 3) then 1 else 2;;
  Interpretation error. Unreachable

  $ ./demos.exe <<- EOF
  > let num = 5;;
  (VInt 5)

  $ ./demos.exe <<- EOF
  > let sum = num + 5;;
  Interpretation error. The value or constructor 'num' is not defined

  $ ./demos.exe <<- EOF
  > let sum x = fun y -> x + y;;
  > sum (sum 7 70) 700;;
  (VInt 777)

  $ ./demos.exe <<- EOF
  > let x = 7.77;; 
  > x;;
  (VFloat 7.77)

  $ ./demos.exe <<- EOF
  > (fun z v -> z % v) 777 5;;
  (VInt 2)

  $ ./demos.exe <<- EOF
  > let rec fact n = if n = 1 then 1 else n * fact (n - 1);;
  > fact 6;;
  (VInt 720)

  $ ./demos.exe <<- EOF
  > let rec fib n = if n <= 1 then n else fib (n - 1) + fib (n - 2);;
  > fib 9;;
  (VInt 34)

Units of Measure

  $ ./demos.exe <<- EOF
  > [<Measure>] type m;;
  (VMeasureList ["m"])

  $ ./demos.exe <<- EOF
  > [<Measure>] type m^3;;
  Interpretation error. Unexpected infix operator in type definition

  $ ./demos.exe <<- EOF
  > [<Measure>] type m;;
  > 7.77<m^3> + 7.73<m^2>;;
  Interpretation error. The unit of measure 'm^3' does not match the unit of measure 'm^2'

  $ ./demos.exe <<- EOF
  > [<Measure>] type m;;
  > 7.77<m^3> + 7.73<m^3>;;
  (VFloatMeasure ((VFloat 15.5), ["m^3"]))

  $ ./demos.exe <<- EOF
  > [<Measure>] type m;;
  > [<Measure>] type m;;
  > [<Measure>] type m;;
  > [<Measure>] type m;;
  (VMeasureList ["m"])

  $ ./demos.exe <<- EOF
  > [<Measure>] type m;;
  > [<Measure>] type sec;;
  > [<Measure>] type speed = m^2;;
  > [<Measure>] type speed = m * sec;;
  > [<Measure>] type speed = m / sec;;
  (VMeasureList ["m"; "/"; "sec"])

  $ ./demos.exe <<- EOF
  > [<Measure>] type m;;
  > [<Measure>] type sec;;
  > 7.77<m * sec> + 7.73<m * sec>;;
  (VFloatMeasure ((VFloat 15.5), ["m"; "*"; "sec"]))

  $ ./demos.exe <<- EOF
  > [<Measure>] type m;;
  > [<Measure>] type sec;;
  > [<Measure>] type sp = m^3 / sec^-1;;
  (VMeasureList ["m^3"; "/"; "sec^-1"])

  $ ./demos.exe <<- EOF
  > [<Measure>] type m;;
  > [<Measure>] type sec;;
  > [<Measure>] type sp = m / sec * dm;;
  Interpretation error. The type 'dm' is not defined

  $ ./demos.exe <<- EOF
  > [<Measure>] type m;;
  > [<Measure>] type sec;;
  > [<Measure>] type dm;;
  > [<Measure>] type speed = m / sec;;
  > [<Measure>] type sp = speed * dm;;
  > 7.<m / sec * dm> + 7.<sp>;;
  (VFloatMeasure ((VFloat 14.), ["m"; "/"; "sec"; "*"; "dm"]))

  $ ./demos.exe <<- EOF
  > [<Measure>] type m;;
  > [<Measure>] type sec;;
  > [<Measure>] type dm;;
  > [<Measure>] type speed = m / sec;;
  > [<Measure>] type sp = speed * dm;;
  > 7.<m^2 / sec^2 * dm^2> + 7.<sp^2>;;
  (VFloatMeasure ((VFloat 14.), ["m^2"; "/"; "sec^2"; "*"; "dm^2"]))

  $ ./demos.exe <<- EOF
  > [<Measure>] type m;;
  > [<Measure>] type sec;;
  > [<Measure>] type dm;;
  > [<Measure>] type speed = m / sec;;
  > [<Measure>] type sp = speed * dm;;
  > (fun x y -> x + y) 7.77<sp> 7.73<speed * dm>;;
  (VFloatMeasure ((VFloat 15.5), ["sp"]))

multiplication float + measure

  $ ./demos.exe <<- EOF
  > [<Measure>] type m;;
  > 7.77<m> * 2.<m>;;
  (VFloatMeasure ((VFloat 15.54), ["m^2"]))

  $ ./demos.exe <<- EOF
  > [<Measure>] type m;;
  > [<Measure>] type n;;
  > 7.75<m> * 2.<n>;;
  (VFloatMeasure ((VFloat 15.5), ["m"; "*"; "n"]))

  $ ./demos.exe <<- EOF
  > [<Measure>] type m;;
  > [<Measure>] type n;;
  > 7.77<m * n> * 2.<n * m>;;
  (VFloatMeasure ((VFloat 15.54), ["m^2"; "*"; "n^2"]))

  $ ./demos.exe <<- EOF
  > [<Measure>] type m;;
  > [<Measure>] type n;;
  > 7.77<m^2 / n> * 2.<n^2 / m>;;
  (VFloatMeasure ((VFloat 15.54), ["m"; "*"; "n"]))

  $ ./demos.exe <<- EOF
  > [<Measure>] type m;;
  > 7.77<m>;;
  (VFloatMeasure ((VFloat 7.77), ["m"]))

  $ ./demos.exe <<- EOF
  > [<Measure>] type m;;
  > [<Measure>] type n;;
  > 7.77<n> * 7.78<m>
  (VFloatMeasure ((VFloat 60.4506), ["n"; "*"; "m"]))

  $ ./demos.exe <<- EOF
  > [<Measure>] type m;;
  > [<Measure>] type n;;
  > [<Measure>] type speed = m^3*n^-1;;
  (VMeasureList ["m^3"; "*"; "n^-1"])

  $ ./demos.exe <<- EOF
  > [<Measure>] type m;;
  > [<Measure>] type n;;
  > 7.77<m^3 / n^4> * 2.<n^5 / m^2>;;
  (VFloatMeasure ((VFloat 15.54), ["m"; "/"; "n^-1"]))

  $ ./demos.exe <<- EOF
  > [<Measure>] type m;;
  > [<Measure>] type n;;
  > 7.77<m / n> * 2.<n^2 / m^2>;;
  (VFloatMeasure ((VFloat 15.54), ["m^-1"; "*"; "n"]))

division float + measure

  $ ./demos.exe <<- EOF
  > [<Measure>] type m;;
  > [<Measure>] type sec;;
  > 7.77<m * sec> / 7.73<sec * m>;;
  (VFloat 1.00517464424)

  $ ./demos.exe <<- EOF
  > [<Measure>] type m;;
  > 7.77<m> / 7.73<m^-1>;;
  (VFloatMeasure ((VFloat 1.00517464424), ["m^2"]))

  $ ./demos.exe <<- EOF
  > [<Measure>] type m;;
  > [<Measure>] type n;;
  > 7.77<m^3 / n^4> / 2.<n^5 / m^2>;;
  (VFloatMeasure ((VFloat 3.885), ["m^5"; "/"; "n^9"]))

  $ ./demos.exe <<- EOF
  > [<Measure>] type m;;
  > [<Measure>] type n;;
  > 7.77<m / n> / 2.<n^2 / m^2>;;
  (VFloatMeasure ((VFloat 3.885), ["m^3"; "/"; "n^3"]))

  $ ./demos.exe <<- EOF
  > [<Measure>] type m;;
  > [<Measure>] type n;;
  > 7.77<m / n> && 2.<n^2 / m^2>;;
  Interpretation error. This type does not support this operator
