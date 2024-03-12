  $ ./demoInterpret.exe << EOF
  > let rec fac = fun n -> if n < 2 then 1 else n * fac (n-1)
  > let a = fac 5
  > EOF
  a: 120
  fac: <rec fun>

  $ ./demoInterpret.exe << EOF
  > let x = if 1+1=2 then 1 else 0
  > EOF
  x: 1

  $ ./demoInterpret.exe << EOF
  > let fff = (1, 2, 3)
  > EOF
  fff: (1, 2, 3)

  $ ./demoInterpret.exe << EOF
  > let mtch = fun x -> match x with | 1 -> "one" | 2 -> "two" | _ -> "other"
  > let a = mtch 1
  > EOF
  a: one
  mtch: <fun>

  $ ./demoInterpret.exe << EOF
  > let binop = fun (a, b, c) -> a + b / c
  > let res = binop(2, 6, 3)
  > EOF
  binop: <fun>
  res: 2
  
  $ ./demoInterpret.exe << EOF
  > let rec fix = fun f -> (fun x -> f (fix f) x)
  > let fac = fix (fun self -> (fun n -> if n <= 1 then 1 else n * self (n - 1)))
  > let a = fac 6
  > EOF
  a: 720
  fac: <fun>
  fix: <rec fun>
  