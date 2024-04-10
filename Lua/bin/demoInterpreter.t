Факториал числа n
  $ ./demoInterpreter.exe <<EOF
  > k = 10
  > i = 1 
  > n = 1 
  > while i <= k do 
  >   n = n * i 
  >   i = i + 1 
  > end
  { vars =
    ["i": (Exp_number 11.),
     "k": (Exp_number 10.),
     "n": (Exp_number 3628800.),
     ];
    last_value = Exp_nil; is_loop = false; jump_stmt = Default }

Простой цикл while
  $ ./demoInterpreter.exe <<EOF
  > spb = 42
  > count = 0
  > while spb ~= 52 do
  >   spb = spb + 2
  >   count = count + 1
  > end
  { vars = ["count": (Exp_number 5.),
            "spb": (Exp_number 52.),
            ];
    last_value = Exp_nil; is_loop = false; jump_stmt = Default }

N-й член последовательности Фибоначчи
  $ ./demoInterpreter.exe <<EOF
  > fib1 = 0
  > fib2 = 1
  > fib_number = 10
  > while fib_number ~= 0 do
  >   fib2 = fib2 + fib1
  >   fib1 = fib2 - fib1
  >   fib_number = fib_number - 1
  > end
  { vars =
    ["fib1": (Exp_number 55.),
     "fib2": (Exp_number 89.),
     "fib_number": (Exp_number 0.),
     ];
    last_value = Exp_nil; is_loop = false; jump_stmt = Default }
