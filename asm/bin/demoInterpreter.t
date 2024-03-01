Поиск n-ого числа Фибоначчи, где n -- значение в rcx
  $ ./demoInterpreter.exe < ./sources/fib.asm
  (Directive (Section .text))
  (Directive (Global _start))
  (LabelDecl _start)
  (Instruction (Mov (Reg_64_imm_a (Rcx, (Imm_int 10)))))
  (Instruction (Mov (Reg_64_imm_a (Rax, (Imm_int 0)))))
  (Instruction (Mov (Reg_64_imm_a (Rbx, (Imm_int 1)))))
  (Instruction (Cmp (Reg_64_imm_a (Rcx, (Imm_int 1)))))
  (Instruction (Jle (Label exit)))
  (Instruction (Sub (Reg_64_imm_a (Rcx, (Imm_int 1)))))
  (LabelDecl fib_loop)
  (Instruction (Add (Reg_64_reg_64 (Rax, Rbx))))
  (Instruction (Xor (Reg_64_reg_64 (Rax, Rbx))))
  (Instruction (Xor (Reg_64_reg_64 (Rbx, Rax))))
  (Instruction (Xor (Reg_64_reg_64 (Rax, Rbx))))
  (Instruction (Sub (Reg_64_imm_a (Rcx, (Imm_int 1)))))
  (Instruction (Cmp (Reg_64_imm_a (Rcx, (Imm_int 0)))))
  (Instruction (Jne (Label fib_loop)))
  (LabelDecl exit)
  (Instruction (Mov (Reg_64_imm_a (Rdi, (Imm_int 0)))))
  (Instruction (Mov (Reg_64_imm_a (Rax, (Imm_int 60)))))
  (Instruction (Syscall (Nothing)))
  Registers:
  Rax: 60
  Rbp: 0
  Rbx: 55
  Rcx: 0
  Rdi: 0
  Rdx: 0
  Rsi: 0
  Rsp: 0
  XMM Registers:
  Xmm0: (0, 0)
  Xmm1: (0, 0)
  Xmm2: (0, 0)
  Xmm3: (0, 0)
  Xmm4: (0, 0)
  Xmm5: (0, 0)
  Xmm6: (0, 0)
  Xmm7: (0, 0)
  Stack: 
  Label to jump: None
  Zero flag: 1

Вычисление произведения двух векторов (1, 2) и (3, 4) в xmm0 и xmm1
Результат лежит в rdx
  $ ./demoInterpreter.exe < ./sources/scalar.asm
  (Directive (Global redefine_start_point))
  (Directive (Section .text))
  (LabelDecl redefine_start_point)
  (Instruction (Mov (Reg_64_imm_a (Rbx, (Imm_int 1)))))
  (Instruction (Mov (Reg_64_imm_a (Rcx, (Imm_int 2)))))
  (Instruction (Pinsrq (Reg_128_reg_64_imm_a (Xmm0, Rbx, (Imm_int 1)))))
  (Instruction (Pinsrq (Reg_128_reg_64_imm_a (Xmm0, Rcx, (Imm_int 0)))))
  (Instruction (Mov (Reg_64_imm_a (Rbx, (Imm_int 3)))))
  (Instruction (Mov (Reg_64_imm_a (Rcx, (Imm_int 4)))))
  (Instruction (Pinsrq (Reg_128_reg_64_imm_a (Xmm1, Rbx, (Imm_int 1)))))
  (Instruction (Pinsrq (Reg_128_reg_64_imm_a (Xmm1, Rcx, (Imm_int 0)))))
  (Instruction (Mulpd (Reg_128_reg_128 (Xmm0, Xmm1))))
  (Instruction (Movapd (Reg_128_reg_128 (Xmm2, Xmm0))))
  (Instruction (Punpckhqdq (Reg_128_reg_128 (Xmm2, Xmm2))))
  (Instruction (Addpd (Reg_128_reg_128 (Xmm0, Xmm2))))
  (Instruction (Movq (Reg_64_reg_128 (Rdx, Xmm0))))
  (LabelDecl exit)
  (Instruction (Mov (Reg_64_imm_a (Rdi, (Imm_int 0)))))
  (Instruction (Mov (Reg_64_imm_a (Rax, (Imm_int 60)))))
  (Instruction (Syscall (Nothing)))
  Registers:
  Rax: 60
  Rbp: 0
  Rbx: 3
  Rcx: 4
  Rdi: 0
  Rdx: 11
  Rsi: 0
  Rsp: 0
  XMM Registers:
  Xmm0: (6, 11)
  Xmm1: (3, 4)
  Xmm2: (3, 3)
  Xmm3: (0, 0)
  Xmm4: (0, 0)
  Xmm5: (0, 0)
  Xmm6: (0, 0)
  Xmm7: (0, 0)
  Stack: 
  Label to jump: None
  Zero flag: 0

Умножение матрицы (1, 2) - xmm0 на вектор (6) - в xmm2
|                 (3, 4) - xmm1           (7)
Результат - вектор (20) в xmm7
|                  (46)
  $ ./demoInterpreter.exe < ./sources/matrix.asm
  (Directive (Global _start))
  (Directive (Section .text))
  (LabelDecl _start)
  (Instruction (Mov (Reg_64_imm_a (Rbx, (Imm_int 1)))))
  (Instruction (Mov (Reg_64_imm_a (Rcx, (Imm_int 2)))))
  (Instruction (Pinsrq (Reg_128_reg_64_imm_a (Xmm0, Rbx, (Imm_int 1)))))
  (Instruction (Pinsrq (Reg_128_reg_64_imm_a (Xmm0, Rcx, (Imm_int 0)))))
  (Instruction (Mov (Reg_64_imm_a (Rbx, (Imm_int 3)))))
  (Instruction (Mov (Reg_64_imm_a (Rcx, (Imm_int 4)))))
  (Instruction (Pinsrq (Reg_128_reg_64_imm_a (Xmm1, Rbx, (Imm_int 1)))))
  (Instruction (Pinsrq (Reg_128_reg_64_imm_a (Xmm1, Rcx, (Imm_int 0)))))
  (Instruction (Mov (Reg_64_imm_a (Rbx, (Imm_int 6)))))
  (Instruction (Mov (Reg_64_imm_a (Rcx, (Imm_int 7)))))
  (Instruction (Pinsrq (Reg_128_reg_64_imm_a (Xmm2, Rbx, (Imm_int 1)))))
  (Instruction (Pinsrq (Reg_128_reg_64_imm_a (Xmm2, Rcx, (Imm_int 0)))))
  (Instruction (Mulpd (Reg_128_reg_128 (Xmm0, Xmm2))))
  (Instruction (Haddpd (Reg_128_reg_128 (Xmm0, Xmm0))))
  (Instruction (Mulpd (Reg_128_reg_128 (Xmm1, Xmm2))))
  (Instruction (Haddpd (Reg_128_reg_128 (Xmm1, Xmm1))))
  (Instruction (Movapd (Reg_128_reg_128 (Xmm7, Xmm0))))
  (Instruction (Punpckhqdq (Reg_128_reg_128 (Xmm7, Xmm1))))
  (LabelDecl exit)
  (Instruction (Mov (Reg_64_imm_a (Rdi, (Imm_int 0)))))
  (Instruction (Mov (Reg_64_imm_a (Rax, (Imm_int 60)))))
  (Instruction (Syscall (Nothing)))
  Registers:
  Rax: 60
  Rbp: 0
  Rbx: 6
  Rcx: 7
  Rdi: 0
  Rdx: 0
  Rsi: 0
  Rsp: 0
  XMM Registers:
  Xmm0: (20, 20)
  Xmm1: (46, 46)
  Xmm2: (6, 7)
  Xmm3: (0, 0)
  Xmm4: (0, 0)
  Xmm5: (0, 0)
  Xmm6: (0, 0)
  Xmm7: (20, 46)
  Stack: 
  Label to jump: None
  Zero flag: 0
