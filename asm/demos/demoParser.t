  $ ./demoParser.exe <<- EOF
  > section .text
  > fibonachch:
  > push rbp
  > mov rbx, 0x2A
  > mov rax, 0x0
  > mov rcx, 1
  > cmp rbx, 1
  > je fibonachchEnd
  > cmp rbx, 2
  > je fibonachchTwo
  > sub rbx, 1
  > fibonachchStart:
  > sub rbx, 1
  > xor rax, rcx
  > xor rcx, rax
  > xor rax, rcx
  > add rax, rcx
  > cmp rbx, 0
  > je fibonachchEnd
  > jmp fibonachchStart
  > fibonachchTwo:
  > mov rax, 1
  > fibonachchEnd:
  > pop rbp
  > ret
  > EOF
  [(Directive (Section ".text")); (Label "fibonachch");
    (Instruction (InstrOperand1 (Push, (Reg (I64 Rbp)))));
    (Instruction (InstrOperand2 (Mov, (Reg (I64 Rbx)), (Imm (NumVal 42)))));
    (Instruction (InstrOperand2 (Mov, (Reg (I64 Rax)), (Imm (NumVal 0)))));
    (Instruction (InstrOperand2 (Mov, (Reg (I64 Rcx)), (Imm (NumVal 1)))));
    (Instruction (InstrOperand2 (Cmp, (Reg (I64 Rbx)), (Imm (NumVal 1)))));
    (Instruction (InstrOperand1 (Je, (LableRef "fibonachchEnd"))));
    (Instruction (InstrOperand2 (Cmp, (Reg (I64 Rbx)), (Imm (NumVal 2)))));
    (Instruction (InstrOperand1 (Je, (LableRef "fibonachchTwo"))));
    (Instruction (InstrOperand2 (Sub, (Reg (I64 Rbx)), (Imm (NumVal 1)))));
    (Label "fibonachchStart");
    (Instruction (InstrOperand2 (Sub, (Reg (I64 Rbx)), (Imm (NumVal 1)))));
    (Instruction (InstrOperand2 (Xor, (Reg (I64 Rax)), (Reg (I64 Rcx)))));
    (Instruction (InstrOperand2 (Xor, (Reg (I64 Rcx)), (Reg (I64 Rax)))));
    (Instruction (InstrOperand2 (Xor, (Reg (I64 Rax)), (Reg (I64 Rcx)))));
    (Instruction (InstrOperand2 (Add, (Reg (I64 Rax)), (Reg (I64 Rcx)))));
    (Instruction (InstrOperand2 (Cmp, (Reg (I64 Rbx)), (Imm (NumVal 0)))));
    (Instruction (InstrOperand1 (Je, (LableRef "fibonachchEnd"))));
    (Instruction (InstrOperand1 (Jmp, (LableRef "fibonachchStart"))));
    (Label "fibonachchTwo");
    (Instruction (InstrOperand2 (Mov, (Reg (I64 Rax)), (Imm (NumVal 1)))));
    (Label "fibonachchEnd");
    (Instruction (InstrOperand1 (Pop, (Reg (I64 Rbp)))));
    (Instruction (InstrOperand0 Ret))]


  $ ./demoParser.exe <<- EOF
  >  section .text
  >  global _start
  >  
  >  _start:
  >      ; Передача аргумента в регистр eax
  >      mov eax, 42
  >  
  >      ; Вызов функции для вычисления факториала
  >      push eax
  >      call factorial
  >      add esp, 4
  >  
  >      ; Вывод результата
  >      add eax, 0x30       ; Преобразование числа в ASCII
  >      mov [esp], eax      ; Сохранение результата в аргументе командной строки
  >  
  >      ; Выход из программы
  >      mov eax, 1          ; Системный вызов для выхода
  >      xor ebx, ebx        ; Код завершения 0
  >      syscall            ; Вызов системного вызова
  >  
  >  
  >  factorial:
  >      push ebp            ; Сохранение значения ebp
  >      mov ebp, esp        ; Установка текущего значения ebp
  >      sub esp, 4          ; Распределение места для локальной переменной
  >  
  >      mov eax, [ebp]      ; Загрузка аргумента из стека
  >      cmp eax, 1          ; Проверка, является ли число равным 1
  >      jle end_factorial   ; Если да, то перейти к концу функции
  >  
  >      dec eax             ; Уменьшение числа на 1
  >      push eax
  >      call factorial      ; Рекурсивный вызов функции для вычисления факториала
  >      add esp, 4
  >  
  >      mov ebx, [ebp]    ; Загрузка аргумента из стека
  >      imul eax, ebx       ; Умножение результата на аргумент
  >  
  >  end_factorial:
  >      mov esp, ebp        ; Восстановление значения esp
  >      pop ebp             ; Восстановление значения ebp
  >      ret                 ; Возврат из функции
  > EOF
  [(Directive (Section ".text")); (Directive (Global "_start"));
    (Label "_start");
    (Instruction (InstrOperand2 (Mov, (Reg (I32 Eax)), (Imm (NumVal 42)))));
    (Instruction (InstrOperand1 (Push, (Reg (I32 Eax)))));
    (Instruction (InstrOperand1 (Call, (LableRef "factorial"))));
    (Instruction (InstrOperand2 (Add, (Reg (I32 Esp)), (Imm (NumVal 4)))));
    (Instruction (InstrOperand2 (Add, (Reg (I32 Eax)), (Imm (NumVal 48)))));
    (Instruction
       (InstrOperand2 (Mov, (RegRef (Ref (I32 Esp))), (Reg (I32 Eax)))));
    (Instruction (InstrOperand2 (Mov, (Reg (I32 Eax)), (Imm (NumVal 1)))));
    (Instruction (InstrOperand2 (Xor, (Reg (I32 Ebx)), (Reg (I32 Ebx)))));
    (Instruction (InstrOperand0 Syscall)); (Label "factorial");
    (Instruction (InstrOperand1 (Push, (Reg (I32 Ebp)))));
    (Instruction (InstrOperand2 (Mov, (Reg (I32 Ebp)), (Reg (I32 Esp)))));
    (Instruction (InstrOperand2 (Sub, (Reg (I32 Esp)), (Imm (NumVal 4)))));
    (Instruction
       (InstrOperand2 (Mov, (Reg (I32 Eax)), (RegRef (Ref (I32 Ebp))))));
    (Instruction (InstrOperand2 (Cmp, (Reg (I32 Eax)), (Imm (NumVal 1)))));
    (Instruction (InstrOperand1 (Jle, (LableRef "end_factorial"))));
    (Instruction (InstrOperand1 (Dec, (Reg (I32 Eax)))));
    (Instruction (InstrOperand1 (Push, (Reg (I32 Eax)))));
    (Instruction (InstrOperand1 (Call, (LableRef "factorial"))));
    (Instruction (InstrOperand2 (Add, (Reg (I32 Esp)), (Imm (NumVal 4)))));
    (Instruction
       (InstrOperand2 (Mov, (Reg (I32 Ebx)), (RegRef (Ref (I32 Ebp))))));
    (Instruction (InstrOperand2 (IMul, (Reg (I32 Eax)), (Reg (I32 Ebx)))));
    (Label "end_factorial");
    (Instruction (InstrOperand2 (Mov, (Reg (I32 Esp)), (Reg (I32 Ebp)))));
    (Instruction (InstrOperand1 (Pop, (Reg (I32 Ebp)))));
    (Instruction (InstrOperand0 Ret))]
