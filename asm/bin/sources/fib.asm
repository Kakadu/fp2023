; Ищем n-ое число Фибоначчи, где n -- значение в rcx
section .text
global _start
_start:
   ; Инициализируем регистры для хранения предыдущих двух чисел Фибоначчи
   mov rcx, 10 ; Количество итераций
   mov rax, 0 ; F(0)
   mov rbx, 1 ; F(1)
   
   cmp rcx, 1
   jle exit ; Если n <= 1, результат уже в rax
   sub rcx, 1 ; Первое число Фибоначчи уже в rax, начинаем с F(2)
   
fib_loop:
   add rax, rbx ; F(n) = F(n-1) + F(n-2)
   ; Обмен значениями rax и rbx, используя xor swap trick
   xor rax, rbx
   xor rbx, rax
   xor rax, rbx
   ; Теперь rax содержит F(n-1), rbx содержит F(n)
   
   sub rcx, 1
   cmp rcx, 0
   jne fib_loop
   
exit:
   mov rdi, 0 ; Код возврата
   mov rax, 60 ; syscall номер для exit
   syscall
