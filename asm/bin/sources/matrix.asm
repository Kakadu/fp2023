; Умножение матрицы (1, 2) - xmm0 на вектор (6) - в xmm2
;                   (3, 4) - xmm1           (7)
; Результат - вектор (20) в xmm7
;                    (46)

global _start
section .text
_start:
    ; Подготовка матрицы A
    mov rbx, 1
    mov rcx, 2
    pinsrq xmm0, rbx, 1
    pinsrq xmm0, rcx, 0
    mov rbx, 3
    mov rcx, 4
    pinsrq xmm1, rbx, 1
    pinsrq xmm1, rcx, 0

    ; Подготовка вектора v
    mov rbx, 6
    mov rcx, 7
    pinsrq xmm2, rbx, 1  ; Загрузка 6 в xmm2
    pinsrq xmm2, rcx, 0  ; Загрузка 7 в xmm2

    ; Умножение первой строки матрицы A на вектор v
    mulpd xmm0, xmm2
    ; Горизонтальное сложение для получения результата умножения первой строки на вектор
    haddpd xmm0, xmm0

    ; Умножение второй строки матрицы A на вектор v
    mulpd xmm1, xmm2
    ; Горизонтальное сложение для получения результата умножения второй строки на вектор
    haddpd xmm1, xmm1

    ; Сохранение результатов в xmm7
    movapd xmm7, xmm0
    punpckhqdq xmm7, xmm1 

exit:
    mov rdi, 0 ; Код возврата
    mov rax, 60 ; syscall номер для exit
    syscall
