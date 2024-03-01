; Считает скалярное произведение двух векторов (1, 2) и (3, 4) в xmm0 и xmm1
; Результат кладет в rdx
global redefine_start_point
section .text
redefine_start_point:
    mov rbx, 1
    mov rcx, 2
    pinsrq xmm0, rbx, 1
    pinsrq xmm0, rcx, 0
    mov rbx, 3
    mov rcx, 4
    pinsrq xmm1, rbx, 1
    pinsrq xmm1, rcx, 0

    mulpd xmm0, xmm1
    movapd xmm2, xmm0
    punpckhqdq xmm2, xmm2
    addpd xmm0, xmm2
    movq rdx, xmm0
exit:
    mov rdi, 0 ; Код возврата
    mov rax, 60 ; syscall номер для exit
    syscall
