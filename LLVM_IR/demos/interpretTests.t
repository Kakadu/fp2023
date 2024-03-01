Copyright 2023-2024, Efremov Alexey 
SPDX-License-Identifier: CC0-1.0

  $ ./demoInterpret.exe < ./attachments/simple_ssa_fail.ll
  SSA check failed: 
  	Variable arg already was assigned

  $ ./demoInterpret.exe < ./attachments/fac.ll
  Programm return: 
  	 (CInteger (32, 120L))

  $ ./demoInterpret.exe 2000 20 4 < ./attachments/sum_args.ll
  Programm return: 
  	 (CInteger (32, 2024L))

  $ ./sum_args.elf 2000 20 4
  2024

  $ ./demoInterpret.exe 2000 -1000 30337 1 1 1 1 1 < ./attachments/sum_args.ll
  Programm return: 
  	 (CInteger (32, 31342L))

  $ ./sum_args.elf 2000 -1000 30337 1 1 1 1 1
  31342


  $ ./demoInterpret.exe  12.0 0.0   0. 0.   0. 12.  < ./attachments/triangle_square.ll
  Programm return: 
  	 (CFloat 72.)

  $ ./triangle_square.elf 12.0 0.0   0.0 0.0   0.0 12.0
  72.000000
  [10]

  $ ./demoInterpret.exe  0.012 0.    0. 0.   0. 0.012  < ./attachments/triangle_square.ll
  Programm return: 
  	 (CFloat 7.2e-05)

  $ ./triangle_square.elf  0.012 0.    0. 0.   0. 0.012
  0.000072
  [9]

  $ ./demoInterpret.exe 1 < ./attachments/fac_arg.ll 
  Programm return: 
  	 (CInteger (32, 1L))

  $ ./fac_arg.elf  1
  1

  $ ./demoInterpret.exe 2 < ./attachments/fac_arg.ll 
  Programm return: 
  	 (CInteger (32, 2L))

  $ ./fac_arg.elf  1
  1

  $ ./demoInterpret.exe 6 < ./attachments/fac_arg.ll 
  Programm return: 
  	 (CInteger (32, 720L))

  $ ./fac_arg.elf  6
  720

  $ ./demoInterpret.exe 10 < ./attachments/fac_arg.ll 
  Programm return: 
  	 (CInteger (32, 3628800L))

  $ ./fac_arg.elf  10
  3628800

  $ ./demoInterpret.exe 111 222 333 888 666 777 < ./attachments/vec_sum_args.ll
  Programm return: 
  	 (CVector
     [(CInteger (32, 999L)); (CInteger (32, 888L)); (CInteger (32, 1110L))])

  $ ./vec_sum_args.elf 111 222 333 888 666 777 
  999 888 1110 
  [14]

  $ ./demoInterpret.exe 1 2 3   4 5 6   7 8 9 < ./attachments/vec_sum_args.ll
  Programm return: 
  	 (CVector [(CInteger (32, 12L)); (CInteger (32, 15L)); (CInteger (32, 18L))])

  $ ./vec_sum_args.elf  1 2 3   4 5 6   7 8 9
  12 15 18 
  [10]

