Copyright 2021-2023, Georgy Sichkar
SPDX-License-Identifier: CC0-1.0

  $ ./demoInterpret.exe << HATE_TESTS_AND_DEADLINES
  > class Program
  > {
  >    int Fac(int num)
  >    {  
  >        if (num == 1)
  >        {
  >            return 1;
  >        }
  >        else 
  >        {
  >            return num * Fac(num - 1);
  >        }
  >    }
  > 
  >    static int Main() 
  >     {
  >       int number;
  >       number = 5;
  >       return Fac(number);
  >     }
  > }
  > HATE_TESTS_AND_DEADLINES
  Result: (Init (Int_v 120))
