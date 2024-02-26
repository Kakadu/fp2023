Copyright 2021-2023, Georgy Sichkar
SPDX-License-Identifier: CC0-1.0

  $  echo "Test: Attempt to write to a closed file" ; ./demoInterpret.exe << HATE_TESTS_AND_DEADLINES
  >    class Program
  >    {
  >      static void Main(){
  >        FileInfo fl = new FileInfo("f.txt");;
  >        fl.CloseFile();
  >        fl.AppendAllText("What???");
  >      }
  >     }
  > HATE_TESTS_AND_DEADLINES
  Test: Attempt to write to a closed file
  Error: (Interpret_error (System_error "Bad file descriptor"))

  $  echo "Test: Using an uninitialized variable" ; ./demoInterpret.exe << HATE_TESTS_AND_DEADLINES
  >    class Program
  >    {
  >      static void Main(){
  >        int a;
  >        int b = 1;
  >        int c = a + b;
  >      }
  >     }
  > HATE_TESTS_AND_DEADLINES
  Test: Using an uninitialized variable
  Error: (Interpret_error Uninitialized_variable)

  $  echo "Test: Type mismatch in operators" ; ./demoInterpret.exe << HATE_TESTS_AND_DEADLINES
  >    class Program
  >    {
  >      static void Main(){
  >        bool v = 1 == 2 != 3;
  >      }
  >     }
  > HATE_TESTS_AND_DEADLINES
  Test: Type mismatch in operators
  Error: (Type_check_error (Other "Types are not equal..."))

  $  echo "Test: The return value is of the wrong type" ; ./demoInterpret.exe << HATE_TESTS_AND_DEADLINES
  >    class Program
  >    {
  >      static void Main(){
  >        return 1;
  >      }
  >     }
  > HATE_TESTS_AND_DEADLINES
  Test: The return value is of the wrong type
  Error: (Type_check_error Type_mismatch)

  $  echo "Test: Method with return type without 'return' in the body" ; ./demoInterpret.exe << HATE_TESTS_AND_DEADLINES
  >    class Program
  >    {
  >      static int Main(){}
  >     }
  > HATE_TESTS_AND_DEADLINES
  Test: Method with return type without 'return' in the body
  Error: (Interpret_error
            (Runtime_error
               "Without return can be used only methods of 'Void' type"))

  $  echo "Test: Access to a private field" ; ./demoInterpret.exe << HATE_TESTS_AND_DEADLINES
  >   class MyClass
  >    {
  >     int value;
  >    }
  >   class Program
  >    {
  >      static void Main(){ 
  >         MyClass cl = new MyClass();
  >         cl.value = 1; 
  >       }
  >    }
  > HATE_TESTS_AND_DEADLINES
  Test: Access to a private field
  Error: (Type_check_error (Access "Attempt to get a private class member"))

  $  echo "Test: Double definition" ; ./demoInterpret.exe << HATE_TESTS_AND_DEADLINES
  >   class Program
  >    {
  >      static void Main(){ 
  >         int a;
  >         int a; 
  >       }
  >    }
  > HATE_TESTS_AND_DEADLINES
  Test: Double definition
  Error: (Type_check_error (Double_definition (Id "a")))

  $  echo "Test: Inconsistency between arguments and parameters" ; ./demoInterpret.exe << HATE_TESTS_AND_DEADLINES
  >   class Program
  >    {
  >      void f(){}
  >      static void Main(){ 
  >         f(1); 
  >       }
  >    }
  > HATE_TESTS_AND_DEADLINES
  Test: Inconsistency between arguments and parameters
  Error: (Type_check_error Type_mismatch)
