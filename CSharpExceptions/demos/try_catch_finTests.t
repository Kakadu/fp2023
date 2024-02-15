Copyright 2021-2023, Georgy Sichkar
SPDX-License-Identifier: CC0-1.0

  $  echo "Test: Checking the correct filter behavior" ; ./demoInterpret.exe << HATE_TESTS_AND_DEADLINES
  >   class A1 : Exception
  >    {
  >      public string msg;
  >      A1(string msg_){
  >        msg = msg_;
  >      }
  >    }
  > 
  >    class Program
  >    {
  >      static int Main(){
  >        FileInfo fl = new FileInfo("test_try_catch_fin_save.txt");
  >        int a = 0;
  >        try 
  >        {
  >          throw new A1("the win?");
  >        } 
  >        catch (A1 e) when (e.msg == "No no no, not now")
  >        {
  >          a = 1;
  >          fl.AppendAllText("sh*t228(");
  >        }
  >        catch (A1 e) when (e.msg == "the win?")
  >        {
  >          a = 2;
  >          fl.AppendAllText("Yeeee, I am the winner!");
  >        } finally {
  >          fl.CloseFile();
  >          a = a + 1000;
  >        }
  >        return a;
  >      }
  >     }
  > HATE_TESTS_AND_DEADLINES
  Test: Checking the correct filter behavior
  Result: (Init (Int_v 1002))
  $ cat test_try_catch_fin_save.txt
  Yeeee, I am the winner!

  $ echo "Test: Checking 'finally case' correct behavior 1.0" ; ./demoInterpret.exe << HATE_TESTS_AND_DEADLINES
  >   class A1 : Exception
  >    {
  >      public string msg;
  >      A1(string msg_){
  >        msg = msg_;
  >      }
  >    }
  > 
  >    class Program
  >    {
  >      static int Main(){
  >        FileInfo fl = new FileInfo("elusive_exception.txt");
  >        int a = 0;
  >        try 
  >        {
  >          throw new A1("I'll never get caught (*_-)");
  >        } 
  >        catch (A1 e) when (e.msg == "no")
  >        {
  >          a = 2;
  >          fl.AppendAllText("hm2");
  >        } finally {
  >          fl.AppendAllText("The End");
  >          fl.CloseFile();
  >        }
  >        return a;
  >      }
  >     }
  > HATE_TESTS_AND_DEADLINES
  Test: Checking 'finally case' correct behavior 1.0
  Error: (Interpret_error (User_exception (Code_ident (Id "A1"))))
  $ cat elusive_exception.txt
  The End

  $ echo "Test: Checking that 'finally case' always run 2.0 " ; ./demoInterpret.exe << HATE_TESTS_AND_DEADLINES
  >   class A1 : Exception
  >    {
  >      public string msg;
  >      A1(string msg_){
  >        msg = msg_;
  >      }
  >    }
  > 
  >    class Program
  >    {
  >      static int Main(){
  >        FileInfo fl = new FileInfo("throw_in_catch.txt");
  >        int a = 0;
  >        try 
  >        {
  >          throw new A1("One");
  >        } 
  >        catch (Exception)
  >        {
  >          a = 2;
  >          throw new Exception();
  >        } finally {
  >          fl.AppendAllText("Good");
  >          fl.CloseFile();
  >        }
  >        return a;
  >      }
  >     }
  > HATE_TESTS_AND_DEADLINES
  Test: Checking that 'finally case' always run 2.0 
  Error: (Interpret_error (User_exception (Code_ident (Id "Exception"))))
  $ cat throw_in_catch.txt
  Good


  $ echo "Test: Checking rudimentary inheritance" ; ./demoInterpret.exe << HATE_TESTS_AND_DEADLINES
  >   class A1 : Exception
  >    {
  >      public string msg;
  >      A1(string msg_){
  >        msg = msg_;
  >      }
  >    }
  > 
  >    class Program
  >    {
  >      static int Main(){
  >        FileInfo fl = new FileInfo("rudimental_exception.txt");
  >        int a = 0;
  >        try 
  >        {
  >          throw new A1("I'll never get caught (*_-)");
  >        } 
  >        catch (Exception)
  >        {
  >          a = 2;
  >          fl.AppendAllText("hm3 && ");
  >        } finally {
  >          fl.AppendAllText("The End");
  >          fl.CloseFile();
  >        }
  >        return a;
  >      }
  >     }
  > HATE_TESTS_AND_DEADLINES
  Test: Checking rudimentary inheritance
  Result: (Init (Int_v 2))
  $ cat rudimental_exception.txt
  hm3 && The End
