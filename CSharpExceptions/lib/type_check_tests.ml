(** Copyright 2021-2023, Georgy Sichkar *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Csharp_Exc_Lib.Errors
open Csharp_Exc_Lib.Parser
open Csharp_Exc_Lib.Type_check

(* ******************** TESTS ******************** *)
let type_check_wrap str =
  match parse_ast str with
  | Result.Error x -> Format.print_string ("Parsing error: " ^ x ^ "\n")
  | Result.Ok x ->
    (match type_check x with
     | _, Result.Error x -> Format.printf "Type_check error: %a@\n" pp_error x
     | _, Result.Ok _ -> Format.print_string "Type_check success\n")
;;

(* ******************** Positive ******************** *)

let%expect_test "Base factorial type check" =
  let s =
    {| 
      class Program
      {
          int Fac(int num)
          { 
              if (num == 1)
              {
                  return 1;
              }
              else 
              {
                  return num * Fac(num - 1);
              }
          }

          static int Main(){
              return Fac(100);
          }
      }
|}
  in
  type_check_wrap s;
  [%expect {| Type_check success |}]
;;

let%expect_test "Base program" =
  let s =
    {| 
      class Program
      {
          int Fac(int num)
          {  }

          static void Main(){
              return ;
          }
      }
|}
  in
  type_check_wrap s;
  [%expect {| Type_check success |}]
;;

let%expect_test "Base program 2" =
  let s =
    {| 
    class Program1
    {
      Program1(int a){}
    }

    class Program2 {}

    class Program
    {
        static void Main(){
            Program1 p1 = new Program1(1);
            Program2 p2 = new Program2();
            return ;
        }
    }
|}
  in
  type_check_wrap s;
  [%expect {| Type_check success |}]
;;

let%expect_test "Type checks" =
  let s =
    {| 
      class Program
      {
        int i = 23;
        char c = 'c';
        bool b;
        string s;
        
        int? i_n = null;
        char? c_n;
        bool? b_n = true;
        string s_n = null;

        int Fac(int num, string str, Program prg)
        { 
          i_n = i_n + i;
          b = i_n == i;
          s = "kek";
          b = !(1 == 2 != false && 1 + 2 % 3 == 1 / -2 * 100 || true);
          b_n = c_n == null;
          return Fac(i, s, null);

          
        }

        static void Main(){
            return;
        }
      }
|}
  in
  type_check_wrap s;
  [%expect {| Type_check success |}]
;;

let%expect_test "Loops & branches" =
  let s =
    {|     
    class Program
    {    
        static void Main()
        { 
          for (;;) {
            for(int a;;){
              for(int b; b <= 10;){
                for(;; b = b + 10){
                  break;
                }
                break;
              }
              break;
            }
          }

          while(true){
            while(1 == 1){
              break;
            }
            break;
          }

          if(2 == 3){
            bool? b = true;
            if (b == null)
              {} else 
              if ('d' == 's') {} else {}
          }
            return;
        }
    }
    
|}
  in
  type_check_wrap s;
  [%expect {| Type_check success |}]
;;

let%expect_test "Many classes" =
  let s =
    {|    
    class A
    {
      public int a = 12;

      public int f() {
        return 1;
      }
    }

    class B
    {
      public A bA = new A();

      public int res = bA.f();

      public void f(int c) {
        return;
      }
    }
    
    class Program
    {    
        static void Main()
        {
          B b1 = new B();
          int? res = b1.bA.a;
          res = null;
        }
    }
|}
  in
  type_check_wrap s;
  [%expect {| Type_check success |}]
;;

let%expect_test "Throw exceptions" =
  let s =
    {| 
      class E:Exception
      {}

      class Program
      {
          int Fac(int num)
          {  }

          static void Main(){
              throw new E();
              return ;
          }
      }
|}
  in
  type_check_wrap s;
  [%expect {| Type_check success |}]
;;

let%expect_test "Try_catch" =
  let s =
    {| 
      class E:Exception
      {
        public string msg;
      }

      class E1:Exception
      {
        public string msg;
      }

      class Program
      {
          int Fac(int num)
          {  }
          
          static void Main(){
              try{}
              catch(){}

              try{}
              finally{}

              try{}
              catch(){}
              finally{}

              try{}
              catch(E){}

              try{}
              catch(Exception e){}

              try{}
              catch(E) when(1 == 1){}

              try{}
              catch(E e) when(e.msg == "error") {}
              catch(E1 e) when(e.msg == "error1") {}
              catch(E1 w) {
                string s = w.msg;
              }
              finally{}
              
              return ;
          }
      }
|}
  in
  type_check_wrap s;
  [%expect {| Type_check success |}]
;;

(* ******************** Negative ******************** *)

let%expect_test "Double definition" =
  let s =
    {| 
      class Program
      {
          bool? b;
          int Fac(int num)
          { 
            bool b;
           }

          static void Main(){
              return ;
          }
      }
|}
  in
  type_check_wrap s;
  [%expect {| Type_check error: (Type_check_error (Double_definition (Id "b"))) |}]
;;

let%expect_test "Double \"Main\" definition " =
  let s =
    {| 
      class Program
      {
          int Fac(int num)
          {  }

          static void Main(){
              return ;
          }
      }

      class D{}

      class E{
        static void Main(){
              return ;
          }
      }
|}
  in
  type_check_wrap s;
  [%expect {| Type_check error: (Type_check_error (Double_definition (Id "Main"))) |}]
;;

let%expect_test "Double method declaration" =
  let s =
    {| 
      class Program
      {
          int Fac(int num)
          {  }

          int Fac(int num)
          {  }

          static void Main(){
              return ;
          }
      }
|}
  in
  type_check_wrap s;
  [%expect {| Type_check error: (Type_check_error (Double_definition (Id "Fac"))) |}]
;;

let%expect_test "Access check" =
  let s =
    {| 
      class Program
      {
          int Fac(int num)
          {  }

          static void Main(){
              return ;
          }
      }

      class D{}

      class E{
        static int My(){
          Program p = new Program();
              p.Fac(1);
              return 1;
          }
      }
|}
  in
  type_check_wrap s;
  [%expect
    {|
    Type_check error: (Type_check_error
                         (Access "Attempt to get a private class member")) |}]
;;

let%expect_test "Throw some class" =
  let s =
    {| 
      class C
      {}

      class Program
      {
          int Fac(int num)
          {  }

          static void Main(){
              throw new C();
              return ;
          }
      }
|}
  in
  type_check_wrap s;
  [%expect
    {|
    Type_check error: (Type_check_error
                         (Other "throw can be used only with exceptions")) |}]
;;

let%expect_test "Throw some class" =
  let s =
    {| 
    class Program
    {
      bool a = true;
    
      static int Main()
      {
        try
        {
        }
        catch
        {
          return 1;
        }
        finally
        {
          for (;;){
            return 1;
          }
        }
      }
    }
|}
  in
  type_check_wrap s;
  [%expect
    {|
    Type_check error: (Type_check_error
                         (Other
                            "Control cannot leave the body of a finally clause")) |}]
;;

let%expect_test "FileInfo_decl" =
  let s =
    {|
      class FileInfo{
        string path;
        bool Exists = false;

        FileInfo(string path_)
        {
          path = path_;
        }

        string ReadAllText () {}

        void AppendAllText(string info) {}

        } 
    |}
  in
  type_check_wrap s;
  [%expect {| Type_check error: (Type_check_error (Double_definition (Id "FileInfo"))) |}]
;;

let%expect_test "Exception_decl" =
  let s = {|
        class Exception{} 
    |} in
  type_check_wrap s;
  [%expect
    {| Type_check error: (Type_check_error (Double_definition (Id "Exception"))) |}]
;;
