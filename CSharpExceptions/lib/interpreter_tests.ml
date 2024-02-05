(** Copyright 2021-2023, Georgy Sichkar *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Csharp_Exc_Lib.Errors
open Csharp_Exc_Lib.Interpreter
open Csharp_Exc_Lib.Env_types.Eval_env

(* ******************** TESTS ******************** *)

let interpret_wrap str =
  match interpret str with
  | Result.Error x -> Format.printf "Error: %a@\n" pp_error x
  | Result.Ok x ->
    (match x with
     | None -> Format.print_string "Interpreter success\n"
     | Some x ->
       (match x with
        | IConst x -> Format.printf "Result: %a@\n" pp_t_env_eval_const x
        | _ -> Format.print_string "Interpreter error\n"))
;;

(* ******************** Positive ******************** *)

let%expect_test "Bin-un ops" =
  let s =
    {| 
      class Program
      {
        int i = 0;
        char c = 'c';
        bool b;
        string s;
        
        int? i_n = null;
        char? c_n = 'b';
        bool? b_n = true;
        string s_n = null;

        static int Main(){
          if (c == 'c' && c != c_n){ i = i + 1; }
          
          s = "value";
          if (s == "value" && s != s_n){ i = i + 1; }
          
          if (true) { i = i + 1; }
          
          b = false;
          if(!b) { i = i + 1; }
          
          if(s == null) {return -1;}
          else { i = i + 1; }
          
          int x;
          int y;
          int z;
          int p = 3;
          x = y = z = p;
          if(x == y && z == p && y == z){  i = i + 1; }

          i_n = 1;
          i_n = 30 + i_n;
          if (true && i_n == 31 && (i_n > 0) && (i_n >= 0) && i < 100 && i < 100 || b){
              return (-(1 + 4 * 2 / 2 - 1) + 4) + i ;
          }
          return -1;
        }
      }
    |}
  in
  interpret_wrap s;
  [%expect {| Result: (Init (Int_v 6)) |}]
;;

let%expect_test "Bin-un ops2" =
  let s =
    {| 
      class Program
      {
        char c = 'c';
        bool b;
        string s;
        
        int? i_n = null;
        char? c_n = 'b';
        bool? b_n = true;
        string s_n = null;

        static int Main(){
          int i = 10;
          if (c == 'c' && c != c_n){ i = i + 1; }
          
          s = "value";
          if (s == "value" && s != s_n){ i = i + 1; }
          
          if (true) { i = i + 1; }
          
          b = false;
          if(!b) { i = i + 1; }
          
          if(s == null) {return -1;}
          else { i = i + 1; }
          
          int x;
          int y;
          int z;
          int p = 3;
          x = y = z = p;
          if(x == y && z == p && y == z){  i = i + 1; }

          i_n = 1;
          i_n = 30 + i_n;
          if (true && i_n == 31 && (i_n > 0) && (i_n >= 0) && i < 100 && i < 100 || b){
              return (-(1 + 4 * 2 / 2 - 1) + 4) + i ;
          }
          return -1;
        }
      }
    |}
  in
  interpret_wrap s;
  [%expect {| Result: (Init (Int_v 16)) |}]
;;

let%expect_test "Scope" =
  let s =
    {| 
      class Program
      {
        static int Main(){
          int d = 0; 
          if (true){
            d = d + 5;
            if (false){
              return -2;
            } else {
              d = d + 5;
            }
          }
          return d;
        }
      }
    |}
  in
  interpret_wrap s;
  [%expect {| Result: (Init (Int_v 10)) |}]
;;

let%expect_test "Multi-class programm (instance) " =
  let s =
    {| 
      class A1
      {
        public int a1;

        public int get_plus(int p){
          return a1 + p;
        }
      }

      class Program
      {
        static int Main(){
          A1 hm = new A1();
          hm.a1 = 1;
          return hm.get_plus(1);
        }
      }
    |}
  in
  interpret_wrap s;
  [%expect {| Result: (Init (Int_v 2)) |}]
;;

let%expect_test "Multi-class programm (constr) " =
  let s =
    {| 
      class A1
      {
        public int a1;

        int a1_ = 2 - 2;
        
        A1 (int v){
          a1_ = v;
        }

        public int get_plus(int p){
          return a1 + p + a1_;
        }
      }

      class Program
      {
        static int Main(){
          A1 hm = new A1(-100);
          hm.a1 = 1;
          return hm.get_plus(1);
        }
      }
    |}
  in
  interpret_wrap s;
  [%expect {| Result: (Init (Int_v -98)) |}]
;;

let%expect_test "Multi-class programm (referential behavior) " =
  let s =
    {| 
      class A1
      {
        public int a1;
      }

      class Program
      {
        void change_class (A1 info)
        {
          info.a1 = 2;
        }

        static int Main(){
          A1 hm1 = new A1();
          hm1.a1 = 1;
          change_class(hm1);
          return hm1.a1;
        }
      }
    |}
  in
  interpret_wrap s;
  [%expect {| Result: (Init (Int_v 2)) |}]
;;

let%expect_test "Multi-class programm (complex constructor) " =
  let s =
    {| 
      class A1
      {
        public int a1;
      }

      class A2
      {
        public A1 f = new A1();
        public A1 s = new A1();
      }

      class Program
      {
        static int Main(){
          A2 hm = new A2();
          hm.f.a1 = 3;
          hm.s.a1 = 2;
          return hm.f.a1 + hm.s.a1;
        }
      }
    |}
  in
  interpret_wrap s;
  [%expect {| Result: (Init (Int_v 5)) |}]
;;

let%expect_test "Try_fin" =
  let s =
    {| 
      class A1 : Exception
      {
        public int flag;
      }

      class Program
      {
        static int Main(){
          int a = 0;
            try
            {
              a = 10;
              throw new A1();
              a = 11;
            } finally {
              a = 100;
            }
          return a;
        }
      }
    |}
  in
  interpret_wrap s;
  [%expect {| Result: (Init (Int_v 100)) |}]
;;

let%expect_test "Try_catch_fin" =
  let s =
    {| 
      class A1 : Exception
      {
        public int flag;
        A1(int flag1){
          flag = flag1;
        }
      }

      class Program
      {
        static int Main(){
          int a = 0;
            try
            {
              a = 10;
              throw new A1(1);
              a = 11;
            } catch(A1 e){
              a = a + 1;
            } 
            finally {
              a = a + 100;
            }
          return a;
        }
      }
    |}
  in
  interpret_wrap s;
  [%expect {| Result: (Init (Int_v 101)) |}]
;;

let%expect_test "Try_catch_fin + filter" =
  let s =
    {| 
      class A1 : Exception
      {
        public int flag;
        A1(int flag1){
          flag = flag1;
        }
      }

      class Program
      {
        static int Main(){
          int a = 0;
            try
            {
              a = 10;
              A1 exn = new A1(1);
              throw exn;
              a = 11;
            } catch(A1 e) when (e.flag == 1){
              a = a + 1;
            } 
            finally {
              a = a + 100;
            }
          return a;
        }
      }
    |}
  in
  interpret_wrap s;
  [%expect {| Result: (Init (Int_v 101)) |}]
;;

let%expect_test "Try_catch_fin + filter + many catch" =
  let s =
    {| 
      class A1 : Exception
      {
        public int flag;
        A1(int flag1){
          flag = flag1;
        }
      }

      class A2 : Exception
      {
        public string msg;
        A2(string info){
          msg = info;
        }
      }

      class Program
      {
        static int Main(){
          int a = 0;
            try
            {
              a = 1111111;
              A2 exn1 = new A2("error");
              throw exn1;
            } catch(A1 e) when (e.flag == 1){
              a = a + 1;
            } catch(A2 e) when (e.msg == "error_no"){
              a = a + 2;
            } catch(A2 e) when (e.msg == "error"){
              a = a + 3;
            } 
            finally {
              a = a + 100;
            }
          return a;
        }
      }
    |}
  in
  interpret_wrap s;
  [%expect {| Result: (Init (Int_v 103)) |}]
;;

let%expect_test "Nested try_catch_fin " =
  let s =
    {| 
      class A1 : Exception
      {
        public int flag;
        A1(int flag1){
          flag = flag1;
        }
      }

      class Program
      {
        static int Main(){
          int a = 0;
            try
            {
              a = 10;

              try{
                throw new A1(2);
              } catch(Exception) {
                throw new A1(1);
              } finally {
                a = -999;
              }

            } catch(A1 e){
              a = a + 1;
            } 
            finally {
              a = a + 100;
            }
          return a;
        }
      }
    |}
  in
  interpret_wrap s;
  [%expect {|
    Result: (Init (Int_v 101)) |}]
;;

let%expect_test "While" =
  let s =
    {| 
      class Program
      {
        static int Main(){
          int d = 0; 
          while(d < 100){
            d = d + 1;
            if(d == 15){
              break;
            }
          }
          return d;
        }
      }
    |}
  in
  interpret_wrap s;
  [%expect {| Result: (Init (Int_v 15)) |}]
;;

let%expect_test "For" =
  let s =
    {| 
      class Program
      {
        static int Main(){
          int d = 0; 
          int i1;
          int i2;

          for(int i = 0; i < 10; i = i + 1){
            d = d + 1;
          }

          for (;;){
            d = d + 10;
            break;
          }

          for (i1 = 0; i1 < 10;){
            d = d + 1;
            i1 = i1 + 1;
          }

          i2 = 0;
          for (; i2 < 10; i2 = i2 + 1){
            d = d + 1;
          }
          return d;
        }
      }
    |}
  in
  interpret_wrap s;
  [%expect {| Result: (Init (Int_v 40)) |}]
;;

let%expect_test "For + while" =
  let s =
    {| 
      class Program
      {
        static int Main(){
          int d = 0;
          while (true){
            for (int i = 0; i < 10; i = i + 1){
              d = d + 1;
              if (d == 5){
                break;
              }
            }
            if (5 < d && d <= 20){
              break;
            }
          }
          return d;
        }
      }
    |}
  in
  interpret_wrap s;
  [%expect {| Result: (Init (Int_v 15)) |}]
;;

let%expect_test "While + try" =
  let s =
    {| 
      class A1 : Exception {}
      class Program
      {
        static int Main(){
          int d = 0;
          try{
            while (true){
              d = d + 1;
              throw new Exception();
              if (5 < d && d <= 20){
              break;
              }
            } 
          } finally {d = 1;}
          return d;
        }
      }
    |}
  in
  interpret_wrap s;
  [%expect {| Result: (Init (Int_v 1)) |}]
;;

let%expect_test "Run void_method" =
  let s =
    {| 
      class A1 : Exception {}
      class Program
      {
        int justForFun(){return 1;}

        static int Main(){
          return justForFun();
        }
      }
    |}
  in
  interpret_wrap s;
  [%expect {| Result: (Init (Int_v 1)) |}]
;;

let%expect_test "Just save information" =
  let s =
    {| 
      class Program
      {
        static void Main(){
          FileInfo fl = new FileInfo("../../../../my.txt");
          fl.AppendAllText("I fucked it");
          fl.CloseFile();
          return;
        }
      }
    |}
  in
  interpret_wrap s;
  [%expect {| Interpreter success |}]
;;

(* ******************** Negative ******************** *)

let%expect_test "Uninit value " =
  let s =
    {| 
      class Program
      {
        bool b;

        static int Main(){
          if (b){}
          return -1;
        }
      }
    |}
  in
  interpret_wrap s;
  [%expect {| Error: (Interpret_error Uninitialized_variable) |}]
;;
