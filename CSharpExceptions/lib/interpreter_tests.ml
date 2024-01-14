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

let%expect_test "Lazy || and && " =
  let s =
    {| 
      class Program
      {
        bool b;

        static int Main(){
          if (true || b){
            if (false && b){
              return -2;
            } else {
              return 31;
            }
          }
          return -1;
        }
      }
    |}
  in
  interpret_wrap s;
  [%expect {| Result: (Init (Int_v 31)) |}]
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

let%expect_test "Base factorial type check" =
  let s =
    {| 
      class Program
      {
          int Fac(int num)
          { 
              if (num <= 1)
              {
                  return 1;
              }
              else 
              {
                  return num * Fac(num - 1);
              }
          }

          static int Main(){
              return Fac(4);
          }
      }
|}
  in
  interpret_wrap s;
  [%expect {| Result: (Init (Int_v 24)) |}]
;;

let%expect_test "Base factorial type check" =
  let s =
    {| 
      class Program
      {
          int Fac(int num)
          { 
              if (num <= 1)
              {
                  return 1;
              }
              else 
              {
                  return num * Fac(num - 1);
              }
          }

          static int Main(){
              return Fac(4);
          }
      }
|}
  in
  interpret_wrap s;
  [%expect {| Result: (Init (Int_v 24)) |}]
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
  [%expect {| Error: Using_an_uninitialized_variable |}]
;;
