(** Copyright 2023-2024, Julia Kononova *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Csharp_lib.Interpreter
open Csharp_lib.Monad
open Csharp_lib.State_type.St_Interpreter

let show_wrap str =
  match interpreter str with
  | Result.Ok x ->
    (match x with
     | Some x -> Format.printf "Result: '%a'" pp_vl x
     | None -> Format.print_string "Result void\n")
  | Result.Error er -> Format.printf "%a\n%!" pp_error er
;;

let%expect_test _ =
  show_wrap
    {| 
  class Test {
    int b = 9;
    int c = 67;
    int a;
    bool r = false;
    string s = "all right";
    char h = 'a';
    bool t;

    static int Main() {
      a = (50 % 2) + b - c;
      r = s != "kkkk" && (190%22 == 100 * -2/5);
      t = (a != b * c) || (a >= b) && (a == c +90);
      return a;
    }
  } |};
  [%expect {|
    Result: '(Init (IValue (VInt -58)))' |}]
;;

let%expect_test _ =
  show_wrap
    {| 
  class Test {
    int n = 10;
    static int Main() {
      int res = 0;
      for(int i = 0; i < n; i = i+1) {
        for(int j = 0; j < i; j = j+1) {
          res = res + i *j;
        }
      }
      return res;
    }
  } |};
  [%expect {|
    Result: '(Init (IValue (VInt 870)))' |}]
;;

let%expect_test _ =
  show_wrap
    {| 
  class Test {
    bool t;
    int a = 5;

    static int Main() {
      int b = 5;
      int c = 2;
      t = true;
      if (t) {
        if (t && false) {
          t = false;
          return 1;
        }
        else if( a == b) {
          a = c*67 + 7;
          return a;
      }
      }
      else {
        return 3;
      }
      return 0;
    }
  } |};
  [%expect {|
    Result: '(Init (IValue (VInt 141)))' |}]
;;

let%expect_test _ =
  show_wrap
    {| 
  class NumbersSum {
    int x = 189;
    int s = 0;
    static int Main() {
      while (x != 0) {
          s = s + x % 10;
          x = x/ 10;
      }
      return s;
    }
  } |};
  [%expect {|
    Result: '(Init (IValue (VInt 18)))' |}]
;;

let%expect_test _ =
  show_wrap
    {| 
  class Test {
    public int is_right_triangle(int a, int b, int c) {
      if ((a + b <= c) || (a + c <= b) || (b + c <= a)) {
          return 0;
      } else if ((a * a + b * b == c * c) || (a * a + c * c == b * b) || (b * b + c * c == a * a)) {
          return 1;
      } else {
          return 2;
      }
    }
    static int Main() {
      return is_right_triangle(3,4,5);
    }
  } |};
  [%expect {|
    Result: '(Init (IValue (VInt 1)))' |}]
;;

let%expect_test _ =
  show_wrap
    {| 
  class A {
    public int i;
    public A(int n) {
      i = n;
    }
  } 
  class B: A {
    public int j;
    public B(int n, int m) : base(n) {
      j = m + 9;
    }
  }
  class C {
    static int Main(){
      B b = new B(1,2);
      return b.j;
    }
  }
  |};
  [%expect {|
    Result: '(Init (IValue (VInt 11)))' |}]
;;

let%expect_test _ =
  show_wrap
    {|
    class Test {
      static int Main() {
        int a;
        int b = a -1 + 4;
        return b;
      }
    } |};
  [%expect {| (Interpret_error (Other "Value is not initialized")) |}]
;;

let%expect_test _ =
  show_wrap
    {| 
  class A {
    protected int i = 9;
  } 
  class B : A {
    protected int j = 7;
    public B() : base() {}
  }
  class C : B {
    public B b = new B();
    public int foo(int n) {
      return n +j +i;
    }
    public C() : base() {}
  }
  class Test {
     static int Main() {
      C test = new C();
      int res = test.foo(9);
      return res;
    }
  }
 |};
  [%expect {|
    Result: '(Init (IValue (VInt 25)))' |}]
;;
