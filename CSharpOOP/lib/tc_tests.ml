(** Copyright 2023-2024, Julia Kononova *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Csharp_lib.Type_checker
open Csharp_lib.Monad
open Csharp_lib.Parser
open Csharp_lib.Ast

let show_wrap = function
  | Some (Ast x) ->
    (match type_checker x with
     | _, Result.Ok _ -> Format.print_string "All right\n"
     | _, Result.Error er -> Format.printf "%a\n%!" pp_error er)
  | _ -> Format.print_string "Some error\n"
;;

let print_tc ps str = show_wrap (parse_to_some ps str)
let test_ast = print_tc p_ast

let%expect_test _ =
  test_ast
    {|
    class Test {
      int Fac(int num) {
        if (num == 1) {
          return 1;
        }
        else 
        {
          return num * Fac(num - 1);
        }
      }
      static int Main() {
        return Fac(5);
      }
    } |};
  [%expect {|
    All right |}]
;;

let%expect_test _ =
  test_ast
    {|
    class Test {
      int Fac(int num) {
        if (num == 1) {
          return "woow";
        }
      }
    } |};
  [%expect
    {|
    (Typecheck_error (Other "Returned type does not match the function type")) |}]
;;

let%expect_test _ =
  test_ast {| 
  class Test {
    int a = 5;
    int b = 9;
    int a = 9;
  } |};
  [%expect {|
    (Typecheck_error (Other "This variable is already declared")) |}]
;;

let%expect_test _ =
  test_ast
    {| 
  class Test {
    int b = 9;
    int c = b * 67;
    int a = (50 % 2) + b - c;
    bool r = (a != b * c) || (a >= b) && (a == c +90);
    string s = "all right";
    char h = 'a';

    void M() {
      a = 5;
      r = s != "kkkk" && (190%22 == 100 * -2/5);
    }
  } |};
  [%expect {|
    All right |}]
;;

let%expect_test _ =
  test_ast {| 
  class Test {
    string a = "5";
    int c = 9 + a;
  } |};
  [%expect {|
    (Typecheck_error Mismatch) |}]
;;

let%expect_test _ =
  test_ast
    {| 
  class Test {
    static int Main() {
      int counter = 0;
      bool b = true;
      while(true) {
        if (counter != 2) {
          counter = counter + 1;
          b = b && false;
        }
        else if (b == false){
          return -1;
        }
        else {
          return 0;
        }
      }
    }
  } |};
  [%expect {|
    All right |}]
;;

let%expect_test _ =
  test_ast
    {| 
  class Test {
    int n = 10;
    int counter = 7%2 *67;
    static int Main() {
      for (int i = 0; i < n; i=i+1) {
        for (int j = 1;;) {
          for (;j != n; j = j + 2) {
            for (;;) {
              counter = counter + i + j;
            }
          }
        }
      }
      return counter;
    }
  } |};
  [%expect {|
    All right |}]
;;

let%expect_test _ =
  test_ast
    {| 
  class A {
    static void Main() {}
  }
  class B {
    static void Main() {}
  } |};
  [%expect {|
    (Typecheck_error (Other "Main method already exists")) |}]
;;

let%expect_test _ =
  test_ast {| 
  class A {
    public virtual void Main() {}
  }
  |};
  [%expect
    {|
    (Typecheck_error
       (Other
          "Main must be a static method, have no params and return only int and void")) |}]
;;

let%expect_test _ =
  test_ast {| 
  class A {
    void Test() {}
    int a = 9;
    void Test() {}
  } |};
  [%expect {|
    (Typecheck_error (Other "This variable is already declared")) |}]
;;

let%expect_test _ =
  test_ast
    {| 
  class A {
    public int foo(int m) {
      return m;
    }
    public bool b = true;
  } 
  class B {
    public int foo(int n) {
      return n -1;
    }
    public A ab = new A();
  }
  class C {
    public A a = new A();
    public int r = a.foo(7);
    public B b = new B();
  }
  class Test {
     static int Main() {
      C test = new C();
      int res = test.b.ab.foo(9);
      int res2 = test.a.foo(9+10002);
      return res +res2;
    }
  }
 |};
  [%expect {|
    All right |}]
;;

let%expect_test _ =
  test_ast
    {| 
  class A {
    private int a = 9;
  } 
  class B {
    public A ab = new A();
    public int b = ab.a;
  }|};
  [%expect {|
    (Typecheck_error Access_error) |}]
;;

let%expect_test _ =
  test_ast
    {| 
  class A {
    public int a1 = 9;
  } 
  class B {
    public A ab = new A();
    public int b = ab.a;
  }|};
  [%expect {|
    (Typecheck_error (Other "Member not found")) |}]
;;

let%expect_test _ =
  test_ast
    {| 
  class A {
    public int a(int n, int m){
      return n+m;
    };
  } 
  class B {
    public A ab = new A();
    public int b = ab.a(5, 9*3);
  }|};
  [%expect {|
    All right |}]
;;

let%expect_test "Fucking shit" =
  test_ast
    {| 
  class A {
    public int b;
    public A(int n) {
      b = n;
    }
  } 
  class B: A {
    public int a;

    public B(int n, int m) : base(n) {
      a = m;
    }
  }|};
  [%expect {|
    All right |}]
;;

let%expect_test _ =
  test_ast
    {| 
  class A {
    public int a;
    public A(int n) {
      a = n;
    }
  } 
  class B: A {
    public int b;
    public B(int n, int m) {
      b = m;
    }
  }|};
  [%expect
    {|
    (Typecheck_error (Other "Class with a parent must have the base parameter")) |}]
;;

let%expect_test _ =
  test_ast
    {| 
  class A {
    public string a;
    public A(string n) {
      a = n;
    }
  } 
  class B: A {
    public int b;
    public B(int n, int m): base(n) {
      b = m;
    }
  }|};
  [%expect {|
    (Typecheck_error (Other "Method invocation check error")) |}]
;;

let%expect_test _ =
  test_ast
    {| 
  class A {
  } 
  class B {
    public int b;
    public B(int n, int m): base(n) {
      b = m;
    }
  }|};
  [%expect
    {|
    (Typecheck_error
       (Other "Class without a parent should not have a base parameter")) |}]
;;

let%expect_test _ =
  test_ast
    {| 
  class A {
    public int a(int n, int m){
      return n+m;
    };
  } 
  class B {
    public A ab = new A();
    public int b = ab.a("5", 9*3);
  }|};
  [%expect {|
    (Typecheck_error (Other "Method invocation check error")) |}]
;;

let%expect_test _ =
  test_ast {| 
  class A {
    public void a(int n, int m){
      return n+m;
    };
  }|};
  [%expect {|
    (Typecheck_error Mismatch) |}]
;;

let%expect_test _ =
  test_ast {| 
  class A {
    public void foo() {
      bool a = new A();
    };
  }|};
  [%expect {|
    (Typecheck_error Occurs_check) |}]
;;

let%expect_test _ =
  test_ast
    {| 
  class A {
    public int a1 = 9;
  } 
  class B {
    public A ab = new A();
    public int b = ab.a1.b;
  }|};
  [%expect {|
    (Typecheck_error (Other "Point access check error")) |}]
;;

let%expect_test _ =
  test_ast
    {| 
  class A {
    protected int a = 1;
  }
  class B : A {
    int b = a;
  }
 |};
  [%expect {|
    All right |}]
;;

let%expect_test _ =
  test_ast {| 
  class A {
    protected int a = 1;
  }
  class B {
    int b = a;
  }
 |};
  [%expect {|
    (Typecheck_error Occurs_check) |}]
;;

let%expect_test _ =
  test_ast
    {| 
  class A {
    private int a = 1;
  }
  class B : A {
    int b = a;
  }
 |};
  [%expect {|
    (Typecheck_error Occurs_check) |}]
;;

let%expect_test _ =
  test_ast {| 
  class A {
      int a = 1;
  }
  class B : A {
    int b = a;
  } |};
  [%expect {|
    (Typecheck_error Occurs_check) |}]
;;

let%expect_test _ =
  test_ast
    {| 
  class A {
    public virtual int foo1() {
      return 1;
    }
    public virtual int foo2() {
      return 2;
    }
  }
  class B : A {
    public override int foo1() {
      return 666;
    }
    public new int foo2() {
      return 999;
    }
  }
  class C : B {
    protected new int foo2(){
      return 0;
    }
  } |};
  [%expect {|
   All right |}]
;;

let%expect_test _ =
  test_ast
    {| 
  class A {
    public virtual int f() {
      return 1;
    }
  }
  class B : A {
    public virtual int f() {
      return 666;
    }
  } |};
  [%expect {|
    (Typecheck_error (Other "Problem with method modifiers")) |}]
;;

let%expect_test _ =
  test_ast
    {|
     interface I
     {
        void a();
        public void b();
     }
     class C : I
     {
        public void a(){
        }
        public void b(){
        }
     } |};
  [%expect {|
    All right |}]
;;

let%expect_test _ =
  test_ast
    {|
     interface I
     {
        void a();
        public void b();
     }
     class C : I
     {
        public void a(){
        }
        public int b(){
          return 1;
        }
        private void c(){
        }
     } |};
  [%expect
    {|
    (Typecheck_error (Other "Class incorrectly inherited from an interface")) |}]
;;

let%expect_test _ =
  test_ast
    {|
    interface I1 {
      void a();
    }
    interface I2 : I1 {
      void a();
      void b();
    }
    |};
  [%expect {|
    (Typecheck_error (Other "Inheritance error ")) |}]
;;

let%expect_test _ =
  test_ast
    {|
    class C {
      public void a(){};
    }
    interface I : C {
      void b();
    }
    |};
  [%expect
    {|
    (Typecheck_error (Other "Interface can only inherit from an interface ")) |}]
;;

let%expect_test _ =
  test_ast
    {|
    interface I1 {
      void a();
    }
    interface I2 : I1 {
      void b();
    }
    class C : I2 {
      public void a(){
      }
      public void b(){
      }
    } |};
  [%expect {|
    All right |}]
;;

let%expect_test _ =
  test_ast
    {|
    interface I1 {
      void a();
    }
    interface I2 : I1 {
      void b();
    }
    class C : I2 {
      public void b(){
      }
    } |};
  [%expect
    {|
    (Typecheck_error (Other "Class incorrectly inherited from an interface")) |}]
;;
