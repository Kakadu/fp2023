(** Copyright 2023-2024, Julia Kononova *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

  $ echo "Factorial"; ./demoInterpreter.exe <<-EOF
  > class Test {
  >   static int Fac(int num) 
  >   {
  >      if (num == 1)
  >      {
  >         return 1;
  >      }
  >      else
  >      {
  >         return num * Fac(num - 1);
  >      }
  >   }
  >   static int Main() {
  >     return Fac(5);
  >   }
  > }
  > EOF
  Factorial
  Result: '(Init (IValue (VInt 120)))'

  $ echo "Loops and branches"; ./demoInterpreter.exe <<-EOF
  > class Point {
  >   public int x;
  >   public int y;
  > }
  > class Distance {
  >   public bool f(Point p1, Point p2) {
  >   return p1.x * p1.x + p1.y * p1.y <= p2.x * p2.x + p2.y * p2.y;
  >   }
  > }
  > class C {
  >   static int Main(){
  >     Point p1 = new Point();
  >     p1.x = 7;
  >     p1.y = 6;
  >     Point p2 = new Point();
  >     p2.x = 10;
  >     p2.y = 17;
  >     Distance d = new Distance();
  >     while (d.f(p1, p2)) {
  >       if(p1.x < 19) {
  >         p1.x = p1.x + 1;
  >       }
  >       else if (p1.y < 17) {
  >         p1.y = p1.y + 1;
  >       }
  >     }
  >     return p1.x + p1.y;
  >   }
  > }
  > EOF
  Loops and branches
  Result: '(Init (IValue (VInt 25)))'

  $ echo "Interface inheritance"; ./demoInterpreter.exe <<-EOF
  > interface I1 {
  >    int a();
  > }
  > interface I2 : I1 {
  >   int b();
  > }
  > class C : I2 {
  >   public int a() {
  >     return 5;
  >   }
  >   public int b() {
  >     return 2+3;
  >   }
  >   static int Main(){
  >     int n = b();
  >     for (int i = 0; i < 5; i = i+1) {
  >       n = n+i;
  >     }
  >     return n;
  >   }
  > }
  > EOF
  Interface inheritance
  Result: '(Init (IValue (VInt 15)))'

  $ echo "Access by point"; ./demoInterpreter.exe <<-EOF
  > class A {
  >   public int foo(int m) {
  >     return m;
  >   }
  > } 
  > class B {
  >   public int foo(int n) {
  >     return n-2;
  >   }
  >   public A ab = new A();
  > }
  > class C {
  >   public B b = new B();
  > }
  > class Test {
  >   static int Main() {
  >     C test = new C();
  >     int res = test.b.ab.foo(9);
  >     return res;
  >   }
  > }
  > EOF
  Access by point
  Result: '(Init (IValue (VInt 9)))'

  $ echo "Class inheritance"; ./demoInterpreter.exe <<-EOF
  > class A {
  >   protected int i = 100;
  >   public int h = 7000;
  > } 
  > class B : A {
  >   protected int j = 20;
  >   public virtual int foo(int n) {
  >     return n;
  >   }
  >   public B() : base() {}
  > }
  > class C : B{
  >   public B b = new B();
  >   public override int foo(int n) {
  >     return n + j +i + h;
  >   }
  >   public C() : base() {}
  > }
  > class Test {
  >   static int Main() {
  >     C test = new C();
  >     int res = test.foo(9);
  >     return res;
  >   }
  > }
  > EOF
  Class inheritance
  Result: '(Init (IValue (VInt 7129)))'
