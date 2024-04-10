(** Copyright 2023-2024, Julia Kononova *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

  $ echo "Function type"; ./demoInterpreter.exe <<-EOF
  > class Test {
  >   int Fac(int num) {
  >     if (num == 1) {
  >       return "woow";
  >     }
  >  }
  >   static void Main() {
  >     return Fac(5);
  >   }
  > }
  > EOF
  Function type
  (Typecheck_error (Other "Returned type does not match the function type"))

  $ echo "Double declare one variable"; ./demoInterpreter.exe <<-EOF
  >  class Test {
  >   int a = 5;
  >   int b = 9;
  >   int a = 9;
  > } 
  > EOF
  Double declare one variable
  (Typecheck_error (Other "This variable is already declared"))

  $ echo "Main method"; ./demoInterpreter.exe <<-EOF
  > class A {
  >   static void Main() {}
  > }
  > class B {
  >     static void Main() {}
  > }
  > EOF
  Main method
  (Typecheck_error (Other "Main method already exists"))

  $ echo "Access by point"; ./demoInterpreter.exe <<-EOF
  > class A {
  >   private int a = 9;
  > } 
  > class B {
  >   public A ab = new A();
  >   public int b = ab.a;
  > }
  > EOF
  Access by point
  (Typecheck_error Access_error)

  $ echo "Interface inheritance"; ./demoInterpreter.exe <<-EOF
  > interface I {
  >   public void b();
  > }
  > class C : I {
  >   public int b(){
  >      return 1;
  >   }
  > } 
  > EOF
  Interface inheritance
  (Typecheck_error (Other "Class incorrectly inherited from an interface"))

  $ echo "Uninitialized value"; ./demoInterpreter.exe <<-EOF
  > class Test {
  >   static void Main() {
  >   int a;
  >   int b = a -1 + 4;
  >  }
  > }
  > EOF
  Uninitialized value
  (Interpret_error (Other "Value is not initialized"))

  $ echo "No main method"; ./demoInterpreter.exe <<-EOF
  > class Test {
  >   static void foo() {
  >   int b = 4;
  >  }
  > }
  > EOF
  No main method
  (Interpret_error (Other "Main method not found"))

  $ echo "Constructor typecheck"; ./demoInterpreter.exe <<-EOF
  > class Test {
  >   public void Test() {}
  >   Test t = new Test();
  > }
  > EOF
  Constructor typecheck
  (Typecheck_error (Other "Method invocation check error"))
