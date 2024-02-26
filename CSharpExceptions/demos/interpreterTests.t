Copyright 2021-2023, Georgy Sichkar
SPDX-License-Identifier: CC0-1.0

  $ echo "Test: Ordinary factorial" ; ./demoInterpret.exe << HATE_TESTS_AND_DEADLINES
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
  Test: Ordinary factorial
  Result: (Init (Int_v 120))

  $ echo "Test: Sum of a singly linked list" ; ./demoInterpret.exe << HATE_TESTS_AND_DEADLINES
  > class Vertex
  > {
  >   public Vertex(Vertex next_, int v){
  >     next = next_;
  >     value = v;
  >   }
  >   public int value;
  >   public Vertex next;
  > }
  > class Program{
  >   static int Main()
  >   {
  >     Vertex last = new Vertex(null, 3);
  >     Vertex middle = new Vertex(last, 2);
  >     Vertex first = new Vertex(middle, 1);
  > 
  >     Vertex i = first;
  >     int res = 0;
  > 
  >     while(i.next != null){
  >       res = res + i.value;
  >       i = i.next;
  >     }
  >     res = res + i.value;
  >     return res;
  >   }
  > }
  > HATE_TESTS_AND_DEADLINES
  Test: Sum of a singly linked list
  Result: (Init (Int_v 6))

  $ echo "Test: Passing by reference" ; ./demoInterpret.exe << HATE_TESTS_AND_DEADLINES
  > class MyClass
  > {
  >   public int value;
  > }
  > class Program{
  >   void f(MyClass cl){ cl.value = 2; }
  >   static int Main()
  >   {
  >     MyClass cl = new MyClass();
  >     cl.value = 1;
  >     f(cl);
  >     return cl.value;
  >   }
  > }
  > HATE_TESTS_AND_DEADLINES
  Test: Passing by reference
  Result: (Init (Int_v 2))

  $ echo "Test: Lazy || and && " ; ./demoInterpret.exe << HATE_TESTS_AND_DEADLINES
  > class Program
  >     {
  >       bool b;
  > 
  >       static int Main(){
  >         if (true || b){
  >           if (false && b){
  >             return -2;
  >          } else {
  >             return 31;
  >           }
  >         }
  >         return -1;
  >       }
  >     }
  > HATE_TESTS_AND_DEADLINES
  Test: Lazy || and && 
  Result: (Init (Int_v 31))
