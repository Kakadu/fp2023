(** Copyright 2023-2024, Julia Kononova *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Csharp_lib.Parser
open Csharp_lib.Ast

(* PARSERS *)
let show_wrap form = function
  | Some x -> Format.printf "%a@\n" form x
  | _ -> Format.print_string "Parsing error\n"
;;

let print_pars ps pp str = show_wrap pp (parse_to_some ps str)

(* TESTS *)

(* statements tests*)

let test_decl = print_pars s_declaration pp_statement

let%expect_test _ =
  test_decl {|int    name = -(25 - 9) *  !7|};
  [%expect
    {|
      (Decl ((Var_Declaration ((TVar TInt), (Name "name"))),
         (Some (Bin_op (Asterisk,
                  (Un_op (Minus,
                     (Bin_op (Dash, (Exp_Const (VInt 25)), (Exp_Const (VInt 9)))))),
                  (Un_op (Not, (Exp_Const (VInt 7)))))))
         )) |}]
;;

let test_body = print_pars p_body pp_statement

let%expect_test _ =
  test_body
    {|{if (true)
      { a();
        if    
                (false)
          {
            e=    b;
            return; ;
          } else
            { 
              int         ppp = 7901-12+9;
            }
          }; ; ;         ;
          a(1+2 , cl)  ; ;   ;
          if   (1+ run())
                  {
                    foo(1);
                  } else if (k<h) {}
          return 999; ; ; ;; ;
    }|};
  [%expect
    {|
    (Body
       [(If ((Exp_Const (VBool true)),
           (Body
              [(Expr (Method_invoke ((Exp_Name (Name "a")), (Args []))));
                (If ((Exp_Const (VBool false)),
                   (Body
                      [(Expr
                          (Bin_op (Assign, (Exp_Name (Name "e")),
                             (Exp_Name (Name "b")))));
                        (Return None)]),
                   (Some (Body
                            [(Decl (
                                (Var_Declaration ((TVar TInt), (Name "ppp"))),
                                (Some (Bin_op (Plus,
                                         (Bin_op (Dash, (Exp_Const (VInt 7901)),
                                            (Exp_Const (VInt 12)))),
                                         (Exp_Const (VInt 9)))))
                                ))
                              ]))
                   ))
                ]),
           None));
         (Expr
            (Method_invoke ((Exp_Name (Name "a")),
               (Args
                  [(Bin_op (Plus, (Exp_Const (VInt 1)), (Exp_Const (VInt 2))));
                    (Exp_Name (Name "cl"))])
               )));
         (If (
            (Bin_op (Plus, (Exp_Const (VInt 1)),
               (Method_invoke ((Exp_Name (Name "run")), (Args []))))),
            (Body
               [(Expr
                   (Method_invoke ((Exp_Name (Name "foo")),
                      (Args [(Exp_Const (VInt 1))]))))
                 ]),
            (Some (If (
                     (Bin_op (Less, (Exp_Name (Name "k")), (Exp_Name (Name "h"))
                        )),
                     (Body []), None)))
            ));
         (Return (Some (Exp_Const (VInt 999))))]) |}]
;;

let test_fac = print_pars c_method pp_member

let%expect_test _ =
  test_fac
    {| 
  int Fac(int num) {
        if (num == 1) {
          return 1;
        }
        else 
        {
          return num * Fac(num - 1);
        }
      }|};
  [%expect
    {|
      (CMethod (
         { m_acc_modifier = None; m_poly_modifier = None; m_type = (TReturn TInt);
           m_name = (Name "Fac");
           m_params = (Params [(Var_Declaration ((TVar TInt), (Name "num")))]) },
         (Body
            [(If ((Bin_op (Equal, (Exp_Name (Name "num")), (Exp_Const (VInt 1)))),
                (Body [(Return (Some (Exp_Const (VInt 1))))]),
                (Some (Body
                         [(Return
                             (Some (Bin_op (Asterisk, (Exp_Name (Name "num")),
                                      (Method_invoke ((Exp_Name (Name "Fac")),
                                         (Args
                                            [(Bin_op (Dash,
                                                (Exp_Name (Name "num")),
                                                (Exp_Const (VInt 1))))
                                              ])
                                         ))
                                      ))))
                           ]))
                ))
              ])
         )) |}]
;;

(* class tests *)
let test_class = print_pars p_class pp_objects

let%expect_test _ =
  test_class
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
  [%expect
    {|
    (Class
       { cl_modifier = None; cl_name = (Name "Test"); cl_parent = None;
         cl_body =
         [(CField (
             { f_modifier = None; f_type = (TVar TInt); f_name = (Name "b") },
             (Some (Exp_Const (VInt 9)))));
           (CField (
              { f_modifier = None; f_type = (TVar TInt); f_name = (Name "c") },
              (Some (Bin_op (Asterisk, (Exp_Name (Name "b")),
                       (Exp_Const (VInt 67)))))
              ));
           (CField (
              { f_modifier = None; f_type = (TVar TInt); f_name = (Name "a") },
              (Some (Bin_op (Dash,
                       (Bin_op (Plus,
                          (Bin_op (Mod, (Exp_Const (VInt 50)),
                             (Exp_Const (VInt 2)))),
                          (Exp_Name (Name "b")))),
                       (Exp_Name (Name "c")))))
              ));
           (CField (
              { f_modifier = None; f_type = (TVar TBool); f_name = (Name "r") },
              (Some (Bin_op (Or,
                       (Bin_op (NotEqual, (Exp_Name (Name "a")),
                          (Bin_op (Asterisk, (Exp_Name (Name "b")),
                             (Exp_Name (Name "c"))))
                          )),
                       (Bin_op (And,
                          (Bin_op (MoreOrEqual, (Exp_Name (Name "a")),
                             (Exp_Name (Name "b")))),
                          (Bin_op (Equal, (Exp_Name (Name "a")),
                             (Bin_op (Plus, (Exp_Name (Name "c")),
                                (Exp_Const (VInt 90))))
                             ))
                          ))
                       )))
              ));
           (CField (
              { f_modifier = None; f_type = (TVar TString); f_name = (Name "s") },
              (Some (Exp_Const (VString "all right")))));
           (CField (
              { f_modifier = None; f_type = (TVar TChar); f_name = (Name "h") },
              (Some (Exp_Const (VChar 'a')))));
           (CMethod (
              { m_acc_modifier = None; m_poly_modifier = None; m_type = TVoid;
                m_name = (Name "M"); m_params = (Params []) },
              (Body
                 [(Expr
                     (Bin_op (Assign, (Exp_Name (Name "a")), (Exp_Const (VInt 5))
                        )));
                   (Expr
                      (Bin_op (Assign, (Exp_Name (Name "r")),
                         (Bin_op (And,
                            (Bin_op (NotEqual, (Exp_Name (Name "s")),
                               (Exp_Const (VString "kkkk")))),
                            (Bin_op (Equal,
                               (Bin_op (Mod, (Exp_Const (VInt 190)),
                                  (Exp_Const (VInt 22)))),
                               (Bin_op (Slash,
                                  (Bin_op (Asterisk, (Exp_Const (VInt 100)),
                                     (Un_op (Minus, (Exp_Const (VInt 2)))))),
                                  (Exp_Const (VInt 5))))
                               ))
                            ))
                         )))
                   ])
              ))
           ]
         }) |}]
;;

let%expect_test _ =
  test_class
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
  [%expect
    {|
    (Class
       { cl_modifier = None; cl_name = (Name "Test"); cl_parent = None;
         cl_body =
         [(CField (
             { f_modifier = None; f_type = (TVar TInt); f_name = (Name "n") },
             (Some (Exp_Const (VInt 10)))));
           (CField (
              { f_modifier = None; f_type = (TVar TInt);
                f_name = (Name "counter") },
              (Some (Bin_op (Asterisk,
                       (Bin_op (Mod, (Exp_Const (VInt 7)), (Exp_Const (VInt 2)))),
                       (Exp_Const (VInt 67)))))
              ));
           (CMethod (
              { m_acc_modifier = None; m_poly_modifier = (Some Static);
                m_type = (TReturn TInt); m_name = (Name "Main");
                m_params = (Params []) },
              (Body
                 [(For (
                     (Some (Decl ((Var_Declaration ((TVar TInt), (Name "i"))),
                              (Some (Exp_Const (VInt 0)))))),
                     (Some (Bin_op (Less, (Exp_Name (Name "i")),
                              (Exp_Name (Name "n"))))),
                     (Some (Bin_op (Assign, (Exp_Name (Name "i")),
                              (Bin_op (Plus, (Exp_Name (Name "i")),
                                 (Exp_Const (VInt 1))))
                              ))),
                     (Body
                        [(For (
                            (Some (Decl (
                                     (Var_Declaration ((TVar TInt), (Name "j"))),
                                     (Some (Exp_Const (VInt 1)))))),
                            None, None,
                            (Body
                               [(For (None,
                                   (Some (Bin_op (NotEqual,
                                            (Exp_Name (Name "j")),
                                            (Exp_Name (Name "n"))))),
                                   (Some (Bin_op (Assign, (Exp_Name (Name "j")),
                                            (Bin_op (Plus, (Exp_Name (Name "j")),
                                               (Exp_Const (VInt 2))))
                                            ))),
                                   (Body
                                      [(For (None, None, None,
                                          (Body
                                             [(Expr
                                                 (Bin_op (Assign,
                                                    (Exp_Name (Name "counter")),
                                                    (Bin_op (Plus,
                                                       (Bin_op (Plus,
                                                          (Exp_Name
                                                             (Name "counter")),
                                                          (Exp_Name (Name "i")))),
                                                       (Exp_Name (Name "j"))))
                                                    )))
                                               ])
                                          ))
                                        ])
                                   ))
                                 ])
                            ))
                          ])
                     ));
                   (Return (Some (Exp_Name (Name "counter"))))])
              ))
           ]
         }) |}]
;;

let%expect_test _ =
  test_class {| 
  class Test {
    string a = "5";
    int b = 9 + a;
  } |};
  [%expect
    {|
    (Class
       { cl_modifier = None; cl_name = (Name "Test"); cl_parent = None;
         cl_body =
         [(CField (
             { f_modifier = None; f_type = (TVar TString); f_name = (Name "a") },
             (Some (Exp_Const (VString "5")))));
           (CField (
              { f_modifier = None; f_type = (TVar TInt); f_name = (Name "b") },
              (Some (Bin_op (Plus, (Exp_Const (VInt 9)), (Exp_Name (Name "a")))))
              ))
           ]
         }) |}]
;;

let%expect_test _ =
  test_class
    {|
    class Test : Program {
    private int a = 9;
    public string s = "aaa";
    protected bool b = true;

    static int Main() {
      int counter = 0;
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
  }|};
  [%expect
    {|
      (Class
         { cl_modifier = None; cl_name = (Name "Test");
           cl_parent = (Some (Name "Program"));
           cl_body =
           [(CField (
               { f_modifier = (Some Private); f_type = (TVar TInt);
                 f_name = (Name "a") },
               (Some (Exp_Const (VInt 9)))));
             (CField (
                { f_modifier = (Some Public); f_type = (TVar TString);
                  f_name = (Name "s") },
                (Some (Exp_Const (VString "aaa")))));
             (CField (
                { f_modifier = (Some Protected); f_type = (TVar TBool);
                  f_name = (Name "b") },
                (Some (Exp_Const (VBool true)))));
             (CMethod (
                { m_acc_modifier = None; m_poly_modifier = (Some Static);
                  m_type = (TReturn TInt); m_name = (Name "Main");
                  m_params = (Params []) },
                (Body
                   [(Decl ((Var_Declaration ((TVar TInt), (Name "counter"))),
                       (Some (Exp_Const (VInt 0)))));
                     (While ((Exp_Const (VBool true)),
                        (Body
                           [(If (
                               (Bin_op (NotEqual, (Exp_Name (Name "counter")),
                                  (Exp_Const (VInt 2)))),
                               (Body
                                  [(Expr
                                      (Bin_op (Assign, (Exp_Name (Name "counter")),
                                         (Bin_op (Plus,
                                            (Exp_Name (Name "counter")),
                                            (Exp_Const (VInt 1))))
                                         )));
                                    (Expr
                                       (Bin_op (Assign, (Exp_Name (Name "b")),
                                          (Bin_op (And, (Exp_Name (Name "b")),
                                             (Exp_Const (VBool false))))
                                          )))
                                    ]),
                               (Some (If (
                                        (Bin_op (Equal, (Exp_Name (Name "b")),
                                           (Exp_Const (VBool false)))),
                                        (Body
                                           [(Return
                                               (Some (Un_op (Minus,
                                                        (Exp_Const (VInt 1))))))
                                             ]),
                                        (Some (Body
                                                 [(Return
                                                     (Some (Exp_Const (VInt 0))))
                                                   ]))
                                        )))
                               ))
                             ])
                        ))
                     ])
                ))
             ]
           }) |}]
;;

let%expect_test _ =
  test_class
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
    } |};
  [%expect
    {|
  (Class
     { cl_modifier = None; cl_name = (Name "Test"); cl_parent = None;
       cl_body =
       [(CMethod (
           { m_acc_modifier = None; m_poly_modifier = None;
             m_type = (TReturn TInt); m_name = (Name "Fac");
             m_params =
             (Params [(Var_Declaration ((TVar TInt), (Name "num")))]) },
           (Body
              [(If (
                  (Bin_op (Equal, (Exp_Name (Name "num")), (Exp_Const (VInt 1))
                     )),
                  (Body [(Return (Some (Exp_Const (VInt 1))))]),
                  (Some (Body
                           [(Return
                               (Some (Bin_op (Asterisk,
                                        (Exp_Name (Name "num")),
                                        (Method_invoke (
                                           (Exp_Name (Name "Fac")),
                                           (Args
                                              [(Bin_op (Dash,
                                                  (Exp_Name (Name "num")),
                                                  (Exp_Const (VInt 1))))
                                                ])
                                           ))
                                        ))))
                             ]))
                  ))
                ])
           ))
         ]
       }) |}]
;;

let%expect_test _ =
  test_class
    {| 
  class Test {
      A p = new A();
      int Sum(int num1, int num2) {
        int a = num1;
        int b = num2;
        int a = a + b;
        int b = a+6;
        A p = new A();
            return a;
         }    
    } |};
  [%expect
    {|
    (Class
       { cl_modifier = None; cl_name = (Name "Test"); cl_parent = None;
         cl_body =
         [(CField (
             { f_modifier = None; f_type = (TVar (TObj (Name "A")));
               f_name = (Name "p") },
             (Some (Un_op (New,
                      (Method_invoke ((Exp_Name (Name "A")), (Args []))))))
             ));
           (CMethod (
              { m_acc_modifier = None; m_poly_modifier = None;
                m_type = (TReturn TInt); m_name = (Name "Sum");
                m_params =
                (Params
                   [(Var_Declaration ((TVar TInt), (Name "num1")));
                     (Var_Declaration ((TVar TInt), (Name "num2")))])
                },
              (Body
                 [(Decl ((Var_Declaration ((TVar TInt), (Name "a"))),
                     (Some (Exp_Name (Name "num1")))));
                   (Decl ((Var_Declaration ((TVar TInt), (Name "b"))),
                      (Some (Exp_Name (Name "num2")))));
                   (Decl ((Var_Declaration ((TVar TInt), (Name "a"))),
                      (Some (Bin_op (Plus, (Exp_Name (Name "a")),
                               (Exp_Name (Name "b")))))
                      ));
                   (Decl ((Var_Declaration ((TVar TInt), (Name "b"))),
                      (Some (Bin_op (Plus, (Exp_Name (Name "a")),
                               (Exp_Const (VInt 6)))))
                      ));
                   (Decl (
                      (Var_Declaration ((TVar (TObj (Name "A"))), (Name "p"))),
                      (Some (Un_op (New,
                               (Method_invoke ((Exp_Name (Name "A")), (Args [])))
                               )))
                      ));
                   (Return (Some (Exp_Name (Name "a"))))])
              ))
           ]
         }) |}]
;;

(* ast tests*)
let test_ast = print_pars p_ast pp_program

let%expect_test _ =
  test_ast
    {| 
  class Dog {
    public string name = "Undefined";
    public int age;
    string breed;
    public void bark() {
      Console.WriteLine("Bark Bark !!");
    }
  }|};
  [%expect
    {|
      (Ast
         [(Class
             { cl_modifier = None; cl_name = (Name "Dog"); cl_parent = None;
               cl_body =
               [(CField (
                   { f_modifier = (Some Public); f_type = (TVar TString);
                     f_name = (Name "name") },
                   (Some (Exp_Const (VString "Undefined")))));
                 (CField (
                    { f_modifier = (Some Public); f_type = (TVar TInt);
                      f_name = (Name "age") },
                    None));
                 (CField (
                    { f_modifier = None; f_type = (TVar TString);
                      f_name = (Name "breed") },
                    None));
                 (CMethod (
                    { m_acc_modifier = (Some Public); m_poly_modifier = None;
                      m_type = TVoid; m_name = (Name "bark");
                      m_params = (Params []) },
                    (Body
                       [(Expr
                           (Method_invoke (
                              (Access_By_Point ((Exp_Name (Name "Console")),
                                 (Exp_Name (Name "WriteLine")))),
                              (Args [(Exp_Const (VString "Bark Bark !!"))]))))
                         ])
                    ))
                 ]
               })
           ]) |}]
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
  } |};
  [%expect
    {|
    (Ast
       [(Class
           { cl_modifier = None; cl_name = (Name "A"); cl_parent = None;
             cl_body =
             [(CMethod (
                 { m_acc_modifier = (Some Public);
                   m_poly_modifier = (Some Virtual); m_type = (TReturn TInt);
                   m_name = (Name "foo1"); m_params = (Params []) },
                 (Body [(Return (Some (Exp_Const (VInt 1))))])));
               (CMethod (
                  { m_acc_modifier = (Some Public);
                    m_poly_modifier = (Some Virtual); m_type = (TReturn TInt);
                    m_name = (Name "foo2"); m_params = (Params []) },
                  (Body [(Return (Some (Exp_Const (VInt 2))))])))
               ]
             });
         (Class
            { cl_modifier = None; cl_name = (Name "B");
              cl_parent = (Some (Name "A"));
              cl_body =
              [(CMethod (
                  { m_acc_modifier = (Some Public);
                    m_poly_modifier = (Some Override); m_type = (TReturn TInt);
                    m_name = (Name "foo1"); m_params = (Params []) },
                  (Body [(Return (Some (Exp_Const (VInt 666))))])));
                (CMethod (
                   { m_acc_modifier = (Some Public);
                     m_poly_modifier = (Some MNew); m_type = (TReturn TInt);
                     m_name = (Name "foo2"); m_params = (Params []) },
                   (Body [(Return (Some (Exp_Const (VInt 999))))])))
                ]
              })
         ]) |}]
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
  [%expect
    {|
    (Ast
       [(Class
           { cl_modifier = None; cl_name = (Name "A"); cl_parent = None;
             cl_body =
             [(CField (
                 { f_modifier = (Some Public); f_type = (TVar TInt);
                   f_name = (Name "a1") },
                 (Some (Exp_Const (VInt 9)))))
               ]
             });
         (Class
            { cl_modifier = None; cl_name = (Name "B"); cl_parent = None;
              cl_body =
              [(CField (
                  { f_modifier = (Some Public);
                    f_type = (TVar (TObj (Name "A"))); f_name = (Name "ab") },
                  (Some (Un_op (New,
                           (Method_invoke ((Exp_Name (Name "A")), (Args []))))))
                  ));
                (CField (
                   { f_modifier = (Some Public); f_type = (TVar TInt);
                     f_name = (Name "b") },
                   (Some (Access_By_Point ((Exp_Name (Name "ab")),
                            (Access_By_Point ((Exp_Name (Name "a1")),
                               (Exp_Name (Name "b"))))
                            )))
                   ))
                ]
              })
         ]) |}]
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
  [%expect
    {|
    (Ast
       [(Interface
           { i_modifier = None; i_name = (Name "I1"); i_parent = None;
             i_body =
             [(IMethod
                 { m_acc_modifier = None; m_poly_modifier = None; m_type = TVoid;
                   m_name = (Name "a"); m_params = (Params []) })
               ]
             });
         (Interface
            { i_modifier = None; i_name = (Name "I2");
              i_parent = (Some (Name "I1"));
              i_body =
              [(IMethod
                  { m_acc_modifier = None; m_poly_modifier = None;
                    m_type = TVoid; m_name = (Name "b"); m_params = (Params []) })
                ]
              });
         (Class
            { cl_modifier = None; cl_name = (Name "C");
              cl_parent = (Some (Name "I2"));
              cl_body =
              [(CMethod (
                  { m_acc_modifier = (Some Public); m_poly_modifier = None;
                    m_type = TVoid; m_name = (Name "a"); m_params = (Params []) },
                  (Body [])));
                (CMethod (
                   { m_acc_modifier = (Some Public); m_poly_modifier = None;
                     m_type = TVoid; m_name = (Name "b"); m_params = (Params [])
                     },
                   (Body [])))
                ]
              })
         ]) |}]
;;

let%expect_test _ =
  test_ast
    {|
  public interface Animal {
    void animalSound();
    void run();
  }
  class Pig: Animal {
    public string name = "Undefined";
    public int age;
    public int Sum(int a) {
      animalSound(a, b);
    };
    public Pig(int name) : base(name, age){};
    public void animalSound() {
      Console.WriteLine("The pig says: wee wee");
    }
  }
|};
  [%expect
    {|
    (Ast
       [(Interface
           { i_modifier = (Some Public); i_name = (Name "Animal");
             i_parent = None;
             i_body =
             [(IMethod
                 { m_acc_modifier = None; m_poly_modifier = None; m_type = TVoid;
                   m_name = (Name "animalSound"); m_params = (Params []) });
               (IMethod
                  { m_acc_modifier = None; m_poly_modifier = None;
                    m_type = TVoid; m_name = (Name "run"); m_params = (Params [])
                    })
               ]
             });
         (Class
            { cl_modifier = None; cl_name = (Name "Pig");
              cl_parent = (Some (Name "Animal"));
              cl_body =
              [(CField (
                  { f_modifier = (Some Public); f_type = (TVar TString);
                    f_name = (Name "name") },
                  (Some (Exp_Const (VString "Undefined")))));
                (CField (
                   { f_modifier = (Some Public); f_type = (TVar TInt);
                     f_name = (Name "age") },
                   None));
                (CMethod (
                   { m_acc_modifier = (Some Public); m_poly_modifier = None;
                     m_type = (TReturn TInt); m_name = (Name "Sum");
                     m_params =
                     (Params [(Var_Declaration ((TVar TInt), (Name "a")))]) },
                   (Body
                      [(Expr
                          (Method_invoke ((Exp_Name (Name "animalSound")),
                             (Args [(Exp_Name (Name "a")); (Exp_Name (Name "b"))])
                             )))
                        ])
                   ));
                (CConstructor (
                   { c_modifier = (Some Public); c_name = (Name "Pig");
                     c_params =
                     (Params [(Var_Declaration ((TVar TInt), (Name "name")))]);
                     c_base =
                     (Some (Args
                              [(Exp_Name (Name "name")); (Exp_Name (Name "age"))]))
                     },
                   (Body [])));
                (CMethod (
                   { m_acc_modifier = (Some Public); m_poly_modifier = None;
                     m_type = TVoid; m_name = (Name "animalSound");
                     m_params = (Params []) },
                   (Body
                      [(Expr
                          (Method_invoke (
                             (Access_By_Point ((Exp_Name (Name "Console")),
                                (Exp_Name (Name "WriteLine")))),
                             (Args
                                [(Exp_Const (VString "The pig says: wee wee"))])
                             )))
                        ])
                   ))
                ]
              })
         ]) |}]
;;

let%expect_test _ =
  test_ast "";
  [%expect {| (Ast []) |}]
;;
