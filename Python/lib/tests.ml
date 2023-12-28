(** Copyright 2021-2023, Averin Pavel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Parser
open Interpreter
open Eval (Result)

(* Parser tests *)
let%test _ =
  parser "print(\"Hello World\")"
  = Ok
      [ Expression (FunctionCall (Identifier "print", [ Const (String "Hello World") ])) ]
;;

let%test _ = parse pyParser "1" = Ok [ Expression (Const (Int 1)) ]

let%test _ =
  parse pyParser "if y == 3:\n\tx = 0"
  = Ok
      [ IfElse
          ( BoolOp (Equal, Variable (Global, Identifier "y"), Const (Int 3))
          , [ Assign (Variable (Global, Identifier "x"), Const (Int 0)) ]
          , [] )
      ]
;;

let%test _ =
  parse pyParser "myFunction(x)"
  = Ok
      [ Expression
          (FunctionCall (Identifier "myFunction", [ Variable (Global, Identifier "x") ]))
      ]
;;

let%test _ =
  parse pyParser "def testFunction(x):\n    x = 1\n    return x + 1"
  = Ok
      [ Function
          ( Identifier "testFunction"
          , [ Identifier "x" ]
          , [ Assign (Variable (Global, Identifier "x"), Const (Int 1))
            ; Return (ArithOp (Add, Variable (Global, Identifier "x"), Const (Int 1)))
            ] )
      ]
;;

let%test _ =
  parse pyParser "while (y == 2):\n    x = 2"
  = Ok
      [ While
          ( BoolOp (Equal, Variable (Global, Identifier "y"), Const (Int 2))
          , [ Assign (Variable (Global, Identifier "x"), Const (Int 2)) ] )
      ]
;;

let%test _ =
  parse
    pyParser
    "\n\
     def factorial(x):\n\
    \    if (x == 1):\n\
    \        return 1\n\
    \    else:\n\
    \        return (x * factorial(x - 1))"
  = Ok
      [ Function
          ( Identifier "factorial"
          , [ Identifier "x" ]
          , [ IfElse
                ( BoolOp (Equal, Variable (Global, Identifier "x"), Const (Int 1))
                , [ Return (Const (Int 1)) ]
                , [ Return
                      (ArithOp
                         ( Mul
                         , Variable (Global, Identifier "x")
                         , FunctionCall
                             ( Identifier "factorial"
                             , [ ArithOp
                                   (Sub, Variable (Global, Identifier "x"), Const (Int 1))
                               ] ) ))
                  ] )
            ] )
      ]
;;

let%test _ =
  parse pyParser "(y == 2)"
  = Ok [ Expression (BoolOp (Equal, Variable (Global, Identifier "y"), Const (Int 2))) ]
;;

let%test _ =
  parse pyParser "(y >= 2)"
  = Ok
      [ Expression
          (BoolOp (GreaterOrEqual, Variable (Global, Identifier "y"), Const (Int 2)))
      ]
;;

let%test _ =
  parse pyParser "(y > 2)"
  = Ok [ Expression (BoolOp (Greater, Variable (Global, Identifier "y"), Const (Int 2))) ]
;;

let%test _ =
  parse pyParser "(y < 2)"
  = Ok [ Expression (BoolOp (Less, Variable (Global, Identifier "y"), Const (Int 2))) ]
;;

let%test _ =
  parse pyParser "(y <= 2)"
  = Ok
      [ Expression
          (BoolOp (LessOrEqual, Variable (Global, Identifier "y"), Const (Int 2)))
      ]
;;

let%test _ =
  parse pyParser "(y != 2)"
  = Ok
      [ Expression (BoolOp (NotEqual, Variable (Global, Identifier "y"), Const (Int 2))) ]
;;

let%test _ =
  parse pyParser "(x and y)"
  = Ok
      [ Expression
          (BoolOp
             (And, Variable (Global, Identifier "x"), Variable (Global, Identifier "y")))
      ]
;;

let%test _ =
  parse pyParser "(x or y)"
  = Ok
      [ Expression
          (BoolOp
             (Or, Variable (Global, Identifier "x"), Variable (Global, Identifier "y")))
      ]
;;

let%test _ =
  parse pyParser "\nif (x > 0):\n    y = 2\n else:\n    y = 1"
  = Ok
      [ IfElse
          ( BoolOp (Greater, Variable (Global, Identifier "x"), Const (Int 0))
          , [ Assign (Variable (Global, Identifier "y"), Const (Int 2)) ]
          , [ Assign (Variable (Global, Identifier "y"), Const (Int 1)) ] )
      ]
;;

let%test _ = true = is_banned "return"
let%test _ = true = is_digit '1'
let%test _ = true = is_sign '-'
let%test _ = parse p_integer "999" = Ok (Const (Int 999))
let%test _ = parse p_string "\"Hello\"" = Ok (Const (String "Hello"))
let%test _ = parse p_global_variable "a" = Ok (Variable (Global, Identifier "a"))

let%test _ =
  parse pyParser "for i in range(0, 3):\n    print(i)"
  = Ok
      [ For
          ( Variable (Global, Identifier "i")
          , [ Const (Int 0); Const (Int 3) ]
          , [ Expression
                (FunctionCall (Identifier "print", [ Variable (Global, Identifier "i") ]))
            ] )
      ]
;;

let%test _ =
  parse pyParser "while 1 == 1:\n    print(1)"
  = Ok
      [ While
          ( BoolOp (Equal, Const (Int 1), Const (Int 1))
          , [ Expression (FunctionCall (Identifier "print", [ Const (Int 1) ])) ] )
      ]
;;

let%test _ =
  parse
    pyParser
    "class MyClass:\n\
     \tdef method1(x):\n\
     \t\treturn 3\n\
     \tdef method2():\n\
     \t\treturn 2\n\
     object = MyClass()"
  = Ok
      [ Class
          ( Identifier "MyClass"
          , [ Function
                (Identifier "method1", [ Identifier "x" ], [ Return (Const (Int 3)) ])
            ; Function (Identifier "method2", [], [ Return (Const (Int 2)) ])
            ] )
      ; Assign (Variable (Global, Identifier "object"), Object (Identifier "MyClass", []))
      ]
;;

let%test _ =
  parse pyParser "someClass.someField(10, 20+10)"
  = Ok
      [ Expression
          (MethodCall
             ( Identifier "someClass"
             , Identifier "someField"
             , [ Const (Int 10); ArithOp (Add, Const (Int 20), Const (Int 10)) ] ))
      ]
;;

let%test _ =
  parse pyParser "someClass.someField = someValue"
  = Ok
      [ Assign
          ( Field (Identifier "someClass", Identifier "someField")
          , Variable (Global, Identifier "someValue") )
      ]
;;

let%test _ = parse p_identifiers "x,y" = Ok [ Identifier "x"; Identifier "y" ]

let%test _ =
  parse pyParser "lambda a,b: a*b"
  = Ok
      [ Expression
          (Lambda
             ( [ Identifier "a"; Identifier "b" ]
             , ArithOp
                 ( Mul
                 , Variable (Global, Identifier "a")
                 , Variable (Global, Identifier "b") ) ))
      ]
;;

let%test _ =
  parse p_interpolationStrElemEndingWithCurly "string{" = Ok (Str (String "string"))
;;

let%test _ = parse p_interpolationVarElem "var}" = Ok (Var (Identifier "var"))
let%test _ = parse p_interpolationStrElemLast "str\"" = Ok (Str (String "str"))

let%test _ =
  parse p_fString "f\"string{var}str\""
  = Ok (FString [ Str (String "string"); Var (Identifier "var"); Str (String "str") ])
;;

let unpacker (Ok x) = x
let env = global_env
let%test _ = Ok (Int 5) = interpret_exp (ArithOp (Add, Const (Int 4), Const (Int 1))) env

let env1 =
  get_env
    env
    [ Assign (Variable (Global, Identifier "x"), Const (Int 6156))
    ; Assign (Variable (Global, Identifier "x"), Const (Int 4))
    ]
;;

let%test _ =
  Ok (Int 6)
  = interpret_exp
      (ArithOp (Add, Const (Int 2), Variable (Global, Identifier "x")))
      (unpacker env1)
;;

let func_test_env1 =
  get_env
    env
    [ Function
        ( Identifier "myFunction"
        , [ Identifier "x" ]
        , [ Return (ArithOp (Add, Const (Int 2), Const (Int 3))) ] )
    ; Function
        ( Identifier "myFunction2"
        , [ Identifier "x" ]
        , [ Assign (Variable (Global, Identifier "x"), Const (Int 3)) ] )
    ]
;;

(* print_funcs (unpacker func_test_env1).functions *)

let%test _ =
  Ok (Int 5)
  = interpret_exp
      (FunctionCall (Identifier "myFunction", [ Const (Int 4) ]))
      (unpacker func_test_env1)
;;

let func_test_env2 =
  get_env
    env
    [ Function (Identifier "myFunction", [ Identifier "x" ], [ Return (Const (Int 2)) ])
    ; Function
        ( Identifier "myFunction2"
        , [ Identifier "x" ]
        , [ Assign (Variable (Global, Identifier "y"), Const (Int 2))
          ; Return (Const (Int 2))
          ] )
    ]
;;

(** print_funcs (unpacker func_test_env2).functions **)

let%test _ =
  Ok (Int 4)
  = interpret_exp
      (ArithOp
         ( Mul
         , FunctionCall (Identifier "myFunction", [ Const (Int 4) ])
         , FunctionCall (Identifier "myFunction2", [ Const (Int 4) ]) ))
      (unpacker func_test_env2)
;;

let func_test_env3 =
  get_env
    env
    [ Function (Identifier "myFunction", [ Identifier "x" ], [ Return (Const (Int 2)) ])
    ; Function
        ( Identifier "myFunction2"
        , [ Identifier "x" ]
        , [ Assign (Variable (Global, Identifier "y"), Const (Int 3))
          ; Return (Variable (Global, Identifier "y"))
          ] )
    ]
;;

let%test _ =
  Ok (Int 6)
  = interpret_exp
      (ArithOp
         ( Mul
         , FunctionCall (Identifier "myFunction", [ Const (Int 4) ])
         , FunctionCall (Identifier "myFunction2", [ Const (Int 4) ]) ))
      (unpacker func_test_env3)
;;

let func_test_env4 =
  get_env
    env
    [ Function (Identifier "myFunction", [ Identifier "x" ], [ Return (Const (Int 2)) ])
    ; Function
        ( Identifier "myFunction2"
        , [ Identifier "x" ]
        , [ Assign (Variable (Global, Identifier "y"), Const (Int 3))
          ; Return
              (ArithOp
                 ( Add
                 , Variable (Global, Identifier "y")
                 , Variable (Global, Identifier "x") ))
          ] )
    ]
;;

let%test _ =
  Ok (Int 9)
  = interpret_exp
      (ArithOp
         ( Add
         , FunctionCall (Identifier "myFunction", [ Const (Int 4) ])
         , FunctionCall (Identifier "myFunction2", [ Const (Int 4) ]) ))
      (unpacker func_test_env4)
;;

let func_test_env4 =
  get_env
    env
    [ Function (Identifier "myFunction", [ Identifier "x" ], [ Return (Const (Int 2)) ])
    ; Function
        ( Identifier "myFunction2"
        , [ Identifier "x" ]
        , [ Assign (Variable (Global, Identifier "y"), Const (Int 3))
          ; Return
              (ArithOp
                 ( Add
                 , Variable (Global, Identifier "y")
                 , Variable (Global, Identifier "x") ))
          ] )
    ]
;;

let%test _ =
  Ok (Int 9)
  = interpret_exp
      (ArithOp
         ( Add
         , FunctionCall (Identifier "myFunction", [ Const (Int 4) ])
         , FunctionCall (Identifier "myFunction2", [ Const (Int 4) ]) ))
      (unpacker func_test_env4)
;;

let fact_env1 =
  get_env
    env
    [ Function
        ( Identifier "factorial"
        , [ Identifier "x" ]
        , [ IfElse
              ( BoolOp (Equal, Variable (Global, Identifier "x"), Const (Int 1))
              , [ Return (Const (Int 1)) ]
              , [ Return
                    (ArithOp
                       ( Mul
                       , Variable (Global, Identifier "x")
                       , FunctionCall
                           ( Identifier "factorial"
                           , [ ArithOp
                                 (Sub, Variable (Global, Identifier "x"), Const (Int 1))
                             ] ) ))
                ] )
          ] )
    ]
;;

let fact_env2 =
  get_env
    env
    [ Function
        ( Identifier "factorial"
        , [ Identifier "x" ]
        , [ IfElse
              ( BoolOp (Equal, Variable (Global, Identifier "x"), Const (Int 1))
              , [ Return (Const (Int 1)) ]
              , [ Return
                    (ArithOp
                       ( Mul
                       , Variable (Global, Identifier "x")
                       , FunctionCall
                           ( Identifier "factorial"
                           , [ ArithOp
                                 (Sub, Variable (Global, Identifier "x"), Const (Int 1))
                             ] ) ))
                ] )
          ] )
    ]
;;

let%test _ =
  Ok (Int 87178291200)
  = interpret_exp
      (FunctionCall (Identifier "factorial", [ Const (Int 14) ]))
      (unpacker fact_env2)
;;

let fact_and_print =
  get_env
    global_env
    [ Function
        ( Identifier "factorial"
        , [ Identifier "x" ]
        , [ IfElse
              ( BoolOp (Equal, Variable (Global, Identifier "x"), Const (Int 1))
              , [ Return (Const (Int 1)) ]
              , [ Return
                    (ArithOp
                       ( Mul
                       , Variable (Global, Identifier "x")
                       , FunctionCall
                           ( Identifier "factorial"
                           , [ ArithOp
                                 (Sub, Variable (Global, Identifier "x"), Const (Int 1))
                             ] ) ))
                ] )
          ] )
    ; Expression
        (FunctionCall
           ( Identifier "print"
           , [ FunctionCall (Identifier "factorial", [ Const (Int 5) ]) ] ))
    ]
;;

let%test _ =
  Ok None
  = interpret_exp
      (FunctionCall (Identifier "print", [ Const (Int 6003) ]))
      (unpacker fact_and_print)
;;
