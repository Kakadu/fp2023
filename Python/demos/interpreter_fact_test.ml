open Python_Lib.Interpreter
open Eval (Result)

let result_fact_of_10 =
  interpret
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
           , [ FunctionCall (Identifier "factorial", [ Const (Int 10) ]) ] ))
    ]
;;
