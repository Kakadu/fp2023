open Python_Lib.Interpreter
open Eval (Result)
open Python_Lib.Parser

let result_fact_of_7 =
  interpret
    [ Function
        ( Identifier "factorial"
        , [ Identifier "x" ]
        , [ IfElse
              ( BoolOp (Equal, Variable (Identifier "x"), Const (Int 1))
              , [ Return (Const (Int 1)) ]
              , [ Return
                    (ArithOp
                       ( Mul
                       , Variable (Identifier "x")
                       , FunctionCall
                           ( Identifier "factorial"
                           , [ ArithOp (Sub, Variable (Identifier "x"), Const (Int 1)) ]
                           ) ))
                ] )
          ] )
    ; Expression
        (FunctionCall
           ( Identifier "print"
           , [ FunctionCall (Identifier "factorial", [ Const (Int 10) ]) ] ))
    ]
;;
