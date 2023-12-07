open Ast
open Parser
open Interpreter
open Eval (Result)

let unpacker = function
  | Ok x -> x
  | _ -> failwith "Failed to unpack"
;;

let env = global_env
let%test _ = Ok (Int 5) = interpret_exp (ArithOp (Add, Const (Int 4), Const (Int 1))) env

let env1 =
  get_env
    env
    [ Assign (Variable (Identifier "x"), Const (Int 6156))
    ; Assign (Variable (Identifier "x"), Const (Int 4))
    ]
;;

let%test _ =
  Ok (Int 6)
  = interpret_exp
      (ArithOp (Add, Const (Int 2), Variable (Identifier "x")))
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
        , [ Assign (Variable (Identifier "x"), Const (Int 3)) ] )
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
        , [ Assign (Variable (Identifier "y"), Const (Int 2)); Return (Const (Int 2)) ] )
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
        , [ Assign (Variable (Identifier "y"), Const (Int 3))
          ; Return (Variable (Identifier "y"))
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
        , [ Assign (Variable (Identifier "y"), Const (Int 3))
          ; Return (ArithOp (Add, Variable (Identifier "y"), Variable (Identifier "x")))
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
        , [ Assign (Variable (Identifier "y"), Const (Int 3))
          ; Return (ArithOp (Add, Variable (Identifier "y"), Variable (Identifier "x")))
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
    ]
;;

let%test _ =
  Ok (Int 120)
  = interpret_exp
      (FunctionCall (Identifier "factorial", [ Const (Int 5) ]))
      (unpacker fact_env1)
;;

let fact_env2 =
  get_env
    env
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
    ]
;;

let%test _ =
  Ok (Int 87178291200)
  = interpret_exp
      (FunctionCall (Identifier "factorial", [ Const (Int 14) ]))
      (unpacker fact_env2)
;;

let%test _ =
  Ok None = interpret_exp (FunctionCall (Identifier "print", [ Const (Int 6003) ])) env
;;

let fact_and_print =
  get_env
    env
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
           , [ FunctionCall (Identifier "factorial", [ Const (Int 5) ]) ] ))
    ]
;;
