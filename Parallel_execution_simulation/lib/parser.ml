(** Copyright 2023-2024, Alexandra Lanovaya*)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Base
open Angstrom

let ws =
  skip_while (function
    | ' ' | '\n' | '\t' | '\r' -> true
    | _ -> false)
;;

let parens p = ws *> char '(' *> p <* char ')' <* ws

let integer =
  ws
  *> take_while1 (function
    | '0' .. '9' -> true
    | _ -> false)
  <* ws
  >>= fun int_str ->
  match int_of_string int_str with
  | exception _ -> fail "Invalid integer"
  | n -> return n
;;

let is_reserved = function
  | "eax" | "ebx" | "r1" | "r2" | "r3" | "r4" | "r5" | "r6" | "r7" | "r8" | "r9" -> true
  | _ -> false
;;

let identifier =
  ws
  *> take_while (function
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true
    | _ -> false)
  <* ws
;;

let register =
  ws
  *> choice
       [ string "ebx" *> return (Register "ebx")
       ; string "eax" *> return (Register "eax")
       ; (string "r"
          *> take_while1 (function
            | '0' .. '9' -> true
            | _ -> false)
          >>= fun n -> return (Register ("r" ^ n)))
       ]
;;

let shared_variable =
  identifier
  >>= fun name ->
  if is_reserved name then fail "Invalid identifier" else return (SharedVariable name)
;;

let constant = integer >>| fun i -> Constant i
let barrier = ws *> string "smp" <* ws >>| fun _ -> Barrier

let expr =
  fix (fun expr ->
    choice
      [ parens expr
      ; lift2 (fun r e -> Assign (r, e)) (register <* ws) (string "<-" *> expr <* ws)
      ; lift2
          (fun sv e -> Write (sv, e))
          (shared_variable <* ws)
          (string "<-" *> expr <* ws)
      ; lift (fun r -> Read r) register
      ; constant
      ; lift (fun sv -> ReadShared sv) shared_variable
      ])
;;

let guarded_expr =
  choice
    [ lift3 (fun b1 e b2 -> Some b1, e, Some b2) barrier expr barrier
    ; lift2 (fun b e -> Some b, e, None) barrier expr
    ; lift2 (fun e b -> None, e, Some b) expr barrier
    ; lift (fun e -> None, e, None) expr
    ]
;;

let thread = sep_by1 (string "|||") guarded_expr
let ast = sep_by1 (char '\n') thread

let parse str show_ast =
  match parse_string ~consume:All ast str with
  | Ok ast -> print_endline (show_ast ast)
  | Error msg -> failwith msg
;;

let parse_prog str = parse str Ast.show_ast

let%expect_test "parse_register" =
  parse "eax<-0" Ast.show_ast;
  [%expect {| [[(None, (Ast.Assign ((Ast.Register "eax"), (Ast.Constant 0))), None)]] |}]
;;

let%expect_test "parse_shared_variable" =
  parse "x<-eax" Ast.show_ast;
  [%expect
    {|
    [[(None,
       (Ast.Write ((Ast.SharedVariable "x"), (Ast.Read (Ast.Register "eax")))),
       None)]
      ] |}]
;;

let%expect_test "parse_simple_helloworld" =
  parse "x<-60 ||| y<-90 smp ||| smp r1<-x ||| r2<-y" Ast.show_ast;
  [%expect
    {|
    [[(None, (Ast.Write ((Ast.SharedVariable "x"), (Ast.Constant 60))), None);
       (None, (Ast.Write ((Ast.SharedVariable "y"), (Ast.Constant 90))),
        (Some Ast.Barrier));
       ((Some Ast.Barrier),
        (Ast.Assign ((Ast.Register "r1"),
           (Ast.ReadShared (Ast.SharedVariable "x")))),
        None);
       (None,
        (Ast.Assign ((Ast.Register "r2"),
           (Ast.ReadShared (Ast.SharedVariable "y")))),
        None)
       ]
      ] |}]
;;
