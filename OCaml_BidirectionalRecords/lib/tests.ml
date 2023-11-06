open Angstrom
open Parser

(* parse *)

let parse p s show_program =
  match parse_string ~consume:All p s with
  | Ok ast -> print_endline (show_program ast)
  | Error msg -> failwith msg
;;

let%expect_test _ =
  parse pexpr "let rec fact = if n < 1 then 1 else n * fact (n - 1)" Ast.show_expr;
  [%expect
    {|
    (Ast.ELet (Ast.Rec, { Ast.name = "fact"; ty = Ast.Unspecified },
       (Ast.EIfThenElse (
          (Ast.EBinOp (Ast.Lt,
             (Ast.EVar { Ast.name = "n"; ty = Ast.Unspecified }),
             (Ast.EConst (Ast.Int 1)))),
          (Ast.EConst (Ast.Int 1)),
          (Ast.EBinOp (Ast.Mult,
             (Ast.EVar { Ast.name = "n"; ty = Ast.Unspecified }),
             (Ast.EApp ((Ast.EVar { Ast.name = "fact"; ty = Ast.Unspecified }),
                (Ast.EBinOp (Ast.Minus,
                   (Ast.EVar { Ast.name = "n"; ty = Ast.Unspecified }),
                   (Ast.EConst (Ast.Int 1))))
                ))
             ))
          )),
       Ast.EUnit)) |}]
;;
