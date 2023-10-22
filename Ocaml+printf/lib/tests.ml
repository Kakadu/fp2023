open Angstrom
open Ocaml_printf_lib.Parser
open Ocaml_printf_lib.Ast

(* simple tests *)
let%test _ =
  parse_string ~consume:All expr_valname_parser "_" = Ok (Expr_val (LCIdent "_"))
;;

let%test _ =
  parse_string ~consume:All expr_valname_parser "_Aasd0320"
  = Ok (Expr_val (LCIdent "_Aasd0320"))
;;

let%test _ =
  parse_string ~consume:All expr_valname_parser "a" = Ok (Expr_val (LCIdent "a"))
;;

let%test _ = parse_string ~consume:All (parenthesis @@ char 'a') "  (  a  )" = Ok 'a'
let%test _ = parse_string ~consume:All const_integer_parser "123" = Ok (Int 123)

(* big tests *)

let test_parser p input expected =
  let result = parse_string p ~consume:All input in
  match result with
  | Ok actual ->
    let res1 = equal_program expected actual in
    if res1 = false
    then Format.printf "Expected: %a\nActual: %a\n" pp_program expected pp_program actual;
    res1
  | Error msg ->
    Format.printf "Parse error %s\n" msg;
    false
;;

let%test _ =
  let expexted =
    [ Let_decl (false, LCIdent "_a", Expr_const (Int 1))
    ; Let_decl (false, LCIdent "_b", Expr_const (Int 1))
    ]
  in
  test_parser program_parser "  ;; ;;;;   ;; ;;let _a=1;; ;;;; let   _b =1  " expexted
;;

let%test _ =
  let expexted =
    [ Let_decl
        ( false
        , LCIdent "abcde"
        , Fun
            ( LCIdent "a"
            , Fun
                ( LCIdent "b"
                , Fun
                    (LCIdent "c", Fun (LCIdent "d", Fun (LCIdent "e", Expr_const (Int 1))))
                ) ) )
    ]
  in
  test_parser program_parser "let abcde a  b  c  d  e   =1     " expexted
;;

let%test _ =
  let expexted =
    [ Let_decl
        ( false
        , LCIdent "a"
        , Bin_op
            ( Sub
            , Bin_op
                ( Mul
                , Bin_op
                    ( Div
                    , Expr_const (Int 10)
                    , Un_op (Un_plus, Bin_op (Add, Expr_const (Int 2), Expr_const (Int 5)))
                    )
                , Bin_op (Sub, Expr_const (Int 7), Un_op (Un_minus, Expr_const (Int 1)))
                )
            , Bin_op
                ( Add
                , Expr_const (Int 1)
                , Bin_op (Mul, Expr_const (Int 2), Expr_const (Int 3)) ) ) )
    ]
  in
  test_parser
    program_parser
    "let a=  10  /  + (2+5 )  *  ( 7  - -1) - (1 + 2 * 3)"
    expexted
;;
