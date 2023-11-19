(** Copyright 2021-2023, Nikita Nemakin *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Base
open Angstrom

let is_idc c = Char.is_alphanum c || Char.equal c '_'

let is_keyword = function
  | "let"
  | "rec"
  | "in"
  | "fun"
  | "match"
  | "with"
  | "if"
  | "then"
  | "else"
  | "true"
  | "false" -> true
  | _ -> false
;;

let ws = take_while Char.is_whitespace
let token s = ws *> string s
let lp = token "("
let rp = token ")"
let lsb = token "["
let rsb = token "]"
let semi = token ";"
let dsemi = token ";;"
let dcol = token "::"
let comma = token ","
let grd = token "|"
let arr = token "->"
let parens p = lp *> p <* rp
let brackets p = lsb *> p <* rsb

(*--------------------------------- Constants --------------------------------*)

let cint i = CInt i
let cbool b = CBool b
let cstring s = CString s

let pcint =
  let* sign = choice [ token "-"; token "+"; token "" ] in
  let* num = take_while1 Char.is_digit in
  return @@ Int.of_string (sign ^ num) >>| cint
;;

let pcbool =
  let t = token "true" *> return (cbool true) in
  let f = token "false" *> return (cbool false) in
  choice [ t; f ]
;;

let pcstring =
  token "\""
  *> take_while (function
    | '"' -> false
    | _ -> true)
  <* char '"'
  >>| cstring
;;

let pcunit = token "()" *> return CUnit
let pcnil = token "[]" *> return CNil
let const = choice [ pcint; pcbool; pcstring; pcunit; pcnil ]

(*------------------------------ Patterns ------------------------------------*)

let pconst c = PConst c
let pvar x = PVar x
let ppoly t p = PPoly (t, p)
let pcons p1 p2 = PCons (p1, p2)
let ptuple ps = PTuple ps
let ppoly t p = PPoly (t, p)
let ppconst = const >>| pconst
let ppany = token "_" *> return PAny

let varname =
  let* fst =
    ws
    *> satisfy (function
      | 'a' .. 'z' | '_' -> true
      | _ -> false)
  in
  let* rest = take_while is_idc in
  match String.of_char fst ^ rest with
  | "_" -> fail "Wildcard can't be used as indetifier"
  | s when is_keyword s -> fail "Keyword can't be used as identifier"
  | _ as name -> return name
;;

let tagname =
  let* fst = token "`" *> satisfy Char.is_uppercase in
  let* rest = take_while is_idc in
  return @@ String.of_char fst ^ rest
;;

let ppvar = varname >>| pvar
let pppoly p = lift2 ppoly tagname (option None (p >>| fun p -> Some p))
let pptuple p = lift2 List.cons p (many1 (comma *> p)) >>| ptuple
let pplist p = brackets @@ sep_by1 semi p >>| List.fold_right ~f:pcons ~init:(pconst CNil)

let chainl1 e op =
  let rec go acc = lift2 (fun f x -> f acc x) op e >>= go <|> return acc in
  e >>= go
;;

let chainr1 e op =
  let rec go acc = lift2 (fun f x -> f acc x) op (e >>= go) <|> return acc in
  e >>= go
;;

type pdispatch =
  { pat : pdispatch -> pattern t
  ; tuple : pdispatch -> pattern t
  ; poly : pdispatch -> pattern t
  ; cons : pdispatch -> pattern t
  }

let pack =
  let pat pack =
    fix (fun _ -> choice [ pack.tuple pack; pack.cons pack; pack.poly pack ])
  in
  let term pack =
    choice
      [ pack.poly pack
      ; ppconst
      ; ppvar
      ; ppany
      ; pplist @@ pack.pat pack
      ; parens @@ pack.pat pack
      ]
  in
  let tuple pack = fix (fun _ -> pptuple @@ pack.cons pack) in
  let poly pack = fix (fun _ -> pppoly @@ term pack) in
  let cons pack = fix (fun _ -> chainr1 (term pack) (dcol *> return pcons)) in
  { pat; tuple; poly; cons }
;;

let pattern = pack.pat pack

(*----------------------------- Expressions ----------------------------------*)
let econst c = EConst c
let evar x = EVar x
let ebinop op e1 e2 = EBin_op (op, e1, e2)
let eif e1 e2 e3 = EIf (e1, e2, e3)
let ematch e cl = EMatch (e, cl)
let elet f b e = ELet (f, b, e)
let efun p e = EFun (p, e)
let etuple el = ETuple el
let econs e1 e2 = ECons (e1, e2)
let eapply e1 e2 = EApply (e1, e2)
let epoly t e = EPoly (t, e)
let peconst = const >>| econst
let pevar = varname >>| evar
let peif p = lift3 eif (token "if" *> p) (token "then" *> p) (token "else" *> p)

let pematch pe =
  let pexpr = token "match" *> pe <* token "with" <* option "" grd in
  let pcase = lift2 (fun p e -> p, e) (pattern <* arr) pe in
  lift2 ematch pexpr (sep_by1 grd pcase)
;;

let pepoly p = lift2 epoly tagname (option None (p >>| fun e -> Some e))
let petuple p = lift2 List.cons p (many1 (comma *> p)) >>| etuple
let pelist p = brackets @@ sep_by1 semi p >>| List.fold_right ~f:econs ~init:(econst CNil)
let efunf ps e = List.fold_right ps ~f:efun ~init:e
let pefun pe = lift2 efun (token "fun" *> pattern) (lift2 efunf (many pattern <* arr) pe)

let pelet pe =
  lift3
    elet
    (token "let" *> option Nonrec (token "rec" *> return Rec))
    (both pattern (lift2 efunf (many pattern <* token "=") pe))
    (token "in" *> pe)
;;

let pmul = token "*" *> return (ebinop Mul)
let pdiv = token "/" *> return (ebinop Div)
let padd = token "+" *> return (ebinop Add)
let psub = token "-" *> return (ebinop Sub)
let peq = token "=" *> return (ebinop Eq)
let plt = token "<" *> return (ebinop Lt)
let plte = token "<=" *> return (ebinop Lte)
let pneq = token "<>" *> return (ebinop Neq)
let pgt = token ">" *> return (ebinop Gt)
let pgte = token ">=" *> return (ebinop Gte)
let pand = token "&&" *> return (ebinop And)
let por = token "||" *> return (ebinop Or)

(*
   fun apply, tag apply  left
  * /                   left
  + -                   left
  ::                    right
  = <...                left
  >...                  left
  &&                    right
  ||                    right
  if                      -
  let match fun           -
*)

let expr =
  fix (fun expr ->
    let expr = choice [ peconst; pevar; pelist expr; parens expr ] in
    let expr = pepoly expr <|> expr in
    let expr = chainl1 expr (return eapply) in
    let expr = chainl1 expr (pmul <|> pdiv) in
    let expr = chainl1 expr (padd <|> psub) in
    let expr = chainr1 expr (dcol *> return econs) in
    let expr = chainl1 expr (choice [ plte; plt; pneq; peq ]) in
    let expr = chainl1 expr (pgt <|> pgte) in
    let expr = chainr1 expr pand in
    let expr = chainr1 expr por in
    let expr = petuple expr <|> expr in
    let expr = peif expr <|> expr in
    let expr = choice [ pelet expr; pematch expr; pefun expr; expr ] in
    expr)
;;

(*----------------------------- Structure ------------------------------------*)

let str_item =
  let pseval = expr >>| fun e -> SEval e in
  let svalue f b = SValue (f, b) in
  let psvalue =
    lift2
      svalue
      (token "let" *> option Nonrec (token "rec" *> return Rec))
      (both pattern (lift2 efunf (many pattern <* token "=") expr))
  in
  choice [ pseval; psvalue ]
;;

let structure : structure t = sep_by dsemi str_item
let parse s = parse_string ~consume:Prefix structure s
(*-------------------------------- Tests -------------------------------------*)

let test_parse p to_str input =
  let parse p s = parse_string ~consume:Prefix p s in
  match parse p input with
  | Ok ast -> Format.printf "%s\n" (to_str ast)
  | Error s -> Format.printf "%s\n" s
;;

let%expect_test _ =
  test_parse pattern show_pattern "123";
  [%expect {| (PConst (CInt 123)) |}]
;;

let%expect_test _ =
  test_parse pattern show_pattern "_";
  [%expect {| PAny |}]
;;

let%expect_test _ =
  test_parse pattern show_pattern "_A";
  [%expect {| (PVar "_A") |}]
;;

let%expect_test _ =
  test_parse pattern show_pattern "true";
  [%expect {| (PConst (CBool true)) |}]
;;

let%expect_test _ =
  test_parse pattern show_pattern "\"true\"";
  [%expect {| (PConst (CString "true")) |}]
;;

let%expect_test _ =
  test_parse pattern show_pattern " [] ";
  [%expect {| (PConst CNil) |}]
;;

let%expect_test _ =
  test_parse pattern show_pattern " () ";
  [%expect {| (PConst CUnit) |}]
;;

let%expect_test _ =
  test_parse pattern show_pattern " `A ";
  [%expect {| (PPoly ("A", None)) |}]
;;

let%expect_test _ =
  test_parse pattern show_pattern " `A `B ";
  [%expect {| (PPoly ("A", (Some (PPoly ("B", None))))) |}]
;;

let%expect_test _ =
  test_parse pattern show_pattern " `A 123 ";
  [%expect {| (PPoly ("A", (Some (PConst (CInt 123))))) |}]
;;

let%expect_test _ =
  test_parse pattern show_pattern " 123, 321 ";
  [%expect {| (PTuple [(PConst (CInt 123)); (PConst (CInt 321))]) |}]
;;

let%expect_test _ =
  test_parse pattern show_pattern " [123] ";
  [%expect {| (PCons ((PConst (CInt 123)), (PConst CNil))) |}]
;;

let%expect_test _ =
  test_parse pattern show_pattern " [1; 2; 3] ";
  [%expect
    {| 
    (PCons ((PConst (CInt 1)), 
       (PCons ((PConst (CInt 2)), (PCons ((PConst (CInt 3)), (PConst CNil))))))) |}]
;;

let%expect_test _ =
  test_parse pattern show_pattern " [1, 2; 3, 4] ";
  [%expect
    {| 
  (PCons ((PTuple [(PConst (CInt 1)); (PConst (CInt 2))]), 
     (PCons ((PTuple [(PConst (CInt 3)); (PConst (CInt 4))]), (PConst CNil))))) |}]
;;

let%expect_test _ =
  test_parse pattern show_pattern " `A 123 ";
  [%expect {| (PPoly ("A", (Some (PConst (CInt 123))))) |}]
;;

let%expect_test _ =
  test_parse pattern show_pattern " 123, 321 ";
  [%expect {| (PTuple [(PConst (CInt 123)); (PConst (CInt 321))]) |}]
;;

let%expect_test _ =
  test_parse pattern show_pattern " [123] ";
  [%expect {| (PCons ((PConst (CInt 123)), (PConst CNil))) |}]
;;

let%expect_test _ =
  test_parse pattern show_pattern " [1; 2; 3] ";
  [%expect
    {| 
   (PCons ((PConst (CInt 1)),
      (PCons ((PConst (CInt 2)), (PCons ((PConst (CInt 3)), (PConst CNil))))))) |}]
;;

let%expect_test _ =
  test_parse pattern show_pattern " [1, 2; 3, 4] ";
  [%expect
    {| 
   (PCons ((PTuple [(PConst (CInt 1)); (PConst (CInt 2))]),
      (PCons ((PTuple [(PConst (CInt 3)); (PConst (CInt 4))]), (PConst CNil))))) |}]
;;

let%expect_test _ =
  test_parse pattern show_pattern " [1; 2], [3; 4] ";
  [%expect
    {| 
  (PTuple 
     [(PCons ((PConst (CInt 1)), (PCons ((PConst (CInt 2)), (PConst CNil)))));
       (PCons ((PConst (CInt 3)), (PCons ((PConst (CInt 4)), (PConst CNil)))))])|}]
;;

let%expect_test _ =
  test_parse pattern show_pattern " 123 :: [] ";
  [%expect {| 
   (PCons ((PConst (CInt 123)), (PConst CNil))) |}]
;;

let%expect_test _ =
  test_parse pattern show_pattern " _ :: _ :: _ ";
  [%expect {| 
   (PCons (PAny, (PCons (PAny, PAny)))) |}]
;;

let%expect_test _ =
  test_parse pattern show_pattern " `A 1, 2 ";
  [%expect {| 
   (PTuple [(PPoly ("A", (Some (PConst (CInt 1))))); (PConst (CInt 2))])|}]
;;

let%expect_test _ =
  test_parse pattern show_pattern " `A (1, 2) ";
  [%expect {| 
   (PPoly ("A", (Some (PTuple [(PConst (CInt 1)); (PConst (CInt 2))]))))|}]
;;

let%expect_test _ =
  test_parse pattern show_pattern " `A 1 :: [`A 3] ";
  [%expect
    {| 
   (PCons ((PPoly ("A", (Some (PConst (CInt 1))))),
      (PCons ((PPoly ("A", (Some (PConst (CInt 3))))), (PConst CNil)))))|}]
;;

let%expect_test _ =
  test_parse
    structure
    show_structure
    {| let rec fac n = if n < 2 then 1 else n * fac(n - 1);; |};
  [%expect
    {|
    [(SValue (Rec,
        ((PVar "fac"),
         (EFun ((PVar "n"),
            (EIf ((EBin_op (Lt, (EVar "n"), (EConst (CInt 2)))),
               (EConst (CInt 1)),
               (EBin_op (Mul, (EVar "n"),
                  (EApply ((EVar "fac"),
                     (EBin_op (Sub, (EVar "n"), (EConst (CInt 1))))))
                  ))
               ))
            )))
        ))
      ] |}]
;;

let%expect_test _ =
  test_parse
    structure
    show_structure
    {| let f = let g x = x + 1 in g;;

     let rec len l = 
       match l with
       | [] -> 0
       | _ :: xs -> 1 + len xs
     ;; |};
  [%expect
    {|
      [(SValue (Nonrec,
          ((PVar "f"),
           (ELet (Nonrec,
              ((PVar "g"),
               (EFun ((PVar "x"), (EBin_op (Add, (EVar "x"), (EConst (CInt 1))))))),
              (EVar "g"))))
          ));
        (SValue (Rec,
           ((PVar "len"),
            (EFun ((PVar "l"),
               (EMatch ((EVar "l"),
                  [((PConst CNil), (EConst (CInt 0)));
                    ((PCons (PAny, (PVar "xs"))),
                     (EBin_op (Add, (EConst (CInt 1)),
                        (EApply ((EVar "len"), (EVar "xs"))))))
                    ]
                  ))
               )))
           ))
        ] |}]
;;

let%expect_test _ =
  test_parse
    structure
    show_structure
    {| let f = (fun x -> x + 1) 123 in f;;
       let x, y, z = (1, 2, 3);; |};
  [%expect
    {|
      [(SEval
          (ELet (Nonrec,
             ((PVar "f"),
              (EApply (
                 (EFun ((PVar "x"), (EBin_op (Add, (EVar "x"), (EConst (CInt 1))))
                    )),
                 (EConst (CInt 123))))),
             (EVar "f"))));
        (SValue (Nonrec,
           ((PTuple [(PVar "x"); (PVar "y"); (PVar "z")]),
            (ETuple [(EConst (CInt 1)); (EConst (CInt 2)); (EConst (CInt 3))]))
           ))
        ] |}]
;;
