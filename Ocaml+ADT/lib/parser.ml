(** Copyright 2021-2023, tepa46 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Angstrom
open Format

let is_keyword = function
  | "and"
  | "as"
  | "assert"
  | "asr"
  | "begin"
  | "class"
  | "constraint"
  | "do"
  | "done"
  | "downto"
  | "else"
  | "end"
  | "exception"
  | "external"
  | "false"
  | "for"
  | "fun"
  | "function"
  | "functor"
  | "if"
  | "in"
  | "include"
  | "inherit"
  | "initializer"
  | "land"
  | "lazy"
  | "let"
  | "lor"
  | "lsl"
  | "lsr"
  | "lxor"
  | "match"
  | "method"
  | "mod"
  | "module"
  | "mutable"
  | "new"
  | "nonrec"
  | "object"
  | "of"
  | "open"
  | "or"
  | "private"
  | "rec"
  | "sig"
  | "struct"
  | "then"
  | "to"
  | "true"
  | "try"
  | "type"
  | "val"
  | "virtual"
  | "when"
  | "while"
  | "with" -> true
  | _ -> false
;;

let letdecl a b c d = a, b, c, d

let chainl1 e op =
  let rec go acc = lift2 (fun f x -> f acc x) op e >>= go <|> return acc in
  e >>= fun init -> go init
;;

let rec chainr1 e op = e >>= fun a -> op >>= (fun f -> chainr1 e op >>| f a) <|> return a

let is_space = function
  | ' ' | '\t' | '\n' | '\r' -> true
  | _ -> false
;;

let is_wildcard = function
  | '_' -> true
  | _ -> false
;;

let is_apostrophe = function
  | '\'' -> true
  | _ -> false
;;

let is_lchar = function
  | 'a' .. 'z' -> true
  | _ -> false
;;

let is_uchar = function
  | 'A' .. 'Z' -> true
  | _ -> false
;;

let is_char ch = is_lchar ch || is_uchar ch

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false
;;

let spaces = take_while is_space
let pchunk p = spaces *> p
let check_chunk s = pchunk @@ string s
let pparens p = check_chunk "(" *> p <* check_chunk ")"

(* variable and constructors parser *)
let pvarname =
  pchunk
    (take_while (fun ch -> is_uchar ch || is_digit ch)
     >>= function
     | "" ->
       take_while1 (fun ch ->
         is_wildcard ch || is_apostrophe ch || is_char ch || is_digit ch)
       >>= fun str -> if is_keyword str then fail "invalid var name" else return str
     | _ -> fail "invalid var name")
;;

let pconstrname =
  pchunk
    (take_while (fun ch -> is_lchar ch || is_digit ch)
     >>= function
     | "" ->
       take_while1 (fun ch ->
         is_wildcard ch || is_apostrophe ch || is_char ch || is_digit ch)
       >>= fun str -> if is_keyword str then fail "invalid constr name" else return str
     | _ -> fail "invalid constr name")
;;

(* type parser *)

let pftbase pftype =
  choice
    [ pparens pftype
    ; check_chunk "int" *> return tint
    ; check_chunk "float" *> return tfloat
    ; check_chunk "string" *> return tstring
    ; check_chunk "bool" *> return tbool
    ; tvar <$> (lname <$> pvarname)
    ]
;;

let pftlist pft = tlist <$> pft <* check_chunk "list"

let pfttuple pft =
  sep_by (check_chunk "*") pft
  >>= function
  | [] -> pft
  | [ h ] -> return h
  | h :: tl -> return (ttuple (h :: tl))
;;

let pftfun = check_chunk "->" *> return (fun type1 type2 -> tfun type1 type2)

let pftype =
  fix
  @@ fun pftype ->
  let pft = pftbase pftype in
  let pft = pftlist pft <|> pft in
  let pft = pfttuple pft <|> pft in
  chainr1 pft pftfun
;;

(* expression parser *)

let pfelet pfexp =
  check_chunk "let"
  *> lift4
       letdecl
       (drec <$> (check_chunk "rec" *> return true <|> return false))
       (lname <$> pvarname)
       (dtype <$> (check_chunk ":" *> pftype <|> return temptytype))
       (check_chunk "=" *> pfexp)
;;

let pfe_number = int_of_string <$> take_while1 is_digit
let pfe_string = check_chunk "\"" *> take_while (fun ch -> ch != '\"') <* check_chunk "\""
let pfe_bool = check_chunk "true" *> return true <|> check_chunk "false" *> return false

let pfebase pfexp =
  choice
    [ pparens pfexp
    ; eint <$> pchunk pfe_number
    ; ebool <$> pfe_bool
    ; evar <$> (lname <$> pvarname)
    ; estring <$> pfe_string
    ]
;;

let pfefuntype =
  pparens
    (pvarname (* change to pattern parser *)
     >>= fun n ->
     check_chunk ":" *> (pftype >>= fun t -> return (lname n, dtype t))
     <|> return (lname n, dtype temptytype))
;;

let pfefun pfe =
  fix @@ fun pfefun -> lift2 efun pfefuntype (pfefun <|> check_chunk "->" *> pfe)
;;

let pfeif pfexp =
  fix
  @@ fun pfeif ->
  lift3
    eif
    (check_chunk "if" *> (pfeif <|> pfexp))
    (check_chunk "then" *> (pfeif <|> pfexp))
    (check_chunk "else" *> (pfeif <|> pfexp))
;;

let pfeapp pfe = chainl1 pfe (return (fun exp1 exp2 -> eapp exp1 exp2))

(* parse operators *)

let fmul = check_chunk "*" *> return (fun exp1 exp2 -> ebinop bop_mul exp1 exp2)
let pfemul pfe = chainl1 pfe fmul
let fdiv = check_chunk "/" *> return (fun exp1 exp2 -> ebinop bop_div exp1 exp2)
let pfediv pfe = chainl1 pfe fdiv
let fadd = check_chunk "+" *> return (fun exp1 exp2 -> ebinop bop_add exp1 exp2)
let pfeadd pfe = chainl1 pfe fadd
let fsub = check_chunk "-" *> return (fun exp1 exp2 -> ebinop bop_sub exp1 exp2)
let pfesub pfe = chainl1 pfe fsub
let feq = check_chunk "==" *> return (fun exp1 exp2 -> ebinop bop_eq exp1 exp2)
let pfeeq pfe = chainl1 pfe feq
let fneq = check_chunk "<>" *> return (fun exp1 exp2 -> ebinop bop_neq exp1 exp2)
let pfeneq pfe = chainl1 pfe fneq
let fles = check_chunk "<" *> return (fun exp1 exp2 -> ebinop bop_les exp1 exp2)
let pfeles pfe = chainl1 pfe fles
let fleq = check_chunk "<=" *> return (fun exp1 exp2 -> ebinop bop_leq exp1 exp2)
let pfeleq pfe = chainl1 pfe fleq
let fgre = check_chunk ">" *> return (fun exp1 exp2 -> ebinop bop_gre exp1 exp2)
let pfegre pfe = chainl1 pfe fgre
let fgeq = check_chunk ">=" *> return (fun exp1 exp2 -> ebinop bop_geq exp1 exp2)
let pfegeq pfe = chainl1 pfe fgeq
let fand = check_chunk "&&" *> return (fun exp1 exp2 -> ebinop bop_and exp1 exp2)
let pfeand pfe = chainr1 pfe fand
let ffor = check_chunk "||" *> return (fun exp1 exp2 -> ebinop bop_or exp1 exp2)
let pfeor pfe = chainr1 pfe ffor
let fcons = check_chunk "::" *> return (fun exp1 exp2 -> ebinop bop_cons exp1 exp2)
let pfecons pfe = chainr1 pfe fcons

let pfearith pfe =
  let pfe = pfemul pfe <|> pfe in
  let pfe = pfediv pfe <|> pfe in
  let pfe = pfeadd pfe <|> pfe in
  let pfe = pfesub pfe <|> pfe in
  let pfe = pfecons pfe <|> pfe in
  let pfe = pfeneq pfe <|> pfe in
  let pfe = pfeeq pfe <|> pfe in
  let pfe = pfeles pfe <|> pfe in
  let pfe = pfeleq pfe <|> pfe in
  let pfe = pfegre pfe <|> pfe in
  let pfe = pfegeq pfe <|> pfe in
  pfe
;;

(* doc:https://v2.ocaml.org/manual/expr.html#ss:precedence-and-associativity *)
let pfexp =
  fix
  @@ fun pfexp ->
  let pfe = pfebase pfexp in
  let pfe = pfeapp pfe <|> pfe in
  let pfe = pfearith pfe <|> pfe in
  let pfe = pfeand pfe <|> pfe in
  let pfe = pfeor pfe <|> pfe in
  let pfe = pfeif pfe <|> pfe in
  let pfe = check_chunk "fun" *> pfefun pfe <|> pfe in
  let pfe = lift2 elet (pfelet pfe) (check_chunk "in" *> pfe) <|> pfe in
  pfe
;;

(* declaration parser *)

let pdecl =
  check_chunk "let"
  *> lift4
       dlet
       (drec <$> (check_chunk "rec" *> return true <|> return false))
       (lname <$> pvarname)
       (dtype <$> (check_chunk ":" *> pftype <|> return temptytype))
       (check_chunk "=" *> pfexp)
;;

let parse_program = many pdecl
let parse str = parse_string ~consume:All parse_program str

let ptest str expected =
  match parse str with
  | Ok actual -> List.equal equal_decl expected actual
  | Error err ->
    printf "%s\n" err;
    false
;;

(* tests *)

(* tests for declaration type parsing *)
let%test _ =
  ptest
    "let a : int -> (int -> string) -> int = \"a\""
    [ DLet
        ( DRec false
        , LName "a"
        , DType (TFun (TInt, TFun (TFun (TInt, TString), TInt)))
        , EString "a" )
    ]
;;

let%test _ =
  ptest
    "let a : int list  = \"a\""
    [ DLet (DRec false, LName "a", DType (TList TInt), EString "a") ]
;;

let%test _ =
  ptest
    "let rec abc : ((int list) list) list = \"a\""
    [ DLet (DRec true, LName "abc", DType (TList (TList (TList TInt))), EString "a") ]
;;

let%test _ =
  ptest
    "let rec abc : int -> (int -> (int -> int)) -> int = \"a\""
    [ DLet
        ( DRec true
        , LName "abc"
        , DType (TFun (TInt, TFun (TFun (TInt, TFun (TInt, TInt)), TInt)))
        , EString "a" )
    ]
;;

let%test _ =
  ptest
    "let aBC:  (string * int) list = \"a\""
    [ DLet (DRec false, LName "aBC", DType (TList (TTuple [ TString; TInt ])), EString "a")
    ]
;;

let%test _ =
  ptest
    "let aBC: (int list * (string -> int)) list = \"a\""
    [ DLet
        ( DRec false
        , LName "aBC"
        , DType (TList (TTuple [ TList TInt; TFun (TString, TInt) ]))
        , EString "a" )
    ]
;;

let%test _ =
  ptest
    "let aBC: (int list * string * a) -> string list = \"a\""
    [ DLet
        ( DRec false
        , LName "aBC"
        , DType (TFun (TTuple [ TList TInt; TString; TVar (LName "a") ], TList TString))
        , EString "a" )
    ]
;;

let%test _ =
  ptest
    "let aBC: ((int list -> int) * string * (int * int)) -> string list = \"a\""
    [ DLet
        ( DRec false
        , LName "aBC"
        , DType
            (TFun
               ( TTuple [ TFun (TList TInt, TInt); TString; TTuple [ TInt; TInt ] ]
               , TList TString ))
        , EString "a" )
    ]
;;

(* tests for expression type parsing *)
let%test _ =
  ptest
    "let aBC: (int list) list = fun (a: string -> int) (b: string -> string) -> 5"
    [ DLet
        ( DRec false
        , LName "aBC"
        , DType (TList (TList TInt))
        , EFun
            ( (LName "a", DType (TFun (TString, TInt)))
            , EFun ((LName "b", DType (TFun (TString, TString))), EInt 5) ) )
    ]
;;

let%test _ =
  ptest
    "let aBC: (int list) list = let b: string = 5 in 5"
    [ DLet
        ( DRec false
        , LName "aBC"
        , DType (TList (TList TInt))
        , ELet ((DRec false, LName "b", DType TString, EInt 5), EInt 5) )
    ]
;;

let%test _ =
  ptest
    "let aBC: int -> int = let b: int -> int = fun (x: int) -> x in b"
    [ DLet
        ( DRec false
        , LName "aBC"
        , DType (TFun (TInt, TInt))
        , ELet
            ( ( DRec false
              , LName "b"
              , DType (TFun (TInt, TInt))
              , EFun ((LName "x", DType TInt), EVar (LName "x")) )
            , EVar (LName "b") ) )
    ]
;;

let%test _ =
  ptest
    "let aBC: int -> int = let b: int -> int = fun (x: int) -> (let summ: int = 5 in b) \
     in c"
    [ DLet
        ( DRec false
        , LName "aBC"
        , DType (TFun (TInt, TInt))
        , ELet
            ( ( DRec false
              , LName "b"
              , DType (TFun (TInt, TInt))
              , EFun
                  ( (LName "x", DType TInt)
                  , ELet ((DRec false, LName "summ", DType TInt, EInt 5), EVar (LName "b"))
                  ) )
            , EVar (LName "c") ) )
    ]
;;

let%test _ =
  ptest
    "let aBC: int = fun (n: int) -> if n then 5 else 0"
    [ DLet
        ( DRec false
        , LName "aBC"
        , DType TInt
        , EFun ((LName "n", DType TInt), EIf (EVar (LName "n"), EInt 5, EInt 0)) )
    ]
;;

let%test _ =
  ptest
    "let aBC: int = fun (n: int) -> if 2 - n * (1 - 1 * 3 + 1)  then 5 else 0"
    [ DLet
        ( DRec false
        , LName "aBC"
        , DType TInt
        , EFun
            ( (LName "n", DType TInt)
            , EIf
                ( EBinop
                    ( Sub
                    , EInt 2
                    , EBinop
                        ( Mul
                        , EVar (LName "n")
                        , EBinop
                            ( Sub
                            , EInt 1
                            , EBinop (Add, EBinop (Mul, EInt 1, EInt 3), EInt 1) ) ) )
                , EInt 5
                , EInt 0 ) ) )
    ]
;;

let%test _ =
  ptest
    "let aBC: int -> int = f n e"
    [ DLet
        ( DRec false
        , LName "aBC"
        , DType (TFun (TInt, TInt))
        , EApp (EApp (EVar (LName "f"), EVar (LName "n")), EVar (LName "e")) )
    ]
;;

let%test _ =
  ptest
    "let x = fun (x: int) -> if true then if true then 1 else 2 else 3"
    [ DLet
        ( DRec false
        , LName "x"
        , DType TEmptyType
        , EFun
            ( (LName "x", DType TInt)
            , EIf (EBool true, EIf (EBool true, EInt 1, EInt 2), EInt 3) ) )
    ]
;;

(* test for factorial *)
let%test _ =
  ptest
    "let rec factorial_recursive = fun (n: int) -> if n <= 1 then 1 else n * \
     factorial_recursive (n - 1)"
    [ DLet
        ( DRec true
        , LName "factorial_recursive"
        , DType TEmptyType
        , EFun
            ( (LName "n", DType TInt)
            , EIf
                ( EBinop (Leq, EVar (LName "n"), EInt 1)
                , EInt 1
                , EBinop
                    ( Mul
                    , EVar (LName "n")
                    , EApp
                        ( EVar (LName "factorial_recursive")
                        , EBinop (Sub, EVar (LName "n"), EInt 1) ) ) ) ) )
    ]
;;
