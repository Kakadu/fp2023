(** Copyright 2021-2023, PavlushaSource *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open! Base
open Ast
open Angstrom

let pp printer parser str =
  Stdlib.Format.printf "%a" printer
  @@ Result.ok_or_failwith
  @@ Angstrom.parse_string ~consume:Angstrom.Consume.All parser str
;;

let chainl1 e op =
  let rec go acc = lift2 (fun f x -> f acc x) op e >>= go <|> return acc in
  e >>= fun init -> go init
;;

let rec chainr1 e op = e >>= fun a -> op >>= (fun f -> chainr1 e op >>| f a) <|> return a

let is_keywords = function
  | "while"
  | "for"
  | "break"
  | "continue"
  | "if"
  | "else"
  | "return"
  | "char"
  | "const"
  | "double"
  | "float"
  | "int"
  | "int32_t"
  | "int16_t"
  | "int8_t"
  | "uint32_t"
  | "uint16_t"
  | "uint8_t"
  | "void"
  | "NULL" -> true
  | _ -> false
;;

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false
;;

let is_whitespace = function
  | ' ' | '\t' | '\n' | '\r' -> true
  | _ -> false
;;

(* remember that the name cannot begin with a number *)
let is_valid_char_id = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true
  | _ -> false
;;

let whitespace = take_while is_whitespace
let token s = whitespace *> string s
let parens p = token "(" *> p <* token ")"
let brackets p = token "[" *> p <* token "]"
let braces p = token "{" *> p <* token "}"

let number =
  let dot =
    peek_char
    >>= function
    | Some '.' -> advance 1 >>| fun () -> true
    | _ -> return false
  in
  let sign =
    peek_char
    >>= function
    | Some '-' -> advance 1 >>| fun () -> "-"
    | Some '+' -> advance 1 >>| fun () -> "+"
    | Some c when is_digit c -> return "+"
    | _ -> fail "Sign or digit expected"
  in
  let* sign = whitespace *> sign in
  let* whole = take_while1 is_digit in
  let* dot = dot in
  match dot with
  | false -> return @@ V_int (int_of_string (sign ^ whole))
  | true ->
    let* part = take_while is_digit in
    return @@ V_float (float_of_string (sign ^ whole ^ "." ^ part))
;;

let identifier =
  let is_valid_first_char = function
    | 'a' .. 'z' | 'A' .. 'Z' | '_' -> true
    | _ -> false
  in
  let* first_char = peek_char in
  match first_char with
  | Some c when is_valid_first_char c ->
    let* ident = take_while is_valid_char_id in
    if is_keywords ident then fail "Keyword name" else return ident
  | _ -> fail "Invalid name"
;;

let parse_char =
  whitespace *> char '\'' *> any_char <* char '\'' >>| fun char -> V_char char
;;

let parr pelm = braces @@ sep_by (token ",") pelm
let parse_elms_arr pelm = parr pelm >>= fun c -> return @@ Const (V_array c)

let parse_type =
  let parse_simple_type = function
    | "int" -> return ID_int
    | "int32_t" -> return ID_int32
    | "int16_t" -> return ID_int16
    | "int8_t" -> return ID_int8
    | "uint32_t" -> return ID_uint32
    | "uint16_t" -> return ID_uint16
    | "uint8_t" -> return ID_uint8
    | "char" -> return ID_char
    | "void" -> return ID_void
    | "float" -> return ID_float
    | "double" -> return ID_float
    | _ -> fail "Unknown type"
  in
  let* t = whitespace *> take_while1 is_valid_char_id in
  let* pars = parse_simple_type t in
  let* list_ptrs = many @@ token "*" in
  return (List.fold ~init:pars ~f:(fun acc _ -> Pointer acc) list_ptrs)
;;

let emul = token "*" *> return (fun e1 e2 -> Bin_expr (Mul, e1, e2))
let ediv = token "/" *> return (fun e1 e2 -> Bin_expr (Div, e1, e2))
let emod = token "%" *> return (fun e1 e2 -> Bin_expr (Mod, e1, e2))
let eadd = token "+" *> return (fun e1 e2 -> Bin_expr (Add, e1, e2))
let esub = token "-" *> return (fun e1 e2 -> Bin_expr (Sub, e1, e2))
let el = token "<" *> return (fun e1 e2 -> Bin_expr (Less, e1, e2))
let eg = token ">" *> return (fun e1 e2 -> Bin_expr (Grow, e1, e2))
let elte = token "<=" *> return (fun e1 e2 -> Bin_expr (LessOrEqual, e1, e2))
let egte = token ">=" *> return (fun e1 e2 -> Bin_expr (GrowOrEqual, e1, e2))
let eeq = token "==" *> return (fun e1 e2 -> Bin_expr (Equal, e1, e2))
let eneq = token "!= " *> return (fun e1 e2 -> Bin_expr (NotEqual, e1, e2))
let eand = token "&&" *> return (fun e1 e2 -> Bin_expr (And, e1, e2))
let eor = token "||" *> return (fun e1 e2 -> Bin_expr (Or, e1, e2))
let epr2 = emul <|> emod <|> ediv
let epr3 = eadd <|> esub
let epr4 = elte <|> egte <|> el <|> eg <|> eeq <|> eneq

let parse_expr : expr t =
  fix (fun expr ->
    let var_name = (fun c -> Var_name c) <$> whitespace *> identifier in
    let null = token "NULL" *> (return @@ V_null) in
    let parse_const : expr t = number <|> parse_char <|> null >>| fun c -> Const c in
    let index = lift2 (fun id exp -> Index (id, exp)) identifier @@ brackets expr in
    let not = token "!" *> expr >>= fun c -> return @@ Unary_expr (Not, c) in
    let deref =
      fix (fun deref ->
        token "*" *> (parens expr <|> index <|> var_name <|> deref)
        >>= fun exp -> return @@ Unary_expr (Dereference, exp))
    in
    let address =
      (fun exp -> Unary_expr (Address, exp))
      <$> token "&" *> (index <|> parens expr <|> var_name <|> expr)
    in
    let func_call =
      lift2
        (fun id ls -> Func_call (id, ls))
        (whitespace *> identifier)
        (token "(" *> sep_by (token ",") (expr <|> (parse_type >>| fun t -> Type t))
         <* token ")")
    in
    let cast =
      lift2 (fun tp exp -> Cast (tp, exp)) (token "(" *> parse_type <* token ")") expr
    in
    let un_arith =
      whitespace *> peek_char_fail
      >>= function
      | '-' ->
        advance 1 *> peek_char_fail
        >>= (function
        | '-' -> advance 1 *> expr >>= fun e1 -> return @@ Unary_expr (Pref_decrement, e1)
        | _ -> expr >>= fun e1 -> return @@ Unary_expr (Minus, e1))
      | '+' ->
        advance 1 *> peek_char_fail
        >>= (function
        | '+' -> advance 1 *> expr >>= fun e1 -> return @@ Unary_expr (Pref_increment, e1)
        | _ -> expr >>= fun e1 -> return @@ Unary_expr (Plus, e1))
      | _ -> fail "Expected '-'"
    in
    let term =
      choice
        [ parens expr; func_call; var_name; parse_const; not; un_arith; address; deref ]
    in
    let term = chainl1 term epr2 in
    let term = chainl1 term epr3 in
    let term = chainl1 term epr4 in
    let term = chainl1 term eand in
    let term = chainl1 term eor in
    let parse_array : expr t = fix (fun e1 -> term <|> parse_elms_arr e1) in
    choice [ parens expr; index; cast; term; parse_array ])
;;

(*-----------------------------*)

let argdecl =
  parse_type <* whitespace >>= fun t -> identifier >>= fun id -> return @@ Arg (t, id)
;;

let compound statements =
  whitespace *> token "{" *> many (whitespace *> statements <* whitespace)
  <* token "}"
  >>= fun s -> whitespace *> (return @@ Compound s)
;;

let if_stmt statements =
  token "if" *> parens parse_expr
  >>= fun cnd -> compound statements >>= fun cmpd -> return @@ If (cnd, cmpd)
;;

let if_else_stmt statements =
  token "if" *> parens parse_expr
  >>= fun cnd ->
  compound statements
  >>= fun cmd_if ->
  token "else" *> compound statements
  >>= fun cmd_else -> return @@ If_else (cnd, cmd_if, cmd_else)
;;

let var_decl =
  let size_arr : int t =
    brackets number
    >>= function
    | V_int x -> return x
    | _ -> fail "size of array must be integer"
  in
  let parse_arr_type t =
    fix (fun arr_type : types t ->
      size_arr
      >>= fun sz ->
      whitespace *> peek_char
      >>= function
      | Some '[' -> arr_type >>= fun t -> return @@ Array (Some sz, t)
      | _ -> return @@ Array (Some sz, t))
  in
  let value idd t =
    match t with
    | ID_int | ID_float | Array _ | Pointer _ ->
      parse_expr >>= fun exp -> return @@ Var_decl (t, idd, Some exp)
    | _ -> fail "Undefinded type"
  in
  parse_type
  >>= function
  | ID_void -> fail "VOID cannot be a type for variable declaration"
  | t ->
    whitespace *> identifier
    >>= fun id ->
    whitespace *> peek_char
    >>= (function
    | Some '[' ->
      parse_arr_type t
      >>= fun t -> token "=" *> value id t >>= fun v -> token ";" *> return v
    | Some '=' -> advance 1 *> whitespace *> value id t >>= fun v -> token ";" *> return v
    | Some ';' -> advance 1 *> (return @@ Var_decl (t, id, None))
    | None | _ -> fail "Error declaration")
;;

let return_parse =
  token "return" *> whitespace *> peek_char
  >>= function
  | Some ';' -> advance 1 *> (return @@ Return (Const V_void))
  | Some _ -> parse_expr >>= fun exp -> token ";" *> (return @@ Return exp)
  | _ -> fail "Error return"
;;

let continue_parse = token "continue" *> token ";" *> return Continue
let break_parse = token "break" *> token ";" *> return Break
(* let for statements =
   token "for" *> sep_by () *)

let while_stmt statements =
  token "while" *> parens parse_expr
  >>= fun exp -> compound statements >>= fun cmd -> return @@ While (exp, cmd)
;;

let assing_parse =
  whitespace *> parse_expr
  <* whitespace
  >>= fun exp1 ->
  peek_char
  >>= function
  | Some '=' ->
    advance 1 *> parse_expr
    <* token ";"
    >>= fun exp2 -> return @@ Assign (exp1, Expression exp2)
  | _ -> fail "Error assign"
;;

let for_parse statements =
  token "for"
  *> token "("
  *> (var_decl
      <|> assing_parse
      >>= (fun var -> return @@ Some var)
      <|> (return None <* token ";"))
  >>= fun var ->
  parse_expr
  >>= (fun var -> return @@ Some var <|> return None)
  >>= fun exp ->
  token ";" *> (parse_expr >>= (fun step -> return @@ Some step) <|> return None)
  >>= fun step ->
  token ")" *> whitespace *> compound statements
  >>= fun cmd -> return @@ For (var, exp, step, cmd)
;;

let statements_parse =
  fix (fun statements ->
    choice
      [ compound statements
      ; if_else_stmt statements
      ; if_stmt statements
      ; while_stmt statements
      ; for_parse statements
      ; assing_parse
      ; var_decl
      ; continue_parse
      ; break_parse
      ; return_parse
      ])
;;

(*-----------------------------*)

let funcdecl statements =
  parse_type
  >>= fun t ->
  whitespace *> identifier
  <* whitespace
  >>= fun id ->
  token "(" *> sep_by (token ",") argdecl
  <* token ")"
  >>= fun argls ->
  whitespace *> peek_char
  >>= function
  | Some '{' ->
    compound statements >>= fun cmd -> return @@ Func_def (Func_decl (t, id, argls), cmd)
  | Some ';' -> advance 1 >>= fun _ -> return @@ Func_decl (t, id, argls)
  | _ -> fail "ERROR func decl"
;;

let top_var_parse =
  var_decl
  >>= function
  | Var_decl (idd, tp, exp) -> return @@ Top_var_decl (idd, tp, exp)
  | _ -> fail "ERROR"
;;

let parse_prog =
  whitespace *> sep_by whitespace (funcdecl statements_parse <|> top_var_parse)
  >>= fun prog_ls -> return @@ My_programm prog_ls
;;

let parse input = parse_string ~consume:All parse_prog input

(*-----------------------------*)
let%expect_test "parse_arith" =
  pp pp_expr parse_expr "malloc((double) 1 + 2, ++3, -(-5) * ((int) 6), {1, --2, ++3.5})";
  [%expect
    {|
    (Func_call ("malloc",
       [(Cast (ID_float, (Bin_expr (Add, (Const (V_int 1)), (Const (V_int 2))))));
         (Unary_expr (Pref_increment, (Const (V_int 3))));
         (Bin_expr (Mul, (Unary_expr (Minus, (Const (V_int -5)))),
            (Cast (ID_int, (Const (V_int 6))))));
         (Const
            (V_array
               [(Const (V_int 1));
                 (Unary_expr (Pref_decrement, (Const (V_int 2))));
                 (Unary_expr (Pref_increment, (Const (V_float 3.5))))]))
         ]
       )) |}]
;;

let%expect_test "parse_array" =
  pp
    pp_expr
    (parse_expr <* whitespace)
    "{ 'a',  2 , {1 , 2 , 3}  , name,  12, 43.23, 'c'}";
  [%expect
    {|
    (Const
       (V_array
          [(Const (V_char 'a')); (Const (V_int 2));
            (Const
               (V_array [(Const (V_int 1)); (Const (V_int 2)); (Const (V_int 3))]));
            (Var_name "name"); (Const (V_int 12)); (Const (V_float 43.23));
            (Const (V_char 'c'))])) |}]
;;

let%expect_test "parse_name" =
  pp pp_expr parse_expr "malloc(a, b)";
  [%expect {|
    (Func_call ("malloc", [(Var_name "a"); (Var_name "b")])) |}]
;;

let%expect_test "arg_decl" =
  pp pp_arg argdecl "int a";
  [%expect {|
    (Arg (ID_int, "a")) |}]
;;

let%expect_test "var decl" =
  pp pp_statements var_decl "int count[2][2] = {{1, 2}, {3, 4}};";
  [%expect
    {|
    (Var_decl ((Array ((Some 2), (Array ((Some 2), ID_int)))), "count",
       (Some (Const
                (V_array
                   [(Const (V_array [(Const (V_int 1)); (Const (V_int 2))]));
                     (Const (V_array [(Const (V_int 3)); (Const (V_int 4))]))])))
       )) |}]
;;

let%expect_test "factorial" =
  pp
    pp_prog
    parse_prog
    "int factorial(int n) {\n\
    \    if (n >= 1) {\n\
    \      return n * factorial(n - 1);\n\
    \    }\n\
    \    else {\n\
    \      return 1;\n\
    \    } \n\
    \  }\n\
    \  int main() {\n\
    \    int n = 5; \n\
    \    return factorial(n);\n\
    \  }\n\
    \  ";
  [%expect
    {|
    (My_programm
       [(Func_def ((Func_decl (ID_int, "factorial", [(Arg (ID_int, "n"))])),
           (Compound
              [(If_else (
                  (Bin_expr (GrowOrEqual, (Var_name "n"), (Const (V_int 1)))),
                  (Compound
                     [(Return
                         (Bin_expr (Mul, (Var_name "n"),
                            (Func_call ("factorial",
                               [(Bin_expr (Sub, (Var_name "n"), (Const (V_int 1))
                                   ))
                                 ]
                               ))
                            )))
                       ]),
                  (Compound [(Return (Const (V_int 1)))])))
                ])
           ));
         (Func_def ((Func_decl (ID_int, "main", [])),
            (Compound
               [(Var_decl (ID_int, "n", (Some (Const (V_int 5)))));
                 (Return (Func_call ("factorial", [(Var_name "n")])))])
            ))
         ]) |}]
;;
