(** Copyright 2023-2024, PavlushaSource *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open! Base
open Ast
open Angstrom

let pp printer parser str =
  Stdlib.Format.printf "%a" printer
  @@ Result.ok_or_failwith
  @@ Angstrom.parse_string ~consume:Angstrom.Consume.All parser str

let chainl1 e op =
  let rec go acc = lift2 (fun f x -> f acc x) op e >>= go <|> return acc in
  e >>= fun init -> go init

let rec chainr1 e op =
  e >>= fun a -> op >>= (fun f -> chainr1 e op >>| f a) <|> return a

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
  | "float"
  | "int"
  | "int32_t"
  | "int16_t"
  | "int8_t"
  | "uint32_t"
  | "uint16_t"
  | "uint8_t"
  | "void"
  | "NULL" ->
      true
  | _ ->
      false

let is_digit = function '0' .. '9' -> true | _ -> false

let is_whitespace = function ' ' | '\t' | '\n' | '\r' -> true | _ -> false

let is_valid_char_id = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' ->
      true
  | _ ->
      false

let whitespace = take_while is_whitespace

let token s = whitespace *> string s

let parens p = token "(" *> p <* token ")"

let brackets p = token "[" *> p <* token "]"

let braces p = token "{" *> p <* token "}"

let p_number =
  let dot =
    peek_char
    >>= function Some '.' -> advance 1 >>| fun () -> true | _ -> return false
  in
  let* whole = whitespace *> take_while1 is_digit in
  let* dot = dot in
  match dot with
  | false ->
      return @@ V_int (int_of_string whole)
  | true ->
      let* part = take_while is_digit in
      return @@ V_float (float_of_string (whole ^ "." ^ part))

let p_ident =
  let is_valid_first_char = function
    | 'a' .. 'z' | 'A' .. 'Z' | '_' ->
        true
    | _ ->
        false
  in
  let* first_char = peek_char in
  match first_char with
  | Some c when is_valid_first_char c ->
      let* ident = take_while is_valid_char_id in
      if is_keywords ident then fail "Keyword name" else return ident
  | _ ->
      fail "Invalid name"

let p_char =
  whitespace *> char '\'' *> any_char <* char '\'' >>| fun char -> V_char char

let p_arr p_elm = braces @@ sep_by (token ",") p_elm

let p_elements_arr p_elm = p_arr p_elm >>= fun c -> return @@ Array_value c

let p_type =
  let parse_simple_type t err =
    match t with
    | "int" ->
        return ID_int32
    | "int32_t" ->
        return ID_int32
    | "int16_t" ->
        return ID_int16
    | "int8_t" ->
        return ID_int8
    | "uint32_t" ->
        return ID_uint32
    | "uint16_t" ->
        return ID_uint16
    | "uint8_t" ->
        return ID_uint8
    | "char" ->
        return ID_char
    | "void" ->
        return ID_void
    | "float" ->
        return ID_float
    | "double" ->
        return ID_float
    | _ ->
        fail @@ "Unknown type in col: " ^ Int.to_string err
  in
  pos
  >>= fun col ->
  let* t = whitespace *> take_while1 is_valid_char_id in
  let* pars = parse_simple_type t col in
  let* list_ptrs = many @@ token "*" in
  return (List.fold ~init:pars ~f:(fun acc _ -> Pointer acc) list_ptrs)

let pe_type = (fun t -> Type t) <$> p_type

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

let ershft = token ">>" *> return (fun e1 e2 -> Bin_expr (Rshift, e1, e2))

let elshift = token "<<" *> return (fun e1 e2 -> Bin_expr (Lshift, e1, e2))

let p_func_call expr =
  lift2
    (fun id ls -> Func_call (id, ls))
    (whitespace *> p_ident)
    (token "(" *> sep_by (token ",") (expr <|> pe_type) <* token ")")

let p_index_array expr =
  let rec helper ind =
    whitespace *> peek_char
    >>= function
    | Some '[' ->
        brackets expr >>= fun e -> helper @@ Index (ind, e)
    | _ ->
        return ind
  in
  whitespace *> p_ident
  >>= fun id ->
  brackets expr
  >>= fun e ->
  whitespace *> peek_char
  >>= function
  | Some '[' ->
      helper @@ Index (Var_name id, e)
  | _ ->
      return @@ Index (Var_name id, e)

let var_name = (fun c -> Var_name c) <$> whitespace *> p_ident

let null = token "NULL" *> return V_null

let p_const : expr t = p_number <|> p_char <|> null >>| fun c -> Const c

let p_un expr =
  whitespace *> peek_char_fail
  >>= function
  | '-' -> (
      advance 1 *> peek_char_fail
      >>= function
      | '-' ->
          advance 1 *> expr
          >>= fun e1 -> return @@ Unary_expr (Pref_decrement, e1)
      | _ ->
          expr >>= fun e1 -> return @@ Unary_expr (Minus, e1) )
  | '+' -> (
      advance 1 *> peek_char_fail
      >>= function
      | '+' ->
          advance 1 *> expr
          >>= fun e1 -> return @@ Unary_expr (Pref_increment, e1)
      | _ ->
          expr >>= fun e1 -> return @@ Unary_expr (Plus, e1) )
  | _ ->
      fail "Expected '-'"

let p_deref expr =
  fix (fun deref ->
      token "*" *> (parens expr <|> p_index_array expr <|> var_name <|> deref)
      >>= fun exp -> return @@ Unary_expr (Dereference, exp) )

let p_address expr =
  (fun exp -> Unary_expr (Address, exp))
  <$> token "&" *> (p_index_array expr <|> parens expr <|> var_name <|> expr)

let p_cast expr =
  lift2 (fun tp exp -> Cast (tp, exp)) (token "(" *> p_type <* token ")") expr

let p_not expr = token "!" *> expr >>= fun c -> return @@ Unary_expr (Not, c)

let p_expr =
  fix (fun expr ->
      let term =
        choice
          [parens expr; p_func_call expr; p_index_array expr; var_name; p_const]
      in
      let term = p_not term <|> term in
      let term = p_cast term <|> term in
      let term = p_address term <|> term in
      let term = p_deref term <|> term in
      let term = p_un term <|> term in
      let term = chainl1 term (emul <|> emod <|> ediv) in
      let term = chainl1 term (eadd <|> esub) in
      let term = chainl1 term (ershft <|> elshift) in
      let term = chainl1 term (elte <|> egte <|> el <|> eg <|> eeq <|> eneq) in
      let term = chainr1 term eand in
      let term = chainr1 term eor in
      let term = fix (fun e -> term <|> p_elements_arr e) <|> term in
      term )

let%expect_test "type parse" =
  pp pp_types p_type "int32_t";
  [%expect {|
    ID_int32 |}]

let%expect_test "shift parse" =
  pp pp_expr p_expr "*a >> 2 + 2";
  [%expect
    {|
    (Bin_expr (Rshift, (Unary_expr (Dereference, (Var_name "a"))),
       (Bin_expr (Add, (Const (V_int 2)), (Const (V_int 2)))))) |}]

let%expect_test "array value parse" =
  pp pp_expr p_expr "{1, 2 + 2, 3}";
  [%expect
    {|
    (Array_value
       [(Const (V_int 1));
         (Bin_expr (Add, (Const (V_int 2)), (Const (V_int 2))));
         (Const (V_int 3))]) |}]

let%expect_test "fun_call test" =
  pp pp_expr p_expr "malloc(n, 12) - malloc(12 - 1)";
  [%expect
    {|
    (Bin_expr (Sub, (Func_call ("malloc", [(Var_name "n"); (Const (V_int 12))])),
       (Func_call ("malloc",
          [(Bin_expr (Sub, (Const (V_int 12)), (Const (V_int 1))))]))
       )) |}]

let%expect_test "index array test" =
  pp pp_expr p_expr "array[1 * 3][2 * 8]";
  [%expect
    {|
    (Index (
       (Index ((Var_name "array"),
          (Bin_expr (Mul, (Const (V_int 1)), (Const (V_int 3)))))),
       (Bin_expr (Mul, (Const (V_int 2)), (Const (V_int 8)))))) |}]

let%expect_test "unary priority test" =
  pp pp_expr p_expr "--1 + 2";
  [%expect
    {|
    (Bin_expr (Add, (Unary_expr (Pref_decrement, (Const (V_int 1)))),
       (Const (V_int 2)))) |}]

let%expect_test "deref priority test" =
  pp pp_expr p_expr "&2 + 2";
  [%expect
    {|
    (Bin_expr (Add, (Unary_expr (Address, (Const (V_int 2)))), (Const (V_int 2))
       )) |}]

let%expect_test "not priority test" =
  pp pp_expr p_expr "!a + 2";
  [%expect
    {|
    (Bin_expr (Add, (Unary_expr (Not, (Var_name "a"))), (Const (V_int 2)))) |}]

let%expect_test "sizeof test" =
  pp pp_expr p_expr "sizeof(char)";
  [%expect {|
    (Func_call ("sizeof", [(Type ID_char)])) |}]

let%expect_test "ref priority test" =
  pp pp_expr p_expr "*a + 2";
  [%expect
    {|
    (Bin_expr (Add, (Unary_expr (Dereference, (Var_name "a"))), (Const (V_int 2))
       )) |}]

let%expect_test "index priority test" =
  pp pp_expr p_expr "a[2] + b[3]";
  [%expect
    {|
    (Bin_expr (Add, (Index ((Var_name "a"), (Const (V_int 2)))),
       (Index ((Var_name "b"), (Const (V_int 3)))))) |}]

let%expect_test "logical not priority test" =
  pp pp_expr p_expr "!a[2]";
  [%expect
    {|
    (Unary_expr (Not, (Index ((Var_name "a"), (Const (V_int 2)))))) |}]

let%expect_test "cast priority test" =
  pp pp_expr p_expr "(int) 2 + 2";
  [%expect
    {|
    (Bin_expr (Add, (Cast (ID_int32, (Const (V_int 2)))), (Const (V_int 2)))) |}]

let p_arg =
  p_type <* whitespace >>= fun t -> p_ident >>= fun id -> return @@ Arg (t, id)

let p_compound statements =
  whitespace *> token "{" *> many (whitespace *> statements <* whitespace)
  <* token "}"
  >>= fun s -> whitespace *> (return @@ Compound s)

let p_if_else statements =
  token "if" *> parens p_expr
  >>= fun cnd ->
  p_compound statements
  >>= fun cmd_if ->
  token "else" *> p_compound statements
  >>= (fun cmd_else -> return @@ If_else (cnd, cmd_if, Some cmd_else))
  <|> return @@ If_else (cnd, cmd_if, None)

let rec p_mult_assign expr1 =
  token "=" *> p_expr
  >>= fun expr2 ->
  whitespace *> peek_char
  >>= function
  | Some '=' ->
      p_mult_assign expr2 >>= fun expr2 -> return @@ Assign (expr1, expr2)
  | Some ';' ->
      advance 1 *> (return @@ Assign (expr1, Expression expr2))
  | _ ->
      fail "expected ';' or '=' at the end"

let p_var_decl =
  let size_arr : int t =
    pos
    >>= fun col ->
    brackets p_number
    >>= function
    | V_int x ->
        return x
    | _ ->
        fail @@ "size of array must be integer. Error in col: "
        ^ Int.to_string col
  in
  let p_type_array t =
    fix (fun arr_type : types t ->
        size_arr
        >>= fun sz ->
        whitespace *> peek_char
        >>= function
        | Some '[' ->
            arr_type >>= fun t -> return @@ Array (Some sz, t)
        | _ ->
            return @@ Array (Some sz, t) )
  in
  p_type
  >>= function
  | ID_void ->
      fail "VOID cannot be a type for variable declaration"
  | t -> (
      whitespace *> p_ident
      >>= fun id ->
      whitespace *> peek_char
      >>= function
      | Some '[' -> (
          p_type_array t
          >>= fun t ->
          token "=" *> p_expr
          >>= fun exp1 ->
          whitespace *> peek_char
          >>= function
          | Some ';' ->
              advance 1 *> (return @@ Var_decl (t, id, Some (Expression exp1)))
          | _ ->
              fail "Error initialization with array" )
      | Some '=' -> (
          advance 1 *> whitespace *> p_expr
          >>= fun exp1 ->
          whitespace *> peek_char
          >>= function
          | Some ';' ->
              advance 1 *> (return @@ Var_decl (t, id, Some (Expression exp1)))
          | Some '=' ->
              p_mult_assign exp1
              >>= fun st -> return @@ Var_decl (t, id, Some st)
          | _ ->
              fail "expected ';' or '=' at the end" )
      | Some ';' ->
          advance 1 *> (return @@ Var_decl (t, id, None))
      | None | _ ->
          fail "Error declaration" )

let p_return =
  token "return" *> whitespace *> peek_char
  >>= function
  | Some ';' ->
      advance 1 *> (return @@ Return (Const V_void))
  | Some _ ->
      p_expr >>= fun exp -> token ";" *> (return @@ Return exp)
  | _ ->
      fail "Error return"

let p_continue = token "continue" *> token ";" *> return Continue

let p_break = token "break" *> token ";" *> return Break

let p_while statements =
  token "while" *> parens p_expr
  >>= fun exp -> p_compound statements >>= fun cmd -> return @@ While (exp, cmd)

let p_assign =
  whitespace *> p_expr <* whitespace
  >>= fun exp1 ->
  peek_char
  >>= function Some '=' -> p_mult_assign exp1 | _ -> fail "Error assign"

let p_func_call_with_st =
  let* expr = p_expr in
  match expr with
  | Func_call _ ->
      token ";" *> return (Expression expr)
  | _ ->
      fail "Error func_call"

let p_for statements =
  token "for" *> token "("
  *> (p_var_decl <|> p_assign >>| Option.some <|> (return None <* token ";"))
  >>= fun st ->
  whitespace *> option None (p_expr >>| Option.some)
  <* token ";"
  >>= fun exp1 ->
  whitespace *> option None (p_expr >>| Option.some)
  <* token ")" <* whitespace
  >>= fun exp2 ->
  p_compound statements >>= fun cmd -> return @@ For (st, exp1, exp2, cmd)

let p_statements =
  fix (fun statements ->
      choice
        [ p_compound statements
        ; p_if_else statements
        ; p_while statements
        ; p_for statements
        ; p_assign
        ; p_var_decl
        ; p_func_call_with_st
        ; p_continue
        ; p_break
        ; p_return ] )

let p_func_decl statements : func_decl t =
  p_type
  >>= fun t ->
  whitespace *> p_ident <* whitespace
  >>= fun id ->
  token "(" *> sep_by (token ",") p_arg
  <* token ")"
  >>= fun argls ->
  whitespace *> peek_char
  >>= function
  | Some '{' ->
      p_compound statements
      >>= fun cmd -> return @@ Func_decl (t, id, argls, cmd)
  | Some ';' ->
      advance 1 >>= fun _ -> fail "Function definitions is not supported"
  | _ ->
      fail "Wrong function declaration or defenition"

let p_programm : program t =
  whitespace *> sep_by whitespace (p_func_decl p_statements)
  >>= fun prog_ls -> return prog_ls

let parse input = parse_string ~consume:All p_programm input
