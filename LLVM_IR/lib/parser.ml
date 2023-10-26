open Angstrom

let test_parse p show str =
  match Angstrom.parse_string ~consume:Consume.Prefix p str with
  | Result.Error _ -> Format.printf "Error"
  | Result.Ok ast -> Format.printf "%s" (show ast)
;;

let whitespace = function
  | ' ' | '\t' | '\n' | '\r' -> true
  | _ -> false
;;

let comment =
  char ';'
  *> return ()
  *> skip_while (function
    | '\n' -> false
    | _ -> true)
;;

let whitespaces = many (skip whitespace <|> comment)

let str_integer =
  take_while1 (function
    | '0' .. '9' -> true
    | _ -> false)
;;

let integer = str_integer >>| int_of_string

let varname_char = function
  | 'a' .. 'z' -> true
  | 'A' .. 'Z' -> true
  | '.' | '$' | '_' | '-' -> true
  | _ -> false
;;

let parse_word =
  let satisfy_char = function
    | a when varname_char a -> true
    | '0' .. '9' -> true
    | _ -> false
  in
  take_while1 satisfy_char
;;

let word str =
  parse_word
  >>= fun parsed -> if parsed = str then return str else fail "Parser error: Wrong word"
;;

let parse_primitive_type =
  whitespaces
  *> (char 'i' *> integer
      >>= (fun size -> return (Ast.Integer size))
      <|> (parse_word
           >>= function
           | "float" -> return Ast.Float
           | "ptr" -> return Ast.Pointer
           | _ -> fail "Parsing error: unknown primitive type"))
;;

let%expect_test _ =
  test_parse parse_primitive_type Ast.show_tp "i64";
  [%expect {| (Integer 64) |}]
;;

let%expect_test _ =
  test_parse parse_primitive_type Ast.show_tp "    i2   ";
  [%expect {| (Integer 2) |}]
;;

let%expect_test _ =
  test_parse parse_primitive_type Ast.show_tp "    c2   ";
  [%expect {| Error |}]
;;

let%expect_test _ =
  test_parse parse_primitive_type Ast.show_tp "float";
  [%expect {| Float |}]
;;

let parse_type =
  fix (fun parse_type ->
    let parse_array_type =
      whitespaces
      *> char '['
      *> whitespaces
      *> lift2
           (fun size t -> Ast.Arr (size, t))
           (integer <* whitespaces <* char 'x')
           (parse_type <* whitespaces <* char ']')
    and parse_vector_type =
      whitespaces
      *> char '<'
      *> whitespaces
      *> lift2
           (fun size t -> Ast.Vector (size, t))
           (integer <* whitespaces <* char 'x')
           (parse_primitive_type <* whitespaces <* char '>')
    and parse_structure_type =
      lift3
        (fun h tl _ -> Ast.Struct (List.cons h tl))
        (whitespaces *> char '{' *> whitespaces *> parse_type)
        (many (whitespaces *> char ',' *> whitespaces *> parse_type))
        (whitespaces *> char '}')
    in
    choice
      [ parse_primitive_type; parse_vector_type; parse_array_type; parse_structure_type ])
;;

let%expect_test _ =
  test_parse parse_type Ast.show_tp "<4xi32>";
  [%expect {| (Vector (4, (Integer 32))) |}]
;;

let%expect_test _ =
  test_parse parse_type Ast.show_tp "[4x[5x<4  x ptr>]]";
  [%expect {| (Arr (4, (Arr (5, (Vector (4, Pointer)))))) |}]
;;

let%expect_test _ =
  test_parse parse_type Ast.show_tp "{i32, i42, float}";
  [%expect {| (Struct [(Integer 32); (Integer 42); Float]) |}]
;;

let%expect_test _ =
  test_parse parse_type Ast.show_tp "{ [4x{i34}], float}";
  [%expect {| (Struct [(Arr (4, (Struct [(Integer 34)]))); Float]) |}]
;;

let%expect_test _ =
  test_parse parse_type Ast.show_tp "{ , float}";
  [%expect {| Error |}]
;;

let%expect_test _ =
  test_parse parse_type Ast.show_tp "{i32, i42,\n  ;some comment \n float}";
  [%expect {| (Struct [(Integer 32); (Integer 42); Float]) |}]
;;

let parse_func_ret_type = word "void" *> return Ast.Void <|> parse_type

let parse_name =
  lift2
    (fun first_char last_part -> String.make 1 first_char ^ last_part)
    (satisfy varname_char)
    parse_word
;;

let parse_unnamed_name =
  lift2
    (fun name _ -> name)
    str_integer
    (peek_char
     >>= fun c ->
     match c with
     | Some c when varname_char c -> fail "Parser error: deprecated name"
     | _ -> return ())
;;

let%expect_test _ =
  test_parse parse_unnamed_name (fun f -> f) "543554";
  [%expect {| 543554 |}]
;;

let%expect_test _ =
  test_parse parse_unnamed_name (fun f -> f) "543554d";
  [%expect {| Error |}]
;;

let%expect_test _ =
  test_parse parse_unnamed_name (fun f -> f) "54,";
  [%expect {| 54 |}]
;;

let _parse_variable_name c = whitespaces *> char c *> (parse_name <|> parse_unnamed_name)
let parse_local_variable_name = _parse_variable_name '%'
let parse_global_variable_name = _parse_variable_name '@'

let parse_local_variable =
  lift2
    (fun tp name : Ast.variable -> { tp; name })
    (whitespaces *> parse_type)
    parse_local_variable_name
;;

type func_annotation =
  { name : string
  ; returnType : Ast.tp
  ; parameters : Ast.variable list
  }
[@@deriving show { with_path = false }]

let parse_function_annotation =
  lift3
    (fun ret name args : func_annotation -> { returnType = ret; name; parameters = args })
    (whitespaces *> word "define" *> whitespaces *> parse_func_ret_type)
    (whitespaces *> char '@' *> parse_name)
    (whitespaces
     *> char '('
     *> sep_by (whitespaces *> char ',') (whitespaces *> parse_local_variable)
     <* whitespaces
     <* char ')')
;;

let%expect_test _ =
  test_parse
    parse_function_annotation
    show_func_annotation
    "define i32 @fac(i32 %0, i34 %1)";
  [%expect
    " \n\
    \    { name = \"fac\"; returnType = (Integer 32);\n\
    \      parameters =\n\
    \      [{ name = \"0\"; tp = (Integer 32) }; { name = \"1\"; tp = (Integer 34) }] } "]
;;
