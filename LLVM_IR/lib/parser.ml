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
      >>= (fun size -> return (Ast.TInteger size))
      <|> (parse_word
           >>= function
           | "float" -> return Ast.TFloat
           | "ptr" -> return Ast.TPointer
           | _ -> fail "Parsing error: unknown primitive type"))
;;

let%expect_test _ =
  test_parse parse_primitive_type Ast.show_tp "i64";
  [%expect {| (TInteger 64) |}]
;;

let%expect_test _ =
  test_parse parse_primitive_type Ast.show_tp "    i2   ";
  [%expect {| (TInteger 2) |}]
;;

let%expect_test _ =
  test_parse parse_primitive_type Ast.show_tp "    c2   ";
  [%expect {| Error |}]
;;

let%expect_test _ =
  test_parse parse_primitive_type Ast.show_tp "float";
  [%expect {| TFloat |}]
;;

let parse_main_type =
  fix (fun parse_type ->
    let parse_array_type =
      whitespaces
      *> char '['
      *> whitespaces
      *> lift2
           (fun size t -> Ast.TArr (size, t))
           (integer <* whitespaces <* char 'x')
           (parse_type <* whitespaces <* char ']')
    and parse_vector_type =
      whitespaces
      *> char '<'
      *> whitespaces
      *> lift2
           (fun size t -> Ast.TVector (size, t))
           (integer <* whitespaces <* char 'x')
           (parse_primitive_type <* whitespaces <* char '>')
    and parse_structure_type =
      lift3
        (fun h tl _ -> Ast.TStruct (List.cons h tl))
        (whitespaces *> char '{' *> whitespaces *> parse_type)
        (many (whitespaces *> char ',' *> whitespaces *> parse_type))
        (whitespaces *> char '}')
    in
    choice
      [ parse_primitive_type; parse_vector_type; parse_array_type; parse_structure_type ])
;;

let%expect_test _ =
  test_parse parse_main_type Ast.show_tp "<4xi32>";
  [%expect {| (TVector (4, (TInteger 32))) |}]
;;

let%expect_test _ =
  test_parse parse_main_type Ast.show_tp "[4x[5x<4  x ptr>]]";
  [%expect {| (TArr (4, (TArr (5, (TVector (4, TPointer)))))) |}]
;;

let%expect_test _ =
  test_parse parse_main_type Ast.show_tp "{i32, i42, float}";
  [%expect {| (TStruct [(TInteger 32); (TInteger 42); TFloat]) |}]
;;

let%expect_test _ =
  test_parse parse_main_type Ast.show_tp "{ [4x{i34}], float}";
  [%expect {| (TStruct [(TArr (4, (TStruct [(TInteger 34)]))); TFloat]) |}]
;;

let%expect_test _ =
  test_parse parse_main_type Ast.show_tp "{ , float}";
  [%expect {| Error |}]
;;

let%expect_test _ =
  test_parse parse_main_type Ast.show_tp "{i32, i42,\n  ;some comment \n float}";
  [%expect {| (TStruct [(TInteger 32); (TInteger 42); TFloat]) |}]
;;

let parse_func_ret_type = word "void" *> return Ast.TVoid <|> parse_main_type

let parse_named_name =
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

let parse_name = (parse_named_name <|> parse_unnamed_name)
let parse_local_variable_name = whitespaces *> char '%' *>  parse_name
let parse_global_variable_name = whitespaces *> char '@' *>  parse_name

let parse_local_variable =
  lift2
    (fun tp name -> (tp, Ast.LocalVar name))
    (whitespaces *> parse_main_type)
    parse_local_variable_name
;;

type func_annotation =
  { name : string
  ; returnType : Ast.tp
  ; parameters : (Ast.tp*Ast.variable) list
  }
[@@deriving show { with_path = false }]

let parse_function_annotation =
  lift3
    (fun ret name args : func_annotation -> { returnType = ret; name; parameters = args })
    (whitespaces *> word "define" *> whitespaces *> parse_func_ret_type)
    (whitespaces *> char '@' *> parse_named_name)
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
    {|
    { name = "fac"; returnType = (TInteger 32);
      parameters =
      [((TInteger 32), (LocalVar "0")); ((TInteger 34), (LocalVar "1"))] } |}]
;;

let parse_instruction: Ast.instruction t = return (Ast.Terminator (Ast.Br Ast.Const))
let parse_basic_block_label = 
  whitespaces *> parse_name <* whitespaces <* char ':'

let parse_basic_block_body = 
  many1 parse_instruction

let parse_basic_block = 
  lift2
  (fun name instructions: Ast.basic_block -> {name; instructions})
  parse_basic_block_label
  parse_basic_block_body

let parse_function_body = 
  whitespaces *> char '{' *> whitespaces 