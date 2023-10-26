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

let whitespaces = skip_while whitespace

let str_integer =
  take_while1 (function
    | '0' .. '9' -> true
    | _ -> false)
;;

let integer = str_integer >>| int_of_string

let parse_word =
  let satisfy_char = function
    | 'a' .. 'z' -> true
    | 'A' .. 'Z' -> true
    | '0' .. '9' -> true
    | '.' | '$' | '_' | '-' -> true
    | _ -> false
  in
  take_while1 satisfy_char
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
    (fun h tl _ -> (Ast.Struct (List.cons h tl)))
     (whitespaces *> char '{' *> whitespaces *> parse_type )
     (many ( whitespaces *> char ',' *> whitespaces *> parse_type) )
   ( whitespaces *> char '}')
  
  
  in
    choice [ parse_primitive_type; parse_vector_type; parse_array_type; parse_structure_type ])
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
  test_parse parse_type Ast.show_tp "<4xi32>";
  [%expect {| (Vector (4, (Integer 32))) |}]
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

