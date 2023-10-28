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

let parse_additional_type =
  whitespaces
  *> choice
       [ parse_main_type
       ; (parse_word
          >>= function
          | "void" -> return Ast.TVoid
          | "label" -> return Ast.TLabel
          | _ -> fail "Parsing error: unknown type")
       ]
;;

let parse_named_name =
  lift2
    (fun first_char last_part -> String.make 1 first_char ^ last_part)
    (satisfy varname_char)
    (parse_word <|> return "")
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

let parse_name = parse_named_name <|> parse_unnamed_name

let parse_local_variable =
  whitespaces *> char '%' *> parse_name >>| fun name -> Ast.LocalVar name
;;

let parse_global_variable =
  whitespaces *> char '@' *> parse_name >>| fun name -> Ast.GlobalVar name
;;


let rec parse_const tp =
  let rec makeList i arg = if i = 0 then [] else arg :: makeList (i - 1) arg in
  match tp with
  | Ast.TVoid -> return Ast.CVoid
  | Ast.TInteger size -> parse_const_integer size
  | Ast.TFloat -> parse_const_float
  | Ast.TPointer -> parse_const_pointer
  | Ast.TVector (size, vtp) ->
    parse_const_aggregate_type '<' '>' (makeList size vtp)
    >>= fun lst -> return (Ast.CVector lst)
  | Ast.TArr (size, atp) ->
    parse_const_aggregate_type '[' ']' (makeList size atp)
    >>= fun lst -> return (Ast.CArr lst)
  | Ast.TStruct slst ->
    parse_const_aggregate_type '{' '}' slst >>= fun lst -> return (Ast.CStruct lst)
  | _ -> fail "Parser error: Get unknown type"

and parse_const_aggregate_type ob cb tp_list =
  char ob
  *> whitespaces
  *> sep_by
       (whitespaces *> char ',')
       (whitespaces *> parse_main_type
        >>= fun tp -> whitespaces *> parse_const tp >>| fun c -> tp, c)
  <* whitespaces
  <* char cb
  >>= fun lst ->
  let readed_types =
    List.map
      (function
       | tp, _ -> tp)
      lst
  in
  let consts =
    List.map
      (function
       | _, c -> c)
      lst
  in
  if List.equal Ast.equal_tp readed_types tp_list
  then return consts
  else fail "Parser error: constant vector/array length mismatch"

and parse_const_array size vector_tp =
  char '<'
  *> whitespaces
  *> sep_by
       (whitespaces *> char ',')
       (whitespaces *> parse_main_type
        >>= fun tp ->
        whitespaces
        *>
        if vector_tp = tp
        then parse_const tp
        else fail "Parser error: constant vector type mismatch")
  <* whitespaces
  <* char '>'
  >>= fun lst ->
  if List.length lst = size
  then return (Ast.CVector lst)
  else fail "Parser error: constant vector length mismatch"

and parse_const_pointer =
  (*TODO: at type check step check that glob exist*)
  parse_global_variable >>| fun var -> Ast.CPointer (Ast.PointerGlob var)

and parse_const_float =
  (*TODO: other floats *)
  parse_word
  >>= fun str ->
  match Float.of_string_opt str with
  | Some res -> return (Ast.CFloat res)
  | None -> fail "Can't parse float"

and parse_const_integer size =
  (*TODO: other integer types *)
  integer
  >>| fun value ->
  Ast.CInteger
    ( size
    , let modulo x y =
        let result = x mod y in
        if result >= 0 then result else result + y
      in
      modulo value (Int.shift_left 1 size) )
;;

let parse_value tp =
  choice
    [ (parse_local_variable >>| fun var -> Ast.FromVariable (var, tp))
    ; (parse_const tp >>| fun const -> Ast.Const const)
    ]
;;

let%expect_test _ =
  test_parse
    (parse_additional_type <* whitespaces >>= fun f -> parse_value f)
    Ast.show_value
    "void ";
  [%expect {| (Const CVoid) |}]
;;

let%expect_test _ =
  test_parse
    (parse_additional_type <* whitespaces >>= fun f -> parse_value f)
    Ast.show_value
    "float 2.3 ";
  [%expect {| (Const (CFloat 2.3)) |}]
;;

let%expect_test _ =
  test_parse
    (parse_additional_type <* whitespaces >>= fun f -> parse_value f)
    Ast.show_value
    "i1 432 ";
  [%expect {| (Const (CInteger (1, 0))) |}]
;;

let%expect_test _ =
  test_parse
    (parse_additional_type <* whitespaces >>= fun f -> parse_value f)
    Ast.show_value
    "ptr @G ";
  [%expect {| (Const (CPointer (PointerGlob (GlobalVar "G")))) |}]
;;

let%expect_test _ =
  test_parse
    (parse_additional_type <* whitespaces >>= fun f -> parse_value f)
    Ast.show_value
    "{i32, float} { i32 4, float 17.0} ";
  [%expect {| (Const (CStruct [(CInteger (32, 4)); (CFloat 17.)])) |}]
;;

let%expect_test _ =
  test_parse
    (parse_additional_type <* whitespaces >>= fun f -> parse_value f)
    Ast.show_value
    "<4 x i32> < i32 42, i32 11, i32 74, i32 100 > ";
  [%expect
    {|
    (Const
       (CVector
          [(CInteger (32, 42)); (CInteger (32, 11)); (CInteger (32, 74));
            (CInteger (32, 100))])) |}]
;;

let%expect_test _ =
  test_parse
    (parse_additional_type <* whitespaces >>= fun f -> parse_value f)
    Ast.show_value
    "[2 x [2 x i32]] [[2 x i32] [i32 1, i32 3], [2 x i32] [i32 2, i32 4]] ";
  [%expect
    {|
    (Const
       (CArr
          [(CArr [(CInteger (32, 1)); (CInteger (32, 3))]);
            (CArr [(CInteger (32, 2)); (CInteger (32, 4))])])) |}]
;;

let%expect_test _ =
  test_parse
    (parse_additional_type <* whitespaces >>= fun f -> parse_value f)
    Ast.show_value
    "{i32, int43} { i32 4, float 17.0} ";
  [%expect {| Error |}]
;;

let%expect_test _ =
  test_parse
    (parse_additional_type <* whitespaces >>= fun f -> parse_value f)
    Ast.show_value
    "<4 x i32> < i32 42, i32 11, i32 100 > ";
  [%expect {| Error |}]
;;

let%expect_test _ =
  test_parse
    (parse_additional_type <* whitespaces >>= fun f -> parse_value f)
    Ast.show_value
    "[2 x [2 x i32]] [[2 x i32] [i32 1, i31 3], [2 x i32] [i32 2, i32 4]] ";
  [%expect {| Error |}]
;;

let%expect_test _ =
  test_parse
    (parse_additional_type <* whitespaces >>= fun f -> parse_value f)
    Ast.show_value
    "i32 %12 ";
  [%expect {| (FromVariable ((LocalVar "12"), (TInteger 32))) |}]
;;

type func_annotation =
  { self : Ast.variable
  ; tp : Ast.tp
  ; parameters : Ast.variable list
  }
[@@deriving show { with_path = false }]

let parse_function_annotation =
  lift3
    (fun ret name args ->
      let arg_types =
        List.map
          (function
           | tp, _ -> tp)
          args
      in
      let arg_vars =
        List.map
          (function
           | _, var -> var)
          args
      in
      { self = name; parameters = arg_vars; tp = Ast.TFunc (ret, arg_types) })
    (whitespaces *> word "define" *> whitespaces *> parse_additional_type)
    parse_global_variable
    (whitespaces
     *> char '('
     *> sep_by
          (whitespaces *> char ',')
          (whitespaces
           *> lift2 (fun tp name -> tp, name) parse_main_type parse_local_variable)
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
    { self = (GlobalVar "fac");
      tp = (TFunc ((TInteger 32), [(TInteger 32); (TInteger 34)]));
      parameters = [(LocalVar "0"); (LocalVar "1")] } |}]
;;

let parse_instruction : Ast.instruction t =
  return (Ast.Terminator (Ast.Br (Ast.Const Ast.CVoid)))
;;

let parse_basic_block_variable =
  whitespaces *> parse_name <* whitespaces <* char ':' >>| fun name -> Ast.LocalVar name
;;

let parse_basic_block_body = many parse_instruction

let parse_basic_block =
  lift2
    (fun self instructions -> self, instructions)
    parse_basic_block_variable
    parse_basic_block_body
;;

let parse_function_body = whitespaces *> char '{' *> whitespaces
