(** Copyright 2023-2024, Arseniy Baytenov *)

(** SPDX-License-Identifier: MIT *)

open Angstrom
open Ast

let kws =
  [ "CREATE"
  ; "MATCH"
  ; "WITH"
  ; "WHERE"
  ; "DELETE"
  ; "DETACH"
  ; "NODETACH"
  ; "RETURN"
  ; "IS"
  ; "NOT"
  ; "NULL"
  ; "AND"
  ; "OR"
  ; "XOR"
  ; "TRUE"
  ; "FALSE"
  ; "ORDER"
  ; "BY"
  ; "ASC"
  ; "DESC"
  ; "CONTAINS"
  ; "STARTS"
  ; "ENDS"
  ; "WITH"
  ; "AS"
  ]
;;

let uc = String.uppercase_ascii

let is_letter = function
  | 'a' .. 'z' | 'A' .. 'Z' -> true
  | _ -> false
;;

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false
;;

let skip_spaces p =
  skip_while (function
    | ' ' | '\t' | '\n' | '\r' -> true
    | _ -> false)
  *> p
;;

let skip_spaces_after p =
  p
  <* skip_while (function
    | ' ' | '\t' | '\n' | '\r' -> true
    | _ -> false)
;;

let check_after p cond =
  p
  <* (satisfy (fun c -> not @@ cond c)
      <|> return @@ Char.chr 0
      >>= fun c ->
      if c != Char.chr 0 then fail "Incorrect symbol" else return @@ Char.chr 0)
;;

let parens p = skip_spaces (char '(') *> p <* skip_spaces (char ')')
let braces p = skip_spaces (char '{') *> p <* skip_spaces (char '}')
let sq_brackets p = skip_spaces (char '[') *> p <* skip_spaces (char ']')

let chainl1 e op =
  let rec go acc = lift2 (fun f x -> f acc x) op e >>= go <|> return acc in
  e >>= fun init -> go init
;;

let rec chainr1 e op = e >>= fun a -> op >>= (fun f -> chainr1 e op >>| f a) <|> return a

let un_chainl1 e op =
  lift2 (fun e ops -> List.fold_left (fun e f -> f e) e ops) e (many op)
;;

let un_chainr1 e op =
  lift2 (fun ops e -> List.fold_right (fun f e -> f e) ops e) (many op) e
;;

let parse p str = parse_string ~consume:All (skip_spaces_after p) str

let name =
  let name1 =
    lift2
      (fun s1 s2 -> s1 ^ s2)
      (take_while1 is_letter)
      (take_while (fun c -> is_letter c || is_digit c || c = '_'))
    >>= fun n -> if List.mem (uc n) kws then fail "Name cannot be keyword" else return n
  in
  let name2 =
    lift3
      (fun s1 s2 s3 -> s1 ^ s2 ^ s3)
      (char '`' *> return "`")
      (take_while1 (( <> ) '`'))
      (char '`' *> return "`")
  in
  name2 <|> name1
;;

let const =
  let sign = choice [ char '-' *> return "-"; string "" *> return "" ] in
  let int64 =
    check_after
      (lift2
         (fun sign num -> Int64 (Int64.of_string (sign ^ num)))
         sign
         (take_while1 is_digit))
      (fun c -> not @@ is_letter c)
  in
  let float =
    check_after
      (lift4
         (fun sign int dot fract -> Float (float_of_string (sign ^ int ^ dot ^ fract)))
         sign
         (take_while1 is_digit)
         (char '.' *> return ".")
         (take_while1 is_digit))
      (fun c -> not @@ is_letter c)
  in
  let string =
    let content_while_not c = lift (fun s -> String s) (take_while (( <> ) c)) in
    let string c = char c *> content_while_not c <* char c in
    choice [ string '\"'; string '\'' ]
  in
  let bool_or_null =
    check_after
      (take_while is_letter
       >>= fun l ->
       match uc l with
       | "TRUE" -> return @@ Bool true
       | "FALSE" -> return @@ Bool false
       | "NULL" -> return Null
       | _ -> fail "")
      (fun c -> not @@ is_digit c)
  in
  skip_spaces @@ lift (fun c -> Const c) (choice [ string; float; int64; bool_or_null ])
;;

let var = lift (fun v -> Var v) (skip_spaces name)

let property =
  lift2
    (fun s1 s2 -> Property (s1, s2))
    (skip_spaces name)
    (skip_spaces (skip (fun c -> c = '.') *> skip_spaces name))
;;

let uminus =
  check_after
    (skip_spaces (char '-') *> return (fun e -> Un_op (Minus, e)))
    (fun c -> not @@ is_digit c)
;;

let uis_null =
  check_after
    (skip_spaces
       (lift2
          (fun s1 s2 -> s1, s2)
          (take_while is_letter)
          (skip_spaces (take_while is_letter))
        >>= fun (s1, s2) ->
        if (uc s1, uc s2) <> ("IS", "NULL")
        then fail "IS_NULL parse fail"
        else return (fun e -> Un_op (IS_NULL, e))))
    (fun c -> not @@ is_digit c)
;;

let uis_not_null =
  check_after
    (skip_spaces
       (lift3
          (fun s1 s2 s3 -> s1, s2, s3)
          (take_while is_letter)
          (skip_spaces (take_while is_letter))
          (skip_spaces (take_while is_letter))
        >>= fun (s1, s2, s3) ->
        if (uc s1, uc s2, uc s3) <> ("IS", "NOT", "NULL")
        then fail "IS_NOT_NULL parse fail"
        else return (fun e -> Un_op (IS_NOT_NULL, e))))
    (fun c -> not @@ is_digit c)
;;

let unot =
  check_after
    (skip_spaces
       (take_while is_letter
        >>= fun s ->
        if uc s <> "NOT" then fail "NOT parse fail" else return (fun e -> Un_op (NOT, e))
       ))
    (fun c -> not @@ is_digit c)
;;

let bcontains =
  check_after
    (skip_spaces
       (take_while is_letter
        >>= fun op ->
        match uc op with
        | "CONTAINS" -> return (fun e1 e2 -> Bin_op (CONTAINS, e1, e2))
        | _ -> fail ""))
    (fun c -> not @@ is_digit c)
;;

let bstarts_ends_with =
  check_after
    (skip_spaces
       (lift2
          (fun s1 s2 -> s1, s2)
          (take_while is_letter)
          (skip_spaces (take_while is_letter))
        >>= fun (s1, s2) ->
        match uc s1, uc s2 with
        | "STARTS", "WITH" -> return (fun e1 e2 -> Bin_op (STARTS_WITH, e1, e2))
        | "ENDS", "WITH" -> return (fun e1 e2 -> Bin_op (ENDS_WITH, e1, e2))
        | _ -> fail ""))
    (fun c -> not @@ is_digit c)
;;

let bcaret = skip_spaces (char '^') *> return (fun e1 e2 -> Bin_op (Caret, e1, e2))
let basterisk = skip_spaces (char '*') *> return (fun e1 e2 -> Bin_op (Asterisk, e1, e2))
let bslash = skip_spaces (char '/') *> return (fun e1 e2 -> Bin_op (Slash, e1, e2))
let bpercent = skip_spaces (char '%') *> return (fun e1 e2 -> Bin_op (Percent, e1, e2))
let bplus = skip_spaces (char '+') *> return (fun e1 e2 -> Bin_op (Plus, e1, e2))
let bminus = skip_spaces (char '-') *> return (fun e1 e2 -> Bin_op (Minus, e1, e2))

let band =
  check_after
    (skip_spaces
       (take_while is_letter
        >>= fun s ->
        if uc s <> "AND"
        then fail "AND parse fail"
        else return (fun e1 e2 -> Bin_op (AND, e1, e2))))
    (fun c -> not @@ is_digit c)
;;

let bor =
  check_after
    (skip_spaces
       (take_while is_letter
        >>= fun s ->
        if uc s <> "OR"
        then fail "OR parse fail"
        else return (fun e1 e2 -> Bin_op (OR, e1, e2))))
    (fun c -> not @@ is_digit c)
;;

let bxor =
  check_after
    (skip_spaces
       (take_while is_letter
        >>= fun s ->
        if uc s <> "XOR"
        then fail "XOR parse fail"
        else return (fun e1 e2 -> Bin_op (XOR, e1, e2))))
    (fun c -> not @@ is_digit c)
;;

let leq =
  skip_spaces (char '=')
  *> return (fun e1 e2 ->
    match e2 with
    | List_op (e, ls) -> List_op (e1, (Eq, e) :: ls)
    | _ -> List_op (e1, (Eq, e2) :: []))
;;

let lneq =
  skip_spaces (string "<>")
  *> return (fun e1 e2 ->
    match e2 with
    | List_op (e, ls) -> List_op (e1, (NEq, e) :: ls)
    | _ -> List_op (e1, (NEq, e2) :: []))
;;

let lless =
  skip_spaces (char '<')
  *> return (fun e1 e2 ->
    match e2 with
    | List_op (e, ls) -> List_op (e1, (Less, e) :: ls)
    | _ -> List_op (e1, (Less, e2) :: []))
;;

let lgreater =
  skip_spaces (char '>')
  *> return (fun e1 e2 ->
    match e2 with
    | List_op (e, ls) -> List_op (e1, (Greater, e) :: ls)
    | _ -> List_op (e1, (Greater, e2) :: []))
;;

let lleq =
  skip_spaces (string "<=")
  *> return (fun e1 e2 ->
    match e2 with
    | List_op (e, ls) -> List_op (e1, (LEq, e) :: ls)
    | _ -> List_op (e1, (LEq, e2) :: []))
;;

let lgeq =
  skip_spaces (string ">=")
  *> return (fun e1 e2 ->
    match e2 with
    | List_op (e, ls) -> List_op (e1, (GEq, e) :: ls)
    | _ -> List_op (e1, (GEq, e2) :: []))
;;

let expr =
  fix (fun expr ->
    let factor = choice [ parens expr; const; property; var ] in
    let ss_comp_op = chainl1 factor (bcontains <|> bstarts_ends_with) in
    let null_or_not_null = un_chainl1 ss_comp_op (uis_null <|> uis_not_null) in
    let minus = un_chainr1 null_or_not_null uminus in
    let caret = chainl1 minus bcaret in
    let asterisk_slash_percent = chainl1 caret (choice [ basterisk; bslash; bpercent ]) in
    let plus_minus = chainl1 asterisk_slash_percent (bplus <|> bminus) in
    let list_op =
      chainr1 plus_minus (choice [ leq; lneq; lleq; lgeq; lless; lgreater ])
    in
    let unot = un_chainr1 list_op unot in
    let bxor = chainl1 unot bxor in
    let band = chainl1 bxor band in
    let bor = chainl1 band bor in
    bor)
;;

let labels =
  let label = skip_spaces (char ':') *> skip_spaces name in
  many label
;;

let properties =
  let property =
    lift2
      (fun n e -> n, e)
      (skip_spaces name)
      (skip_spaces (char ':' *> skip_spaces expr))
  in
  choice
    [ braces @@ return []
    ; braces
        (lift2 (fun p ps -> p :: ps) property (many (skip_spaces (char ',' *> property))))
    ; return []
    ]
;;

let pattern = lift2 (fun ls ps -> ls, ps) labels properties

let path =
  let named_patt =
    lift2
      (fun n p -> n, p)
      (skip_spaces name >>= (fun n -> return @@ Some n) <|> return None)
      pattern
  in
  let rel =
    choice
      [ skip_spaces
          (string "<-" *> (sq_brackets named_patt <|> return (None, ([], [])))
           <* skip_spaces @@ char '-'
           >>= fun (n, p) -> return (n, p, Left))
      ; skip_spaces
          (char '-' *> (sq_brackets named_patt <|> return (None, ([], [])))
           <* skip_spaces @@ string "->"
           >>= fun (n, p) -> return (n, p, Right))
      ; skip_spaces
          (char '-' *> (sq_brackets named_patt <|> return (None, ([], [])))
           <* skip_spaces @@ char '-'
           >>= fun (n, p) -> return (n, p, No))
      ]
  in
  let node = parens named_patt in
  lift2 (fun n rns -> n, rns) node (many (lift2 (fun r n -> r, n) rel node))
;;

let paths = lift2 (fun p ps -> p :: ps) path (many (skip_spaces (char ',') *> path))

let where =
  check_after
    (skip_spaces
       (take_while is_letter
        >>= fun s ->
        match uc s with
        | "WHERE" -> return ()
        | _ -> fail ""))
    (fun c -> not @@ is_digit c)
  *> lift2 (fun c cs -> c :: cs) expr (many (skip_spaces (char ',') *> expr))
  <|> return []
;;

let star = skip_spaces (char '*') *> (return @@ Some All) <|> return None

let order_by =
  let order_by_cond =
    check_after
      (skip_spaces
         (take_while is_letter
          >>= fun c ->
          match uc c with
          | "ASC" -> return Asc
          | "DESC" -> return Desc
          | _ -> fail ""))
      (fun c -> not @@ is_digit c)
    <|> return Asc
  in
  let part =
    lift2
      (fun es c -> es, c)
      (lift2 (fun e es -> e :: es) expr (many (skip_spaces (char ',') *> expr)))
      order_by_cond
  in
  check_after
    (skip_spaces
       (lift2
          (fun s1 s2 -> uc s1, uc s2)
          (take_while is_letter)
          (skip_spaces (take_while is_letter))
        >>= function
        | "ORDER", "BY" -> return ()
        | _ -> fail ""))
    (fun c -> not @@ is_digit c)
  *> lift2 (fun p ps -> p :: ps) part (many (skip_spaces (char ',') *> part))
  <|> return []
;;

let kwas =
  check_after
    (skip_spaces
       (take_while is_letter
        >>= fun kwas ->
        match uc kwas with
        | "AS" -> return ()
        | _ -> fail ""))
    (fun c -> not @@ is_digit c)
;;

let cwith =
  let alias = lift2 (fun e n -> e, n) expr (kwas *> skip_spaces name) in
  let aliases =
    lift2 (fun al als -> al :: als) alias (many (skip_spaces (char ',') *> alias))
  in
  check_after
    (skip_spaces
       (take_while is_letter
        >>= fun c ->
        match uc c with
        | "WITH" -> return ()
        | _ -> fail ""))
    (fun c -> not @@ is_digit c)
  *> lift3
       (fun (s_opt, als) o_b wh -> s_opt, als, o_b, wh)
       (star
        >>= function
        | Some All ->
          skip_spaces (char ',') *> aliases
          <|> return []
          >>= fun als -> return (Some All, als)
        | None -> aliases >>= fun als -> return (None, als))
       order_by
       where
;;

let creturn =
  let alias =
    lift2
      (fun e n -> e, n)
      expr
      (kwas *> skip_spaces name >>= (fun n -> return @@ Some n) <|> return None)
  in
  let aliases =
    lift2 (fun al als -> al :: als) alias (many (skip_spaces (char ',') *> alias))
  in
  check_after
    (skip_spaces
       (take_while is_letter
        >>= fun c ->
        match uc c with
        | "RETURN" -> return ()
        | _ -> fail ""))
    (fun c -> not @@ is_digit c)
  *> lift2
       (fun (s_opt, als) o_b -> s_opt, als, o_b)
       (star
        >>= function
        | Some All ->
          skip_spaces (char ',') *> aliases
          <|> return []
          >>= fun als -> return (Some All, als)
        | None -> aliases >>= fun als -> return (None, als))
       order_by
;;

let cmatch =
  check_after
    (skip_spaces
       (take_while is_letter
        >>= fun c ->
        match uc c with
        | "MATCH" -> return ()
        | _ -> fail ""))
    (fun c -> not @@ is_digit c)
  *> lift2 (fun ps wh -> ps, wh) paths where
;;

let ccreate =
  check_after
    (skip_spaces
       (take_while is_letter
        >>= fun c ->
        match uc c with
        | "CREATE" -> return ()
        | _ -> fail ""))
    (fun c -> not @@ is_digit c)
  *> paths
;;

let cdelete =
  let delete_attr =
    check_after
      (skip_spaces
         (take_while is_letter
          >>= fun c ->
          match uc c with
          | "DETACH" -> return Detach
          | "NODETACH" -> return Nodetach
          | _ -> fail ""))
      (fun c -> not @@ is_digit c)
    <|> return Nodetach
  in
  let dname = parens (skip_spaces (skip_spaces_after name)) <|> skip_spaces name in
  let names =
    lift2 (fun n ns -> n :: ns) dname (many (skip_spaces (char ',') *> dname))
  in
  delete_attr
  >>= fun attr ->
  check_after
    (skip_spaces
       (take_while is_letter
        >>= fun c ->
        match uc c with
        | "DELETE" -> return ()
        | _ -> fail ""))
    (fun c -> not @@ is_digit c)
  *> lift (fun ns -> attr, ns) names
;;

type cd =
  | C of path list
  | D of delete_attr * name list

let request =
  let safety c_opt =
    c_opt
    >>= function
    | Some c -> return c
    | None -> fail "Incomplete request: there is expected clause."
  in
  let r =
    many cwith
    >>= fun ws ->
    creturn
    >>= fun (s_opt, als, o_b) ->
    return
    @@ List.fold_right
         (fun (s_opt, als, o_b, wh) c -> With (s_opt, als, o_b, wh, c))
         ws
         (Return (s_opt, als, o_b))
  in
  let c = many cwith >>= fun ws -> ccreate >>= fun ps -> return (C ps, ws) in
  let d =
    many cwith >>= fun ws -> cdelete >>= fun (attr, ns) -> return (D (attr, ns), ws)
  in
  let cds =
    many (c <|> d)
    >>= fun cdwss ->
    r
    >>= (fun r -> return @@ Some r)
    <|> return None
    >>= fun r_opt ->
    return
    @@ List.fold_right
         (fun (cd, ws) c_opt ->
           Some
             (List.fold_right
                (fun (s_opt, als, o_b, wh) c -> With (s_opt, als, o_b, wh, c))
                ws
                (match cd with
                 | C ps -> Create (ps, c_opt)
                 | D (attr, ns) -> Delete (attr, ns, c_opt))))
         cdwss
         r_opt
  in
  many cwith
  >>= (fun ws ->
        cmatch
        >>= fun (ps, wh) ->
        safety cds
        >>= fun c ->
        return
        @@ List.fold_right
             (fun (s_opt, als, o_b, wh) c -> With (s_opt, als, o_b, wh, c))
             ws
             (Match (ps, wh, c)))
  <|> safety cds
;;

let parse_request = parse request

let parse_and_print s =
  match parse_request s with
  | Ok v -> Stdlib.Format.printf "%a" pp_clause v
  | Error msg -> Stdlib.Format.printf "%a" pp_name msg
;;

let%expect_test "Simple expr" =
  parse_and_print {| RETURN 120.607 + 400 / 10 ^ 5 |};
  [%expect
    {|
    (Return (None,
       [((Bin_op (Plus, (Const (Float 120.607)),
            (Bin_op (Slash, (Const (Int64 400L)),
               (Bin_op (Caret, (Const (Int64 10L)), (Const (Int64 5L))))))
            )),
         None)],
       [])) |}]
;;

let%expect_test "Simple expr with unary minus" =
  parse_and_print {| RETURN - 120.607 + -400 / 10 ^ -x |};
  [%expect
    {|
    (Return (None,
       [((Bin_op (Plus, (Un_op (Minus, (Const (Float 120.607)))),
            (Bin_op (Slash, (Const (Int64 -400L)),
               (Bin_op (Caret, (Const (Int64 10L)), (Un_op (Minus, (Var "x")))))
               ))
            )),
         None)],
       [])) |}]
;;

let%expect_test "Multiple sequential comparison operators" =
  parse_and_print {| RETURN 1+-1 = 0 <> 10 >= 5*1 <= 5/1 > - -4 <> null |};
  [%expect
    {|
    (Return (None,
       [((List_op ((Bin_op (Plus, (Const (Int64 1L)), (Const (Int64 -1L)))),
            [(Eq, (Const (Int64 0L))); (NEq, (Const (Int64 10L)));
              (GEq, (Bin_op (Asterisk, (Const (Int64 5L)), (Const (Int64 1L)))));
              (LEq, (Bin_op (Slash, (Const (Int64 5L)), (Const (Int64 1L)))));
              (Greater, (Un_op (Minus, (Const (Int64 -4L)))));
              (NEq, (Const Null))]
            )),
         None)],
       [])) |}]
;;

let%expect_test "Null check operators" =
  parse_and_print {| RETURN 4 = (4 + a is null IS NOT NULL) is null |};
  [%expect
    {|
    (Return (None,
       [((List_op ((Const (Int64 4L)),
            [(Eq,
              (Un_op (IS_NULL,
                 (Bin_op (Plus, (Const (Int64 4L)),
                    (Un_op (IS_NOT_NULL, (Un_op (IS_NULL, (Var "a")))))))
                 )))
              ]
            )),
         None)],
       [])) |}]
;;

let%expect_test "Boolean oparators and literals" =
  parse_and_print
    {| RETURN not 1 = 1 or 4 = a + 23.0 and false xor not true or "Hello" = a + "llo" |};
  [%expect
    {|
    (Return (None,
       [((Bin_op (OR,
            (Bin_op (OR,
               (Un_op (NOT,
                  (List_op ((Const (Int64 1L)), [(Eq, (Const (Int64 1L)))])))),
               (Bin_op (AND,
                  (List_op ((Const (Int64 4L)),
                     [(Eq, (Bin_op (Plus, (Var "a"), (Const (Float 23.)))))])),
                  (Bin_op (XOR, (Const (Bool false)),
                     (Un_op (NOT, (Const (Bool true))))))
                  ))
               )),
            (List_op ((Const (String "Hello")),
               [(Eq, (Bin_op (Plus, (Var "a"), (Const (String "llo")))))]))
            )),
         None)],
       [])) |}]
;;

let%expect_test "String specific comparison operators" =
  parse_and_print
    {| RETURN "hola" STARTS WITH "ho", "hola" ENDS WITH "la", "hola" CONTAINS "ola" |};
  [%expect
    {|
    (Return (None,
       [((Bin_op (STARTS_WITH, (Const (String "hola")), (Const (String "ho")))),
         None);
         ((Bin_op (ENDS_WITH, (Const (String "hola")), (Const (String "la")))),
          None);
         ((Bin_op (CONTAINS, (Const (String "hola")), (Const (String "ola")))),
          None)
         ],
       [])) |}]
;;

let%expect_test "RETURN clause test1" =
  parse_and_print {| RETURN * |};
  [%expect {| (Return ((Some All), [], [])) |}]
;;

let%expect_test "RETURN clause test2" =
  parse_and_print {| RETURN *, 4, 5 as five |};
  [%expect
    {|
    (Return ((Some All),
       [((Const (Int64 4L)), None); ((Const (Int64 5L)), (Some "five"))],
       [])) |}]
;;

let%expect_test "RETURN clause test3" =
  parse_and_print {| RETURN 4 as four, 5 as five |};
  [%expect
    {|
    (Return (None,
       [((Const (Int64 4L)), (Some "four")); ((Const (Int64 5L)), (Some "five"))],
       [])) |}]
;;

let%expect_test "WITH clause test1" =
  parse_and_print {| WITH * RETURN * |};
  [%expect {| (With ((Some All), [], [], [], (Return ((Some All), [], [])))) |}]
;;

let%expect_test "WITH clause test2" =
  parse_and_print {| WITH *, 4 as four, 5 as five RETURN * |};
  [%expect
    {|
    (With ((Some All),
       [((Const (Int64 4L)), "four"); ((Const (Int64 5L)), "five")], [],
       [], (Return ((Some All), [], [])))) |}]
;;

let%expect_test "WITH clause test3" =
  parse_and_print {| WITH 4 as four RETURN * |};
  [%expect
    {|
    (With (None, [((Const (Int64 4L)), "four")], [], [],
       (Return ((Some All), [], [])))) |}]
;;

let%expect_test "MATCH clause test1" =
  parse_and_print {|
  WITH *
  WITH 4 as four, 5 as five
  MATCH ()
  RETURN * |};
  [%expect
    {|
    (With ((Some All), [], [], [],
       (With (None, [((Const (Int64 4L)), "four"); ((Const (Int64 5L)), "five")],
          [], [],
          (Match ([((None, ([], [])), [])], [], (Return ((Some All), [], []))))))
       )) |}]
;;

let%expect_test "MATCH clause test2" =
  parse_and_print {|
  WITH 5 as f
  MATCH ()--(), ()-[]-()-->()<--()
  RETURN * |};
  [%expect
    {|
    (With (None, [((Const (Int64 5L)), "f")], [], [],
       (Match (
          [((None, ([], [])), [((None, ([], []), No), (None, ([], [])))]);
            ((None, ([], [])),
             [((None, ([], []), No), (None, ([], [])));
               ((None, ([], []), Right), (None, ([], [])));
               ((None, ([], []), Left), (None, ([], [])))])
            ],
          [], (Return ((Some All), [], []))))
       )) |}]
;;

let%expect_test "MATCH clause test3" =
  parse_and_print
    {|
  MATCH (:L1:L2{name:"Sasha"})-[r1:L1{d:5, f:6}]-(n1{})-[{}]->(n2)
  RETURN * |};
  [%expect
    {|
    (Match (
       [((None, (["L1"; "L2"], [("name", (Const (String "Sasha")))])),
         [(((Some "r1"),
            (["L1"], [("d", (Const (Int64 5L))); ("f", (Const (Int64 6L)))]), No),
           ((Some "n1"), ([], [])));
           ((None, ([], []), Right), ((Some "n2"), ([], [])))])
         ],
       [], (Return ((Some All), [], [])))) |}]
;;

let%expect_test "CREATE clause test1" =
  parse_and_print
    {|
  WITH *
  WITH 4 as four, 5 as five
  CREATE (), ()-->(), ()<-[]-()-->()<--()
  |};
  [%expect
    {|
    (With ((Some All), [], [], [],
       (With (None, [((Const (Int64 4L)), "four"); ((Const (Int64 5L)), "five")],
          [], [],
          (Create (
             [((None, ([], [])), []);
               ((None, ([], [])), [((None, ([], []), Right), (None, ([], [])))]);
               ((None, ([], [])),
                [((None, ([], []), Left), (None, ([], [])));
                  ((None, ([], []), Right), (None, ([], [])));
                  ((None, ([], []), Left), (None, ([], [])))])
               ],
             None))
          ))
       )) |}]
;;

let%expect_test "CREATE clause test2" =
  parse_and_print
    {|
  WITH 5 as f
  CREATE (:L1:L2{name:"Sasha"})<-[r1:L1{d:5, f:6}]-(n1{})-[{}]->(n2)
  |};
  [%expect
    {|
    (With (None, [((Const (Int64 5L)), "f")], [], [],
       (Create (
          [((None, (["L1"; "L2"], [("name", (Const (String "Sasha")))])),
            [(((Some "r1"),
               (["L1"], [("d", (Const (Int64 5L))); ("f", (Const (Int64 6L)))]),
               Left),
              ((Some "n1"), ([], [])));
              ((None, ([], []), Right), ((Some "n2"), ([], [])))])
            ],
          None))
       )) |}]
;;

let%expect_test "DELETE clause test" =
  parse_and_print
    {|
  WITH *
  WITH 4 as four, 5 as five
  MATCH (n1)-[r]-(n2)
  DELETE n1, (r), n2
  |};
  [%expect
    {|
    (With ((Some All), [], [], [],
       (With (None, [((Const (Int64 4L)), "four"); ((Const (Int64 5L)), "five")],
          [], [],
          (Match (
             [(((Some "n1"), ([], [])),
               [(((Some "r"), ([], []), No), ((Some "n2"), ([], [])))])],
             [], (Delete (Nodetach, ["n1"; "r"; "n2"], None))))
          ))
       )) |}]
;;

let%expect_test "DETACH DELETE test" =
  parse_and_print {|
CREATE (n)
DETACH DELETE n |};
  [%expect
    {|
    (Create ([(((Some "n"), ([], [])), [])],
       (Some (Delete (Detach, ["n"], None))))) |}]
;;

let%expect_test "NODETACH DELETE test" =
  parse_and_print {|
CREATE (n)
NODETACH DELETE n |};
  [%expect
    {|
    (Create ([(((Some "n"), ([], [])), [])],
       (Some (Delete (Nodetach, ["n"], None))))) |}]
;;

let%expect_test "many DELETE and CREATE clauses test" =
  parse_and_print
    {|
  CREATE (n1)
  CREATE (n2)
  DELETE n1
  DELETE n2
  CREATE (n3)
  DELETE n3
  CREATE (n4)
  |};
  [%expect
    {|
    (Create ([(((Some "n1"), ([], [])), [])],
       (Some (Create ([(((Some "n2"), ([], [])), [])],
                (Some (Delete (Nodetach, ["n1"],
                         (Some (Delete (Nodetach, ["n2"],
                                  (Some (Create ([(((Some "n3"), ([], [])), [])],
                                           (Some (Delete (Nodetach, ["n3"],
                                                    (Some (Create (
                                                             [(((Some "n4"),
                                                                ([], [])),
                                                               [])],
                                                             None)))
                                                    )))
                                           )))
                                  )))
                         )))
                )))
       )) |}]
;;

let%expect_test "Read-write request test" =
  parse_and_print
    {|
  WITH *
  MATCH ()
  WITH *
  WITH *
  CREATE (n1)
  CREATE (n2)
  WITH *
  WITH *
  DELETE n1
  DELETE n2
  CREATE (n3)
  DELETE n3
  CREATE (n4{name:"hola", id:1})
  RETURN *, n4.name, n4.id as id
  |};
  [%expect
    {|
    (With ((Some All), [], [], [],
       (Match ([((None, ([], [])), [])], [],
          (With ((Some All), [], [], [],
             (With ((Some All), [], [], [],
                (Create ([(((Some "n1"), ([], [])), [])],
                   (Some (Create ([(((Some "n2"), ([], [])), [])],
                            (Some (With ((Some All), [], [], [],
                                     (With ((Some All), [], [], [],
                                        (Delete (Nodetach, ["n1"],
                                           (Some (Delete (Nodetach, ["n2"],
                                                    (Some (Create (
                                                             [(((Some "n3"),
                                                                ([], [])),
                                                               [])],
                                                             (Some (Delete (
                                                                      Nodetach,
                                                                      ["n3"],
                                                                      (Some (
                                                                      Create (
                                                                        [((
                                                                        (Some "n4"),
                                                                        (
                                                                        [],
                                                                        [("name",
                                                                        (Const
                                                                        (String
                                                                        "hola")));
                                                                        ("id",
                                                                        (Const
                                                                        (Int64 1L)))
                                                                        ])),
                                                                        [])],
                                                                        (Some (
                                                                        Return (
                                                                        (Some All),
                                                                        [((
                                                                        Property (
                                                                        "n4",
                                                                        "name")),
                                                                        None);
                                                                        ((
                                                                        Property (
                                                                        "n4",
                                                                        "id")),
                                                                        (Some "id"))
                                                                        ],
                                                                        []))))))
                                                                      )))
                                                             )))
                                                    )))
                                           ))
                                        ))
                                     )))
                            )))
                   ))
                ))
             ))
          ))
       )) |}]
;;
