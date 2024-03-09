(** Copyright 2021-2024, DmitryPilyuk and raf-nr *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Auxiliary
open Angstrom

(* Dispatch recording is needed to call parsers at a point,
   where they have not yet been declared, but are declared later *)

type dispatch =
  { parse_un_op : dispatch -> expr Angstrom.t
  ; parse_bin_op : dispatch -> expr Angstrom.t
  ; parse_list : dispatch -> expr Angstrom.t
  ; parse_list_cons : dispatch -> expr Angstrom.t
  ; parse_tuple : dispatch -> expr Angstrom.t
  ; parse_application : dispatch -> expr Angstrom.t
  ; parse_fun : dispatch -> expr Angstrom.t
  ; parse_let_in : dispatch -> expr Angstrom.t
  ; parse_match_with : dispatch -> expr Angstrom.t
  ; parse_try_with : dispatch -> expr Angstrom.t
  ; parse_if_then_else : dispatch -> expr Angstrom.t
  ; parse_effect_with_arguments : dispatch -> expr Angstrom.t
  ; parse_perform : dispatch -> expr Angstrom.t
  ; parse_continue : dispatch -> expr Angstrom.t
  }

(* Helper parsers *)

let skip_wspace = skip_while is_whitespace
let skip_wspace1 = take_while1 is_whitespace
let parens p = skip_wspace *> char '(' *> p <* skip_wspace <* char ')'
let sqr_parens p = skip_wspace *> char '[' *> p <* skip_wspace <* char ']'
let list_constr = skip_wspace *> string "::"
let arrow = skip_wspace *> string "->"
let trait = skip_wspace *> string "|"
let tying = skip_wspace *> string "="

let list_sep =
  skip_wspace *> char ';' *> skip_wspace
  <|> (skip_wspace *> peek_char
       >>= function
       | Some c when c != ']' -> fail "Error: Expected semicolon or end of list"
       | _ -> return ())
;;

(* ---------------- *)

(* Name parsers *)

let parse_name =
  fix
  @@ fun self ->
  skip_wspace
  *> (parens self
      <|> take_while1 (fun x ->
        is_lower x || is_upper x || is_digit x || x = '\'' || x = '_'))
;;

let parse_uncapitalized_name =
  parse_name
  >>= fun name ->
  if (is_lower name.[0] || name.[0] = '_') && is_keyword name != true
  then return name
  else fail "Parsing error: not an uncapitalized entity."
;;

let parse_capitalized_name =
  parse_name
  >>= fun name ->
  if is_upper name.[0] && is_keyword name != true
  then return name
  else fail "Parsing error: not an capitalized entity."
;;

let ident constr =
  skip_wspace *> peek_char
  >>= is_acceptable_fl
  >>= fun _ ->
  take_while is_ident
  >>= fun s ->
  if is_keyword s
  then fail "Parsing error: name is used as keyword"
  else return @@ constr s
;;

let parse_ident = ident eidentifier (* Expression identifier parser. *)

(* ---------------- *)

(* Constant parsers *)

let parse_cint = take_while1 is_digit >>| int_of_string >>| fun x -> Int x
let parse_cstring = char '"' *> take_while (( != ) '"') <* char '"' >>| fun x -> String x
let parse_cchar = char '\'' *> any_char <* char '\'' >>| fun x -> Char x
let parse_cbool = string "true" <|> string "false" >>| bool_of_string >>| fun x -> Bool x
let parse_cunit = string "()" >>| fun _ -> Unit

let const constr =
  fix
  @@ fun self ->
  skip_wspace
  *> (parens self
      <|>
      let const =
        choice [ parse_cint; parse_cstring; parse_cchar; parse_cbool; parse_cunit ]
      in
      lift constr const)
;;

let parse_const = const econst (* Expression const parser. *)

(* ---------------- *)

(* Pattern parsers *)

let parse_pattern_nill = sqr_parens skip_wspace >>| pnill
let parse_pattern_any = skip_wspace *> char '_' >>| pany
let parse_pattern_val = ident pval
let parse_pattern_const = const pconst
let parse_pattern_list_constr = list_constr *> return plist_cons

let parse_pattern_effect_without_args =
  skip_wspace *> lift peffect_without_args parse_capitalized_name
;;

let parse_pattern_effect_with_args pattern_parser =
  skip_wspace *> lift2 peffect_with_args parse_capitalized_name pattern_parser
;;

let parse_pattern_tuple p_pattern =
  parens
  @@ lift2
       (fun h tl -> ptuple @@ (h :: tl))
       p_pattern
       (many1 (skip_wspace *> string "," *> p_pattern))
;;

let parse_primitive_pattern =
  choice
    [ parse_pattern_nill
    ; parse_pattern_any
    ; parse_pattern_val
    ; parse_pattern_const
    ; parse_pattern_effect_without_args
    ]
;;

let parse_pattern =
  fix
  @@ fun self ->
  skip_wspace
  *>
  let parse_pattern_list_constr =
    chainr1 (parens self <|> parse_primitive_pattern) parse_pattern_list_constr
  in
  let p_parser =
    choice
      [ parse_pattern_list_constr
      ; parse_pattern_effect_with_args self
      ; parse_primitive_pattern
      ; parse_pattern_tuple self
      ]
  in
  parse_pattern_effect_with_args p_parser <|> p_parser
;;

(* ---------------- *)

(* Effect annotation parsers *)

let parse_prim_type p_type =
  choice
    [ parens p_type
    ; skip_wspace *> string "int" *> return aint
    ; skip_wspace *> string "bool" *> return abool
    ; skip_wspace *> string "char" *> return achar
    ; skip_wspace *> string "string" *> return astring
    ; skip_wspace *> string "unit" *> return aunit
    ]
;;

let parse_arrow_type = arrow *> return (fun t1 t2 -> aarrow t1 t2)

let parse_tuple_type p_type =
  lift2
    (fun h tl -> atuple @@ (h :: tl))
    p_type
    (many1 (skip_wspace *> string "*" *> p_type))
;;

let parse_list_type p_type = lift alist (p_type <* skip_wspace <* string "list")
let parse_effect_type p_type = lift aeffect (p_type <* skip_wspace <* string "effect")

let simple_type_parsers self =
  let parse_t = parse_prim_type self in
  let parse_t = parse_list_type parse_t <|> parse_t in
  let parse_t = parse_effect_type parse_t <|> parse_t in
  let parse_t = parse_tuple_type parse_t <|> parse_t in
  parse_t
;;

let parse_simple_type = fix @@ fun self -> skip_wspace *> simple_type_parsers self

let parse_function_type =
  fix @@ fun self -> skip_wspace *> chainr1 (simple_type_parsers self) parse_arrow_type
;;

let parse_hard_effect_type =
  parens parse_function_type
  <* skip_wspace
  <* string "effect"
  >>| aeffect (* (arrow) effect *)
;;

let parse_type_annotation =
  let parse_effect_with_args_type =
    lift2
      aarrow
      (parens parse_function_type <|> parse_simple_type)
      (arrow
       *> skip_wspace
       *> (parse_simple_type <|> parse_hard_effect_type <|> parens parse_function_type))
  in
  let parse_effect_without_args_type = parse_simple_type <|> parse_function_type in
  parse_effect_with_args_type <|> parse_effect_without_args_type
;;

(* ---------------- *)

(* Effects parsers *)

let parse_effect_declaration =
  lift2
    deffect_declaration
    (skip_wspace *> string "effect" *> parse_capitalized_name)
    (skip_wspace *> string ":" *> parse_type_annotation)
;;

let parse_effect_without_arguments =
  fix
  @@ fun self ->
  skip_wspace *> (parens self <|> lift eeffect_without_arguments parse_capitalized_name)
;;

let parse_effect_with_arguments pack =
  fix
  @@ fun self ->
  let parse_expr =
    choice
      [ pack.parse_tuple pack <|> parens (pack.parse_tuple pack)
      ; pack.parse_list_cons pack <|> parens (pack.parse_list_cons pack)
      ; parens @@ pack.parse_bin_op pack
      ; parens @@ pack.parse_un_op pack
      ; pack.parse_list pack <|> parens (pack.parse_list pack)
      ; parens @@ pack.parse_perform pack
      ; parens @@ pack.parse_application pack
      ; parens @@ pack.parse_fun pack
      ; parens @@ pack.parse_let_in pack
      ; parens @@ pack.parse_if_then_else pack
      ; parens @@ pack.parse_match_with pack
      ; parse_const
      ; parse_ident
      ]
  in
  skip_wspace
  *> (parens self <|> lift2 eefect_with_arguments parse_capitalized_name parse_expr)
;;

let parse_effect pack =
  parse_effect_with_arguments pack <|> parse_effect_without_arguments
;;

let parse_try_with pack =
  fix
  @@ fun self ->
  let parse_expr =
    choice
      [ pack.parse_bin_op pack
      ; pack.parse_un_op pack
      ; self
      ; pack.parse_list pack
      ; pack.parse_list_cons pack
      ; pack.parse_tuple pack
      ; pack.parse_application pack
      ; pack.parse_fun pack
      ; pack.parse_let_in pack
      ; pack.parse_if_then_else pack
      ; pack.parse_perform pack
      ; pack.parse_continue pack
      ; pack.parse_match_with pack
      ; pack.parse_effect_with_arguments pack
      ; parse_effect_without_arguments
      ; parse_const
      ; parse_ident
      ]
  in
  let parse_case =
    lift3
      (fun pat cont expr -> effecthandler pat expr cont)
      (trait *> (parse_pattern_effect_without_args <|> parse_pattern))
      (skip_wspace *> parse_uncapitalized_name >>| continue)
      (arrow *> parse_expr)
  in
  skip_wspace
  *> string "try"
  *> lift2
       etry_with
       (parens parse_expr <|> parse_expr <* skip_wspace <* string "with")
       (many1 parse_case)
;;

let parse_perform pack =
  skip_wspace
  *>
  let perform = skip_wspace *> string "perform" <* skip_wspace in
  let* result = perform *> (parse_effect pack <|> parse_ident) in
  return (eeffect_perform result)
;;

let parse_continue pack =
  fix
  @@ fun self ->
  skip_wspace
  *> (parens self
      <|>
      let parse_expr =
        choice
          [ parens @@ pack.parse_tuple pack
          ; parens @@ pack.parse_list_cons pack
          ; parens @@ pack.parse_bin_op pack
          ; parens @@ pack.parse_un_op pack
          ; pack.parse_list pack
          ; parens @@ pack.parse_perform pack
          ; parens @@ pack.parse_application pack
          ; parens @@ pack.parse_fun pack
          ; parens @@ pack.parse_let_in pack
          ; parens @@ pack.parse_if_then_else pack
          ; parens @@ pack.parse_match_with pack
          ; parse_const
          ; parse_ident
          ]
      in
      let parse_continue = skip_wspace *> string "continue" *> skip_wspace in
      let parse_continue_content = parse_uncapitalized_name >>| econt_val in
      lift2
        eeffect_continue
        (parse_continue *> parse_continue_content)
        (skip_wspace *> parse_expr))
;;

(* ---------------- *)

(* Basic expression parsers *)

let addition = skip_wspace *> char '+' >>| sadd
let subtraction = skip_wspace *> char '-' >>| ssub
let multiplication = skip_wspace *> char '*' >>| smul
let division = skip_wspace *> char '/' >>| sdiv
let eqality = skip_wspace *> char '=' >>| seq
let neqality = skip_wspace *> (string "<>" <|> string "!=") >>| sneq
let logand = skip_wspace *> string "&&" >>| sand
let logor = skip_wspace *> string "||" >>| sor
let larger = skip_wspace *> char '>' >>| sgt
let largerEq = skip_wspace *> string ">=" >>| sgte
let less = skip_wspace *> char '<' >>| slt
let lessEq = skip_wspace *> string "<=" >>| slte

let binary_operations =
  [ multiplication
  ; division
  ; addition
  ; subtraction
  ; larger
  ; largerEq
  ; less
  ; lessEq
  ; eqality
  ; neqality
  ; logand
  ; logor
  ]
;;

let unary_minus = skip_wspace *> char '-' >>| umin
let unary_plus = skip_wspace *> char '+' >>| uplus
let logic_not = skip_wspace *> string "not" >>| unot

let parse_un_op pack =
  fix
  @@ fun self ->
  skip_wspace
  *>
  let parse_minus_and_plus_content =
    choice
      [ parens self
      ; parens @@ pack.parse_application pack
      ; parens @@ pack.parse_if_then_else pack
      ; parens @@ pack.parse_perform pack
      ; parens @@ pack.parse_continue pack
      ; parse_const
      ; parse_ident
      ; parens @@ pack.parse_bin_op pack
      ]
  in
  let parse_not_content =
    choice
      [ parens @@ pack.parse_bin_op pack
      ; parens self
      ; parens @@ pack.parse_application pack
      ; parens @@ pack.parse_if_then_else pack
      ; parens @@ pack.parse_perform pack
      ; parens @@ pack.parse_continue pack
      ; parse_const
      ; parse_ident
      ]
  in
  parens self
  <|> lift2 eunop unary_minus parse_minus_and_plus_content
  <|> lift2 eunop unary_plus parse_minus_and_plus_content
  <|> lift2 eunop logic_not parse_not_content
;;

let parse_bin_op pack =
  fix
  @@ fun self ->
  skip_wspace
  *>
  let parse_expr =
    choice
      [ parens self
      ; pack.parse_un_op pack
      ; pack.parse_list pack
      ; pack.parse_list_cons pack
      ; pack.parse_tuple pack
      ; pack.parse_application pack
      ; pack.parse_fun pack
      ; pack.parse_let_in pack
      ; pack.parse_match_with pack
      ; pack.parse_perform pack
      ; pack.parse_continue pack
      ; pack.parse_if_then_else pack
      ; parse_const
      ; parse_ident
      ]
  and chainl1 e op =
    let rec go acc = lift2 (fun f x -> ebinop f acc x) op e >>= go <|> return acc in
    e >>= fun init -> go init
  in
  List.fold_left
    (fun acc x -> chainl1 acc x)
    (chainl1 parse_expr multiplication)
    binary_operations
  >>= fun res ->
  match res with
  | EBinaryOperation (_, _, _) -> return res
  | _ -> fail "Error: not binary operation."
;;

let parse_if_then_else pack =
  fix
  @@ fun self ->
  skip_wspace
  *>
  let parse_expr =
    choice
      [ pack.parse_bin_op pack
      ; pack.parse_un_op pack
      ; pack.parse_list pack
      ; pack.parse_list_cons pack
      ; pack.parse_tuple pack
      ; pack.parse_let_in pack
      ; pack.parse_match_with pack
      ; pack.parse_continue pack
      ; pack.parse_perform pack
      ; pack.parse_effect_with_arguments pack
      ; parse_effect_without_arguments
      ; pack.parse_application pack
      ; pack.parse_fun pack
      ; self
      ; parse_const
      ; parse_ident
      ]
  in
  parens self
  <|> string "if"
      *> lift3
           eif_then_else
           parse_expr
           (skip_wspace *> string "then" *> parse_expr)
           (skip_wspace *> string "else" *> parse_expr)
;;

let parse_fun pack =
  let parse_expr =
    choice
      [ pack.parse_bin_op pack
      ; pack.parse_un_op pack
      ; pack.parse_list pack
      ; pack.parse_list_cons pack
      ; pack.parse_tuple pack
      ; pack.parse_application pack
      ; pack.parse_match_with pack
      ; pack.parse_continue pack
      ; pack.parse_perform pack
      ; pack.parse_effect_with_arguments pack
      ; parse_effect_without_arguments
      ; pack.parse_if_then_else pack
      ; pack.parse_let_in pack
      ; parse_const
      ; parse_ident
      ]
  in
  let fun_helper =
    skip_wspace *> string "fun" *> many1 parse_pattern
    >>= fun args ->
    arrow *> parse_expr
    >>= fun expr ->
    match List.rev args with
    | h :: tl -> return (List.fold_left (fun acc x -> efun x acc) (efun h expr) tl)
    | _ -> fail "Error"
  in
  parens fun_helper <|> fun_helper
;;

let parse_list_cons pack =
  fix
  @@ fun self ->
  skip_wspace
  *>
  let parse_expr =
    choice
      [ pack.parse_tuple pack
      ; parens self
      ; parens @@ pack.parse_bin_op pack
      ; pack.parse_un_op pack
      ; pack.parse_list pack
      ; pack.parse_application pack
      ; pack.parse_let_in pack
      ; parens @@ pack.parse_perform pack
      ; pack.parse_continue pack
      ; pack.parse_effect_with_arguments pack
      ; parens @@ pack.parse_fun pack
      ; parens @@ pack.parse_if_then_else pack
      ; parens @@ pack.parse_match_with pack
      ; parse_effect_without_arguments
      ; parse_const
      ; parse_ident
      ]
  in
  let left = parse_expr <* list_constr in
  let right = skip_wspace *> (self <|> parse_expr) in
  lift2 elistcons left right
;;

let parse_list pack =
  fix
  @@ fun self ->
  skip_wspace
  *>
  let parse_expr =
    choice
      [ pack.parse_bin_op pack
      ; pack.parse_un_op pack
      ; self
      ; pack.parse_list_cons pack
      ; pack.parse_tuple pack
      ; pack.parse_application pack
      ; pack.parse_fun pack
      ; pack.parse_let_in pack
      ; pack.parse_match_with pack
      ; pack.parse_continue pack
      ; pack.parse_perform pack
      ; pack.parse_effect_with_arguments pack
      ; parse_effect_without_arguments
      ; pack.parse_if_then_else pack
      ; parse_const
      ; parse_ident
      ]
  in
  let content = skip_wspace *> many (parse_expr <* list_sep) in
  parens self <|> lift elist @@ sqr_parens content
;;

let parse_tuple pack =
  fix
  @@ fun self ->
  skip_wspace
  *>
  let parse_expr =
    choice
      [ pack.parse_bin_op pack
      ; pack.parse_un_op pack
      ; self
      ; pack.parse_list pack
      ; pack.parse_list_cons pack
      ; pack.parse_application pack
      ; pack.parse_fun pack
      ; pack.parse_let_in pack
      ; pack.parse_match_with pack
      ; pack.parse_continue pack
      ; pack.parse_perform pack
      ; pack.parse_effect_with_arguments pack
      ; parse_effect_without_arguments
      ; pack.parse_if_then_else pack
      ; parse_const
      ; parse_ident
      ]
  in
  parens
  @@ lift2
       (fun h tl -> etuple @@ (h :: tl))
       parse_expr
       (many1 (skip_wspace *> string "," *> parse_expr))
;;

let parse_declaration pack =
  let parse_expr =
    choice
      [ pack.parse_bin_op pack
      ; pack.parse_un_op pack
      ; pack.parse_list pack
      ; pack.parse_list_cons pack
      ; pack.parse_tuple pack
      ; pack.parse_application pack
      ; pack.parse_fun pack
      ; pack.parse_let_in pack
      ; pack.parse_perform pack
      ; pack.parse_continue pack
      ; pack.parse_match_with pack
      ; pack.parse_try_with pack
      ; pack.parse_effect_with_arguments pack
      ; parse_effect_without_arguments
      ; pack.parse_if_then_else pack
      ; parse_const
      ; parse_ident
      ]
  in
  let helper constr =
    lift2
      constr
      parse_uncapitalized_name
      (many parse_pattern
       >>= fun args ->
       tying *> parse_expr
       >>= fun expr ->
       skip_wspace
       *>
       match List.rev args with
       | h :: tl -> return @@ List.fold_left (fun acc x -> efun x acc) (efun h expr) tl
       | _ -> return expr)
  in
  skip_wspace *> string "let" *> skip_wspace1 *> option "" (string "rec" <* skip_wspace1)
  >>= function
  | "rec" -> helper drec_declaration
  | _ -> helper ddeclaration
;;

let parse_let_in pack =
  fix
  @@ fun self ->
  let parse_expr =
    choice
      [ pack.parse_bin_op pack
      ; pack.parse_un_op pack
      ; pack.parse_list pack
      ; pack.parse_list_cons pack
      ; pack.parse_tuple pack
      ; pack.parse_application pack
      ; pack.parse_fun pack
      ; pack.parse_perform pack
      ; pack.parse_continue pack
      ; pack.parse_match_with pack
      ; pack.parse_effect_with_arguments pack
      ; pack.parse_try_with pack
      ; parse_effect_without_arguments
      ; self
      ; pack.parse_if_then_else pack
      ; parse_const
      ; parse_ident
      ]
  in
  let helper constr =
    lift3
      constr
      parse_uncapitalized_name
      (many parse_pattern
       >>= fun args ->
       tying *> parse_expr
       >>= fun expr ->
       skip_wspace
       *>
       match List.rev args with
       | h :: tl -> return @@ List.fold_left (fun acc x -> efun x acc) (efun h expr) tl
       | _ -> return expr)
      (skip_wspace *> string "in" *> parse_expr)
  in
  skip_wspace *> string "let" *> skip_wspace1 *> option "" (string "rec" <* skip_wspace1)
  >>= function
  | "rec" -> helper erec_let_in
  | _ -> helper elet_in
;;

let parse_application pack =
  fix
  @@ fun self ->
  skip_wspace
  *> (parens self
      <|>
      let function_parser =
        choice
          [ parens @@ pack.parse_fun pack
          ; parens @@ pack.parse_let_in pack
          ; parens @@ pack.parse_if_then_else pack
          ; parens @@ pack.parse_match_with pack
          ; parse_ident
          ]
      and operand_parser =
        choice
          [ parens @@ pack.parse_bin_op pack
          ; parens @@ pack.parse_un_op pack
          ; pack.parse_tuple pack
          ; pack.parse_list pack
          ; parens @@ pack.parse_list_cons pack
          ; parens self
          ; parens @@ pack.parse_fun pack
          ; parens @@ pack.parse_match_with pack
          ; parens @@ pack.parse_perform pack
          ; parens @@ pack.parse_if_then_else pack
          ; parens @@ pack.parse_let_in pack
          ; parens @@ pack.parse_effect_with_arguments pack
          ; parse_effect_without_arguments
          ; parse_const
          ; parse_ident
          ]
      in
      let chainl acc = lift (eapplication acc) operand_parser in
      let rec go acc = chainl acc >>= go <|> return acc in
      function_parser >>= fun init -> chainl init >>= fun init -> go init)
;;

let parse_match_with pack =
  fix
  @@ fun self ->
  let parse_expr =
    choice
      [ pack.parse_bin_op pack
      ; pack.parse_un_op pack
      ; self
      ; pack.parse_list pack
      ; pack.parse_list_cons pack
      ; pack.parse_tuple pack
      ; pack.parse_application pack
      ; pack.parse_fun pack
      ; pack.parse_let_in pack
      ; pack.parse_if_then_else pack
      ; pack.parse_continue pack
      ; pack.parse_perform pack
      ; pack.parse_effect_with_arguments pack
      ; parse_effect_without_arguments
      ; parse_const
      ; parse_ident
      ]
  in
  let parse_case =
    lift2 (fun pat expr -> pat, expr) (trait *> parse_pattern) (arrow *> parse_expr)
  in
  parens self
  <|> skip_wspace
      *> string "match"
      *> lift2 ematch_with (parse_expr <* skip_wspace <* string "with") (many1 parse_case)
;;

(* ---------------- *)

(* Running the parser *)

let default =
  { parse_bin_op
  ; parse_un_op
  ; parse_list
  ; parse_list_cons
  ; parse_tuple
  ; parse_application
  ; parse_fun
  ; parse_let_in
  ; parse_match_with
  ; parse_try_with
  ; parse_if_then_else
  ; parse_effect_with_arguments
  ; parse_perform
  ; parse_continue
  }
;;

let expr_parsers input =
  let helper =
    choice
      [ parse_let_in input
      ; parse_bin_op input
      ; parse_un_op input
      ; parse_list input
      ; parse_list_cons input
      ; parse_tuple input
      ; parse_application input
      ; parse_fun input
      ; parse_match_with input
      ; parse_try_with input
      ; parse_if_then_else input
      ; parse_effect_without_arguments
      ; parse_effect_with_arguments input
      ; parse_perform input
      ; parse_continue input
      ; parse_ident
      ; parse_const
      ]
  in
  lift sexpression helper
;;

let decl_parsers input =
  let helper = choice [ parse_declaration input; parse_effect_declaration ] in
  lift sdeclaration helper
;;

let parsers input = expr_parsers input <|> decl_parsers input

let parse input =
  let del = (skip_wspace *> string ";;" *> skip_wspace <|> skip_wspace) *> skip_wspace in
  parse_string ~consume:All (many (parsers default <* del)) input
;;

(* ---------------- *)
