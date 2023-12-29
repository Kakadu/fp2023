open Angstrom
open Ast

exception Parse_error of string

(* IS *)
let is_space = function
  | ' ' | '\t' | '\n' | '\r' -> true
  | _ -> false
;;

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false
;;

let is_letter = function
  | 'a' .. 'z' | 'A' .. 'Z' | '_' -> true
  | _ -> false
;;

(* Correct name for column or table.contents
   - Names can contain letters of the Latin alphabet (upper or lower case), numbers and underscores ('_')*)
let is_naming_letter c = is_letter c || is_digit c

let is_string_s_char = function
  | '\'' | '\"' -> true
  | _ -> false
;;

let is_arithm_op = function
  | '+' | '-' | '/' | '*' | '%' -> true
  | _ -> false
;;

let is_condition_op = function
  | '>' | '<' | '=' | '!' -> true
  | _ -> false
;;

(* Digit parser *)

let digit_parser =
  lift2
    (fun s d ->
      match s with
      | x when x = '+' -> Digit (int_of_string d)
      | x when x = '-' -> Digit (-1 * int_of_string d)
      | _ -> raise (Parse_error "Can't parse digit"))
    (peek_char
     >>= function
     | Some x when x = '+' -> advance 1 *> return '+'
     | Some x when is_digit x -> return '+'
     | Some x when x = '-' -> advance 1 *> return '-'
     | _ -> fail "Can't parse digit")
    (take_while is_digit)
;;

(* String parser *)
let string_parser =
  advance 1 *> take_while (fun c -> not (is_string_s_char c))
  >>= fun s -> return (String s) <* advance 1
;;

(* Parser for column/table names*)

let name_parser_s =
  (peek_char
   >>= function
   | Some x when is_letter x -> return (String.make 1 x)
   | _ -> fail "Names must begin with a letter of the Latin alphabet")
  *> take_while is_naming_letter
  >>= fun str -> return str
;;

let name_parser = name_parser_s >>| fun r -> Name r

(** Bool value parser *)
let bool_parser =
  string_ci "true"
  <|> string_ci "false"
  >>= fun r ->
  match String.lowercase_ascii r with
  | "true" -> return (Bool true)
  | "false" -> return (Bool false)
  | _ -> fail "Can't parse bool"
;;

(* Space parsers *)
let space_skip = skip_while is_space
let space_left p = space_skip *> p
let space_right p = p <* space_skip
let space_both p = space_skip *> p <* space_skip

(** Const of value parser *)
let value = digit_parser <|> bool_parser <|> name_parser <|> string_parser

(* --- BINARY OPERATORS ---*)

(** Arithm operators parser*)
let arithm_op_parser =
  satisfy is_arithm_op
  >>= fun op ->
  match op with
  | '+' -> return Add
  | '-' -> return Substract
  | '/' -> return Divide
  | '*' -> return Multiply
  | '%' -> return Modulo
  | _ -> fail "Unsupported arithmetic operator"
;;

(** Logic operators parser *)
let logic_op_parser =
  take_while (fun c -> not (is_space c))
  >>= fun op ->
  match op with
  | "AND" -> return And
  | "OR" -> return Or
  | _ -> fail "Can't parse binary logic operator"
;;

(** Compare operators parser *)
let compare_op_parser =
  let str_p =
    satisfy is_condition_op
    >>= fun c1 ->
    peek_char
    >>= fun cx ->
    match cx with
    | Some c2 when not (is_condition_op c2) -> return (String.make 1 c1)
    | Some c2 -> advance 1 *> return (String.make 1 c1 ^ String.make 1 c2)
    | None -> fail "Unsupported compare operator"
  in
  lift
    (function
      | "=" -> Equal
      | "<>" | "!=" -> NotEqual
      | ">" -> GreaterThan
      | "<" -> LessThan
      | ">=" -> GreaterThanOrEqual
      | "<=" -> LessThanOrEqual
      | _ -> raise (Parse_error "Unsupported compare operator"))
    str_p
;;

(** Binary operators parser *)
let bin_op_parser = logic_op_parser <|> compare_op_parser <|> arithm_op_parser

(* --- Unary operators --- *)

(** NOT parser *)
let unary_logic_parser =
  take_while (fun c -> not (is_space c))
  >>= fun r ->
  match r with
  | "NOT" -> return Not
  | _ -> fail "Can't parse logic operator"
;;

(** Unary operators parser*)

let un_op_parser = unary_logic_parser
let expr_parser =
  let const = value >>| fun r -> Const r in
  lift2 (* | Unary operation *)
    (fun op x -> Unary_operation (op, x))
    (space_right un_op_parser)
    const
  <|> lift3 (* | Binary operation *)
        (fun l op r -> Binary_operation (op, l, r))
        const
        (space_both bin_op_parser)
        const
  <|> const
;;
(** exprs parser *)



(** SELECT <expr> parser *)

let select_expr_parser =
  let choice_pars =
    choice
      [ (* "*" Parse *)
        (space_both peek_char
         >>= fun c ->
         match c with
         | Some x when x = '*' -> advance 1 *> return All_Columns
         | Some _ | None -> fail "Can't parse All Columns")
      ; (* Other exprs pars*)
        (expr_parser >>| fun r -> Expr r)
      ]
  in
  sep_by1 (space_both (char ',')) choice_pars
;;

(* Statements parser*)

let statement_parser =
  let sfw_pars =
    space_both (string "SELECT") *> select_expr_parser
    >>= fun exprs ->
    space_both (string "FROM") *> name_parser_s
    >>= fun name ->
    space_both (string "WHERE") *> space_right expr_parser
    >>= fun expr -> return (Select { exprs; table = name; condition = Some expr })
  in
  let sf_pars =
    space_both (string "SELECT") *> select_expr_parser
    >>= fun exprs ->
    space_both (string "FROM") *> space_right name_parser_s
    >>= fun name -> return (Select { exprs; table = name; condition = None })
  in
  choice [ sfw_pars; sf_pars ]
;;
