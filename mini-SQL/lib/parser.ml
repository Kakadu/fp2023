open Angstrom
open Ast

exception Parse_error of string

(* ### comparisons ### *)
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
let is_name c = is_letter c || is_digit c

let is_string_char = function
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

(* #### Basic parsers ### *)

(* --- nums --- *)

let dot =
  peek_char
  >>= function
  | Some '.' -> advance 1 *> return true
  | _ -> return false
;;

let d_sign =
  peek_char
  >>= function
  | Some '+' -> advance 1 *> return '+'
  | Some '-' -> advance 1 *> return '-'
  | Some x when is_digit x -> return '+'
  | _ -> fail "Incorrect string for digit parsing"
;;

let digit =
  d_sign
  >>= fun s ->
  take_while is_digit
  >>= fun ld ->
  dot
  >>= function
  | true ->
    take_while is_digit
    >>= fun rd -> return (Float_Digit (float_of_string (String.make 1 s ^ ld ^ "." ^ rd)))
  | false -> return (Digit (int_of_string (String.make 1 s ^ ld)))
;;

(* --- strings --- *)
let str =
  peek_char
  >>= function
  | Some chr when is_string_char chr ->
    advance 1 *> take_while (fun c -> not (is_string_char c))
    >>= fun s -> return (String s) <* char chr
  | _ -> fail "Can't parse string"
;;

(* --- names --- *)

let name =
  (peek_char
   >>= function
   | Some x when is_letter x -> return (String.make 1 x)
   | _ -> fail "Names must begin with a letter of the Latin alphabet")
  *> take_while is_name
  >>| fun str -> Name str
;;

(** --- Bool --- *)
let bool =
  string_ci "true"
  <|> string_ci "false"
  >>= fun r ->
  match String.lowercase_ascii r with
  | "true" -> return (Bool true)
  | "false" -> return (Bool false)
  | _ -> fail "Can't parse bool"
;;

let value = digit <|> bool <|> name <|> str

(* --- Spaces --- *)
let space = skip_while is_space
let lspace p = space *> p
let rspace p = p <* space
let bspace p = space *> p <* space

(* ### operators ### *)

let parens p = char '(' *> bspace p <* char ')'
let op s = lspace (string s)

(* --- Arithmetic --- *)

let ar_add = op "+" *> return (fun x y -> Binary_operation (Add, x, y))
let ar_sub = op "-" *> return (fun x y -> Binary_operation (Substract, x, y))
let ar_mult = op "*" *> return (fun x y -> Binary_operation (Multiply, x, y))
let ar_div = op "/" *> return (fun x y -> Binary_operation (Divide, x, y))
let ar_mod = op "%" *> return (fun x y -> Binary_operation (Modulo, x, y))

(* --- Logic --- *)

let l_and = op "AND" *> return (fun x y -> Binary_operation (And, x, y))
let l_or = op "OR" *> return (fun x y -> Binary_operation (Or, x, y))
let l_not = op "NOT" *> return (fun x -> Unary_operation (Not, x))

(* --- Compare --- *)

let eq = op "=" *> return (fun x y -> Binary_operation (Equal, x, y))
let neq = (op "!=" <|> op "<>") *> return (fun x y -> Binary_operation (Not_Equal, x, y))
let gr = op ">" *> return (fun x y -> Binary_operation (Greater_Than, x, y))
let ls = op "<" *> return (fun x y -> Binary_operation (Less_Than, x, y))
let greq = op ">=" *> return (fun x y -> Binary_operation (Greater_Than_Or_Equal, x, y))
let lseq = op "<=" *> return (fun x y -> Binary_operation (Less_Than_Or_Equal, x, y))

(* --- Priorities --- *)

let ar_pr_low = bspace (ar_add <|> ar_sub)
let ar_pr_med = bspace ar_mod
let ar_pr_high = bspace (ar_mult <|> ar_div)
let cmp_op = eq <|> greq <|> lseq <|> neq <|> gr <|> ls

(* #### Arithm parser ### *)

let chainl1 e op =
  let rec go acc = lift2 (fun f x -> f acc x) op e >>= go <|> return acc in
  e >>= fun init -> go init
;;

let const (v : value t) = v >>| fun r -> Const r

let arithm =
  fix (fun ar ->
    let pars = parens ar <|> const value in
    let term1 = chainl1 pars ar_pr_high in
    let term2 = chainl1 term1 ar_pr_med in
    chainl1 term2 ar_pr_low)
;;

(*### Compare ###*)

let cmp =
  fix (fun cmp ->
    let pars = parens cmp <|> bspace arithm in
    chainl1 pars cmp_op)
;;

(* ### Logic ### *)

(* TODO: NOT PARS *)
let logic =
  fix (fun logic ->
    let term1 = parens logic <|> bspace cmp in
    let term2 = chainl1 term1 l_and in
    chainl1 term2 l_or)
;;

(** ### expr ### *)
let expr_p = logic

let select_p =
  let choice_pars =
    choice
      [ (* "*" Parse *)
        (bspace peek_char
         >>= fun c ->
         match c with
         | Some '*' -> advance 1 *> return All_Columns
         | Some _ | None -> fail "Can't parse All Columns")
      ; (expr_p >>| fun r -> Expr r)
      ]
  in
  sep_by1 (bspace (char ',')) choice_pars
;;

(* Statements parser*)
let statement_parser =
  let string_of_value = function
    | String x | Name x -> x
    | _ -> raise (Parse_error "Incorrect table name")
  in
  let sfw_pars =
    bspace (string "SELECT") *> select_p
    >>= fun exprs ->
    bspace (string "FROM") *> name
    >>= fun table_name ->
    bspace (string "WHERE") *> rspace expr_p
    >>= fun expr ->
    return (Select { exprs; table = string_of_value table_name; condition = Some expr })
  in
  let sf_pars =
    bspace (string "SELECT") *> select_p
    >>= fun exprs ->
    bspace (string "FROM") *> rspace name
    >>= fun table_name ->
    return (Select { exprs; table = string_of_value table_name; condition = None })
  in
  choice [ sfw_pars; sf_pars ]
;;
