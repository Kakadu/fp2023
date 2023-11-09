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

let expr_of_value (v : value) = Const v

let string_of_value = function
  | String x | Name x -> x
  | _ -> raise (Parse_error "Incorrect table name")
;;
let value_of_expr = function
  | Const x -> x
  | _ -> raise (Parse_error "Incorrect value")
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
  | Some er ->
    fail
      (String.concat
         "\""
         [ "Invalid char "; String.make 1 er; " occured while parsing the digit sign" ])
  | _ -> fail "Can't parse sign of digit"
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
  | Some er ->
    fail
      (String.concat
         "\""
         [ "Invalid char "; String.make 1 er; " occured while parsing the string" ])
  | _ -> fail "Can't parse string"
;;

(* --- names --- *)

let dot =
  peek_char
  >>= function
  | Some c when c = '.' -> advance 1 *> return true
  | _ -> return false
;;

let name =
  (peek_char
   >>= function
   | Some x when is_letter x -> return (String.make 1 x)
   | Some er ->
     fail
       (String.concat
          "\""
          [ "Invalid name "
          ; String.make 1 er
          ; ". Names must begin with a letter of the Latin alphabet"
          ])
   | _ -> fail "Can't parse name")
  *> take_while is_name
;;

let table_name = name >>= fun str -> return (Name str)

(* [table.name] *)
let column_name =
  name
  >>= fun l ->
  dot
  >>= function
  | true -> name >>= fun r -> return (Name (l ^ "." ^ r))
  | false -> return (Name l)
;;

(** --- Bool --- *)
let bool =
  string_ci "true"
  <|> string_ci "false"
  >>= fun r ->
  match String.lowercase_ascii r with
  | "true" -> return (Bool true)
  | "false" -> return (Bool false)
  | er ->
    fail (String.concat "\"" [ "Invalid string "; er; " occured while parsing the bool" ])
;;

let value = digit <|> bool <|> str <|> column_name

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

let eq = op "=" *> return (fun x y -> Binary_operation (Compare Equal, x, y))

let neq =
  (op "!=" <|> op "<>") *> return (fun x y -> Binary_operation (Compare Not_Equal, x, y))
;;

let gr = op ">" *> return (fun x y -> Binary_operation (Compare Greater_Than, x, y))
let ls = op "<" *> return (fun x y -> Binary_operation (Compare Less_Than, x, y))

let greq =
  op ">=" *> return (fun x y -> Binary_operation (Compare Greater_Than_Or_Equal, x, y))
;;

let lseq =
  op "<=" *> return (fun x y -> Binary_operation (Compare Less_Than_Or_Equal, x, y))
;;

(* --- joins --- *)

let join_inner = op "INNER JOIN" *> return Inner
let join_left = op "LEFT JOIN" *> return Left
let join_right = op "RIGHT JOIN" *> return Right
let join_full = (op "FULL JOIN" <|> op "FULL OUTER JOIN") *> return Full
let joins = join_full <|> join_inner <|> join_left <|> join_right

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

let arithm =
  fix (fun ar ->
    let pars = parens ar <|> (value >>| expr_of_value) in
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

let logic =
  fix (fun logic ->
    let term1 = parens logic <|> bspace cmp in
    let term2 = chainl1 term1 l_and in
    chainl1 term2 l_or)
;;

(** ### SELECT exprs ### *)
let expr_p = logic

let select_p =
  let choice_pars =
    choice
      [ (* "*" Parse *)
        (bspace peek_char
         >>= fun c ->
         match c with
         | Some '*' -> advance 1 *> return Asterisk
         | Some _ | None -> fail "Can't parse (*)")
      ; (expr_p >>| fun r -> Expression r)
      ]
  in
  sep_by1 (bspace (char ',')) choice_pars
;;

(* ### JOIN ### *)

let on_p = op "ON" *> chainl1 ((bspace value) >>| expr_of_value) cmp_op

let join =
  lift4
    (fun l op r ex -> Join (op, l, r, ex))
    ((lspace table_name) >>| string_of_value >>| fun x -> Table x)
    (bspace joins)
    ((rspace table_name) >>| string_of_value)
    (rspace on_p)
;;

let from = join <|> (expr_p >>| fun r -> Table (string_of_value (value_of_expr r)))

(* ### Request parser ### *)

(* Optional words parser *)
let opt_word (w : string) (p : 'a t) =
  let n = String.length w in
  peek_string n
  >>= function
  | r when r = w -> advance n *> p >>| fun r -> Some r
  | r when r <> w -> advance n *> return None
  | _ -> fail (String.concat "\"" [ "Error occured during parsing word "; w; " :<" ])
;;

let word (w : string) (p : 'a t) =
  opt_word w p
  >>= function
  | Some x -> return x
  | _ -> fail (String.concat "\"" [ "Can't parse special word "; w; " :<" ])
;;

let parse =
  bspace (word "SELECT" (bspace select_p))
  >>= fun exprs ->
  bspace (word "FROM" (bspace from))
  >>= fun from_st ->
  bspace (opt_word "WHERE" (bspace expr_p))
  >>= fun expr -> return { select = exprs; from = from_st; where = expr }
;;
