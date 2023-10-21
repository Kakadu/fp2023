open! Base
open Angstrom
open Ast

let pp printer parser str =
  Stdlib.Format.printf "%a" printer
  @@ Result.ok_or_failwith
  @@ Angstrom.parse_string ~consume:Angstrom.Consume.All parser str

let ws = skip_while Char.is_whitespace

let ws1 = take_while1 Char.is_whitespace *> return ()

(* ======= Value names ======= *)

let is_keyword = function
  | "and"
  | "as"
  | "assert"
  | "asr"
  | "begin"
  | "class"
  | "constraint"
  | "do"
  | "done"
  | "downto"
  | "else"
  | "end"
  | "exception"
  | "external"
  | "false"
  | "for"
  | "fun"
  | "function"
  | "functor"
  | "if"
  | "in"
  | "include"
  | "inherit"
  | "initializer"
  | "land"
  | "lazy"
  | "let"
  | "lor"
  | "lsl"
  | "lsr"
  | "lxor"
  | "match"
  | "method"
  | "mod"
  | "module"
  | "mutable"
  | "new"
  | "nonrec"
  | "object"
  | "of"
  | "open"
  | "or"
  | "private"
  | "rec"
  | "sig"
  | "struct"
  | "then"
  | "to"
  | "true"
  | "try"
  | "type"
  | "val"
  | "virtual"
  | "when"
  | "while"
  | "with" ->
      true
  | _ ->
      false

let parse_value_name =
  let parse_first =
    satisfy (function 'a' .. 'z' | '_' -> true | _ -> false)
    >>| String.of_char
  in
  let parse_rest =
    take_while (function
      | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' | '\'' ->
          true
      | _ ->
          false )
  in
  let* name = lift2 String.( ^ ) parse_first parse_rest in
  if not (is_keyword name) then return name
  else fail (name ^ " keyword can't be used as value name")

(* ======= Constants ======= *)

let parse_int =
  take_while1 (function '0' .. '9' -> true | _ -> false)
  >>| fun i -> Const_integer (Int.of_string i)

(** ["hello world"] *)
let parse_string =
  let open Char in
  char (of_int_exn 34) *> take_till (Char.equal (of_int_exn 34))
  <* char (of_int_exn 34) (* [of_int_exn 34 = '"'] *)
  >>| fun s -> Const_string s

let parse_char = char '\'' *> any_char <* char '\'' >>| fun c -> Const_char c

let parse_const = choice [parse_char; parse_string; parse_int]

(* ======= Value bindings ======= *)

let skip_let_keyword = ws *> string "let"

let parse_rec_flag =
  ws1 *> option Nonrecursive (string "rec" *> return Recursive)

(**
  [P1 = E1 and ... and Pn = En]
  [P1 PArg1 = E1 and ... and Pn = En]
*)
let parse_bindings pexp ppat =
  let parse_binding =
    let parse_args = ws *> sep_by ws ppat in
    let insert_args exp = function
      (* [let f x = E] => [let f = fun x -> E] *)
      | [] ->
          exp
      | _ as args ->
          Exp_function (args, Function_body exp)
    in
    lift3
      (fun pat args exp -> {pat; expr= insert_args exp args})
      ppat parse_args
      (ws *> char '=' *> pexp)
  in
  sep_by1 (ws *> string "and") parse_binding

let parse_let_binding pexp ppat =
  skip_let_keyword *> both parse_rec_flag (parse_bindings pexp ppat)

(* ======= Infix & prefix parsing ======= *)

type 'a infix_operator = {op: 'a; op_length: int}

let parse_infix_prefix ~parse_operand ~peek_infix_op ~get_infix_binding_power
    ~infix_fold_fun ~parse_prefix_op ~get_prefix_binding_power ~apply_prefix_op
    =
  (*
     Pratt parsing
     https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html
  *)
  let rec helper min_bp =
    let* lhs =
      option None (ws *> parse_prefix_op >>| Option.some)
      >>= function
      | None ->
          parse_operand
      | Some op ->
          let r_bp = get_prefix_binding_power op in
          let* rhs = helper r_bp in
          return (apply_prefix_op op rhs)
    in
    many
      (let* {op; op_length} = ws *> peek_infix_op in
       let l_bp, r_bp = get_infix_binding_power op in
       if l_bp < min_bp then fail "found op with lower binding power"
       else
         advance op_length
         *> let* rhs = helper r_bp in
            return (op, rhs) )
    >>| fun results -> List.fold_left ~init:lhs ~f:infix_fold_fun results
  in
  helper 0
