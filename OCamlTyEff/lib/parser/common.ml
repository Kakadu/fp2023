open! Base
open Angstrom
open Ast

let ws = skip_while Char.is_whitespace

let parse_name = take_while1 (function 'a' .. 'z' -> true | _ -> false)

(* ======= Constants ======= *)

let parse_int =
  take_while1 (function '0' .. '9' -> true | _ -> false)
  >>| fun i -> Const_integer (Int.of_string i)

(** ["hello world"] *)
let parse_string =
  char '"' *> take_till (Char.equal '"') <* char '"' >>| fun s -> Const_string s

let parse_char = char '\'' *> any_char <* char '\'' >>| fun c -> Const_char c

let parse_const = choice [parse_char; parse_string; parse_int]

(* ======= Value bindings ======= *)

let skip_let_keyword = ws *> string "let"

let parse_rec_flag = ws *> option Nonrecursive (string "rec" *> return Recursive)

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
