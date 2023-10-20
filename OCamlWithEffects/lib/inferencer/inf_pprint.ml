(** Copyright 2021-2024, DmitryPilyuk and raf-nr *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Typedtree

let pp_const_type ppf = function
  | Int -> Format.fprintf ppf "int"
  | Bool -> Format.fprintf ppf "bool"
  | Unit -> Format.fprintf ppf "unit"
  | Char -> Format.fprintf ppf "char"
  | String -> Format.fprintf ppf "string"
;;

let pp_type ppf typ =
  let rec helper ppf = function
    | TPrim t -> pp_const_type ppf t
    | TVar v ->
      let var_name = Char.chr (Char.code 'a' + v) in
      Format.fprintf ppf "'%c" var_name
    | TArr (l, r) ->
      let pp =
        match l with
        | TArr (_, _) -> Format.fprintf ppf "(%a) -> %a" helper l helper r
        | _ -> Format.fprintf ppf "%a -> %a" helper l helper r
      in
      pp
    | TList t ->
      let pp =
        match t with
        | TArr (_, _) -> Format.fprintf ppf "(%a) list" helper t
        | _ -> Format.fprintf ppf "%a list" helper t
      in
      pp
    | TTuple tl ->
      Format.fprintf
        ppf
        "%a"
        (Format.pp_print_list
           ~pp_sep:(fun ppf () -> Format.fprintf ppf " * ")
           (fun ppf ty -> helper ppf ty))
        tl
    | TEffect (TArr (_, _) as t) -> Format.fprintf ppf "(%a) effect" helper t
    | TEffect t -> Format.fprintf ppf "%a effect" helper t
    | TContinuation (a, l) ->
      Format.fprintf ppf "continuation %a %a" helper a helper l
      (* Unreachable, used artificially for debugging *)
    | TContinuePoint ->
      Format.fprintf
        ppf
        "continue variable" (* Unreachable, used artificially for debugging *)
  in
  helper ppf typ
;;

let pp_error ppf = function
  | `Occurs_check -> Format.fprintf ppf "Type error: occurs check failed."
  | `Not_reachable -> Format.fprintf ppf "Not reachable."
  | `Unification_failed (l, r) ->
    Format.fprintf
      ppf
      "Type error: unification failed - type %a does not match expected type %a"
      pp_type
      l
      pp_type
      r
  | `Unbound_variable name -> Format.fprintf ppf "Type error: unbound variable '%s'" name
  | `Unbound_effect name -> Format.fprintf ppf "Type error: unbound effect '%s'" name
  | `Several_bounds name ->
    Format.fprintf ppf "Type error: variable '%s' is bound several times" name
  | `Handler_without_effect ->
    Format.fprintf
      ppf
      "Type error: left side of effect handler can only contain an effect with continue \
       point."
  | `Wrong_effect_typ (name, typ) ->
    let expected_typ =
      match typ with
      | TArr (l, r) -> l @-> teffect r
      | _ -> teffect typ
    in
    Format.fprintf
      ppf
      "Type error: effect %s is of type %a, but type %a was expected."
      name
      pp_type
      typ
      pp_type
      expected_typ
  | `Not_effect_with_args name ->
    Format.fprintf
      ppf
      "Type error: effect '%s' cannot take arguments - it is an effect without arguments."
      name
  | `Not_effect_without_args name ->
    Format.fprintf
      ppf
      "Type error: effect '%s' is an effect that takes an argument, but it's presented \
       without an argument in the handler."
      name
  | `Perform_with_no_effect ->
    Format.fprintf
      ppf
      "Type error: the type of the argument passed to perform must be the effect typ."
  | `Not_continue_val name ->
    Format.fprintf ppf "Type error: variable '%s' is not continue variable. " name
  | `Handler_without_continue ->
    Format.fprintf ppf "Type error: the effect handler does not contain a continuation."
;;

let print_inferencer_error e =
  let error_str = Format.asprintf "%a" pp_error e in
  Format.printf "%s\n" error_str
;;

let type_to_string typ = Format.asprintf "%a" pp_type typ
let expr_without_name typ = "- : " ^ type_to_string typ
let expr_with_name name typ = String.concat " " [ "val"; name; ":"; type_to_string typ ]
let print_expr_type typ = Format.printf "%s\n" (expr_without_name typ)

let print_program_type env names_list =
  Base.List.iter names_list ~f:(fun name ->
    match Base.Map.find env name with
    | Some (Scheme (_, typ)) -> Format.printf "%s\n" (expr_with_name name typ)
    | _ -> Format.printf "")
;;
