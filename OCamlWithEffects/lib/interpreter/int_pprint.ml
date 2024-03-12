(** Copyright 2021-2024, DmitryPilyuk and raf-nr *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Values
open Typedtree
open Inf_pprint

let pp_value ppf typ =
  let rec helper ppf = function
    | VInt i -> Format.fprintf ppf "%d" i
    | VBool b -> Format.fprintf ppf "%B" b
    | VChar c -> Format.fprintf ppf "'%c'" c
    | VString s -> Format.fprintf ppf "%S" s
    | VUnit -> Format.fprintf ppf "()"
    | VTuple l ->
      Format.fprintf
        ppf
        "@[<hov>(%s)@]"
        (String.concat ", " (List.map (fun v -> Format.asprintf "%a" helper v) l))
    | VList l ->
      Format.fprintf
        ppf
        "@[<hov>[%s]@]"
        (String.concat "; " (List.map (fun v -> Format.asprintf "%a" helper v) l))
    | VFun _ -> Format.fprintf ppf "<fun>"
    | VRecFun _ -> Format.fprintf ppf "<fun>"
    | VEffectDeclaration _ | VEffectWithoutArguments _ | VEffectWithArguments _ ->
      Format.fprintf ppf "<effect>"
    | VEffectContinue _ ->
      Format.fprintf ppf "<continue>" (* Used only for debugging and test. *)
    | VThrowingValue _ ->
      Format.fprintf ppf "throwing value" (* Used only for debugging and test. *)
  in
  helper ppf typ
;;

let pp_error ppf = function
  | `Division_by_zero -> Format.fprintf ppf "Error: division by zero."
  | `Unbound_variable name -> Format.fprintf ppf "Error: unbound variable '%s'." name
  | `Unbound_effect name -> Format.fprintf ppf "Error: unbound effect '%s'." name
  | `Type_error ->
    Format.fprintf ppf "Type error: please use type inference for more information."
  | `Unbound_handler id ->
    Format.fprintf ppf "Error: no suitable handler was found for effect '%s'." id
  | `Handler_without_continue name ->
    Format.fprintf
      ppf
      "Error: the handler for effect '%s' does not contain the expressions needed to \
       continue."
      name
  | `Not_continue_var name ->
    Format.fprintf ppf "Error: variable '%s' isn't continue variable." name
  | `Pattern_matching_failure ->
    Format.fprintf
      ppf
      "Error: pattern matching failure - the value does not match any pattern."
;;

let print_interpreter_error e =
  let error_str = Format.asprintf "%a" pp_error e in
  Format.printf "%s\n" error_str
;;

let value_to_string value = Format.asprintf "%a" pp_value value

let print_expr_value value typ =
  Format.printf "%s = %s \n" (expr_without_name typ) (value_to_string value)
;;

let print_program_value val_env typ_env names_list =
  Base.List.iter names_list ~f:(fun name ->
    let typ = Base.Map.find typ_env name in
    let value = Base.Map.find val_env name in
    match typ, value with
    | Some (Scheme (_, typ)), Some value ->
      Format.printf "%s = %s\n" (expr_with_name name typ) (value_to_string value)
    | _, _ -> Printf.printf "")
;;
