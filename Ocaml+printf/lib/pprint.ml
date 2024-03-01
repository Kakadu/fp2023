(** Copyright 2023, aartdem *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(* match typed variables to letters *)
let assign_names typ =
  let calc_name letter num =
    "\'"
    ^
    if num = 0
    then Base.Char.to_string letter
    else Base.Char.to_string letter ^ Int.to_string num
  in
  let next_letter c = Base.Char.of_int_exn (Base.Char.to_int c + 1) in
  let next letter num =
    if Char.equal letter 'z' then 'a', num + 1 else next_letter letter, num
  in
  let rec helper names letter num = function
    | Typedtree.TVar n ->
      (match Base.Map.add names ~key:n ~data:(calc_name letter num) with
       | `Ok new_names -> new_names, next letter num
       | `Duplicate -> names, (letter, num))
    | TArr (l, r) ->
      let names, (letter, num) = helper names letter num l in
      helper names letter num r
    | TList t -> helper names letter num t
    | TTuple (h, list) ->
      List.fold_left
        (fun (names, (letter, num)) -> helper names letter num)
        (names, (letter, num))
        (h :: list)
    | _ -> names, (letter, num)
  in
  let names, (_, _) = helper (Base.Map.empty (module Base.Int)) 'a' 0 typ in
  names
;;

(* pretty print types with letters *)
let pp_typ ppf typ =
  let open Typedtree in
  let names = assign_names typ in
  let rec helper ppf = function
    | TVar n ->
      (try Format.fprintf ppf "%s" (Base.Map.find_exn names n) with
       | Base.Not_found_s _ | Stdlib.Not_found ->
         Format.fprintf ppf "Names for types are counted incorrectly")
    | TPrim s -> Format.fprintf ppf "%s" s
    | TArr (l, r) ->
      (match l, r with
       | TArr (_, _), _ -> Format.fprintf ppf "(%a) -> %a" helper l helper r
       | _ -> Format.fprintf ppf "%a -> %a" helper l helper r)
    | TUnit -> Format.fprintf ppf "unit"
    | TTuple (h, list) ->
      let print_item item fmt =
        match item with
        | TArr (_, _) | TTuple _ -> Format.fprintf ppf (fmt ^^ "(%a)") helper item
        | _ -> Format.fprintf ppf (fmt ^^ "%a") helper item
      in
      List.fold_left (fun _ item -> print_item item " * ") (print_item h "") list
    | TList t -> Format.fprintf ppf "%a list" helper t
    | TFString t -> Format.fprintf ppf "%a format_string" helper t
  in
  helper ppf typ
;;

(* print scheme without binded vars *)
let pp_scheme_binder ppf (Typedtree.Scheme (binder_set, typ)) =
  let open Typedtree in
  let names = assign_names typ in
  try
    if not (TypeVarSet.is_empty binder_set) then Format.fprintf ppf "forall";
    TypeVarSet.iter
      (fun n -> Format.fprintf ppf " %s" (Base.Map.find_exn names n))
      binder_set;
    if not (TypeVarSet.is_empty binder_set) then Format.fprintf ppf ". ";
    Format.fprintf ppf "%a" pp_typ typ
  with
  | Base.Not_found_s _ | Stdlib.Not_found ->
    Format.fprintf ppf "Binder set does not match the type"
;;

(* print scheme without binded vars *)
let pp_scheme_without ppf = function
  | Typedtree.Scheme (_, t) -> Format.fprintf ppf "%a" pp_typ t
;;
