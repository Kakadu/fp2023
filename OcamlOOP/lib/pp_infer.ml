(** Copyright 2021-2023, Artem-Rzhankoff *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Typedtree

(* Convert binders for further replacement with strings
  Example: type '10 * '22 -> '11 will be replaced by '0 * '1 -> '2 *)
let reconstruct_binders ty =
  let open Base in
  let empty = Map.empty (module Int) in
  (* k - old binder, v - new binder*)
  let find_insert (k, v) binders =
    match Map.find binders k with
    | Some v -> v, binders
    | None -> v + 1, Map.update binders k ~f:(fun _ -> v)
  in
  let repls =
    let rec helper (last, acc) = function
      | TVar n -> find_insert (n, last) acc
      | TArrow (l, r) ->
        let last1, acc1 = helper (last, acc) l in
        helper (last1, acc1) r
      | TList t -> helper (last, acc) t
      | TTuple ts -> List.fold_left ts ~init:(last, acc) ~f:helper
      | TPrim _ -> last, acc
    in
    snd @@ helper (0, empty) ty
  in
  let rec construct = function
    | TVar n -> tvar (Map.find_exn repls n)
    | TArrow (l, r) -> tarrow (construct l) (construct r)
    | TList t -> tlist (construct t)
    | TTuple ts -> ttuple (List.map ts ~f:construct)
    | other -> other
  in
  construct ty
;;

let%expect_test _ =
  let ty = tarrow (tarrow (tvar 5) (tvar 10)) (ttuple [ tvar 11; tvar 29; tvar 33 ]) in
  let _ = print_string (show_ty (reconstruct_binders ty)) in
  [%expect
    {|
    TArrow (TArrow (TVar (0), TVar (1)), TTuple ([TVar (2); TVar (3); TVar (4)])) |}]
;;

(* [WARNING] Works correctly for strings of length <= 2 *)
let convert_to_string binder =
  let rec helper binder acc =
    if 26 > binder && binder >= 0
    then (
      let s = Char.chr (97 + binder) in
      Base.Char.to_string s ^ acc)
    else (
      let hd = Char.chr @@ (97 + (binder mod 26)) in
      helper ((binder / 26) - 1) (Format.sprintf "%c%s" hd acc))
  in
  helper binder ""
;;

let%expect_test _ =
  let _ = Format.printf "%s" (convert_to_string 5) in
  [%expect {| f |}]
;;

let%expect_test _ =
  let _ = Format.printf "%s" (convert_to_string 33) in
  [%expect {| ah |}]
;;

(* pos --> attribute *)
let pp_type ppf ty =
  let new_ty = reconstruct_binders ty in
  let open Format in
  let rec helper ppf = function
    | TVar n, _ -> fprintf ppf "'%s" (convert_to_string n)
    | TPrim s, _ -> Format.pp_print_string ppf s
    | TArrow (l, r), pos -> (* Format.fprintf ppf "(%a -> %a)" helper l helper r *)
      (match l with 
      | TArrow (_, _) -> Format.fprintf ppf "(%a) -> %a" helper (l, pos) helper (r, pos)
      | _ -> Format.fprintf ppf "%a -> %a" helper (l, pos) helper (r, pos))
    | TList t, pos -> Format.fprintf ppf "%a list" helper (t, pos)
    | TTuple ts, pos ->
      let pp_tuple ppf (ts, pos) = 
        fprintf
        ppf
        "%a"
        (pp_print_list
        ~pp_sep:(fun ppf () -> fprintf ppf " * ")
        (fun ppf t -> fprintf ppf "%a" helper (t, pos)))
        ts
      in
      if pos then 
        fprintf ppf "(%a)" pp_tuple (ts, pos)
      else fprintf ppf "%a" pp_tuple (ts, true) 
  in
  helper ppf (new_ty, false)
;;

let pp_program ppf env =
  List.iter
    (fun (v, S (_, ty)) -> Format.fprintf ppf "var %s: %a\n" v pp_type ty)
    (List.rev env)
;;

let map_binder t1 t2 binder =
  let rec helper acc = function
    | TVar n1, TVar n2 -> if n1 = binder then convert_to_string n2 else acc
    | TArrow (l1, r1), TArrow (l2, r2) ->
      let acc = helper acc (l1, l2) in
      helper acc (r1, r2)
    | TList t1, TList t2 -> helper acc (t1, t2)
    | TTuple ts1, TTuple ts2 -> List.fold_left helper acc (List.combine ts1 ts2)
    | _ ->
      acc (* Unreachable cases, because only diff between two tt are binders in vars *)
  in
  helper "" (t1, t2)
;;

let pp_error ppf = function
  | `Occurs_check (k, v) ->
    let v1 = reconstruct_binders v in
    let literal = map_binder v v1 k in
    Format.fprintf ppf "The type variable '%s occurs inside %a" literal pp_type v
  | `Unbound_variable s -> Format.fprintf ppf "Unbound value '%s'" s
  | `Unification_failed (l, r) ->
    Format.fprintf
      ppf
      "This expression has type %a but an expression was expected of type %a"
      pp_type
      l
      pp_type
      r
  | `Several_bounds s -> Format.fprintf ppf "Variable %s is bound several times" s
  | `Unreachable -> Format.fprintf ppf "Unreachable error"
  | `Not_implemented -> Format.fprintf ppf "Not implemented"
  | `No_variable_rec ->
    Format.fprintf ppf "Only variables are allowed as left-side of 'let rec'"
;;

let pp_subst ppf subst =
  let open Stdlib.Format in
  let open Base in
  fprintf
    ppf
    "[ %a ]"
    (pp_print_list
       ~pp_sep:(fun ppf () -> fprintf ppf " , ")
       (fun ppf (k, v) -> fprintf ppf "%d -> %a" k pp_type v))
    (Map.to_alist subst)
;;

let pp_scheme ppf = function
  | S (xs, t) -> Format.fprintf ppf "forall %a . %a" VarSet.pp xs pp_type t
;;

let pp_env ppf xs =
  Stdlib.Format.fprintf ppf "{| ";
  List.iter (fun (n, s) -> Stdlib.Format.fprintf ppf "%s -> %a; \n" n pp_scheme s) xs;
  Stdlib.Format.fprintf ppf "|}%!"
;;
