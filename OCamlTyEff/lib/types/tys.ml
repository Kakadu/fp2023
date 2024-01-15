(** Copyright 2023, Danil S, Andrei *)

(** SPDX-License-Identifier: MIT *)

open! Base
open Vars
open Ast

module rec Eff : sig
  module Label : sig
    type t =
      | Label of Ident.t * Ty.t option
          (** Effect label. E.g. [console], [exn Division_by_zero] *)

    val pp : Format.formatter -> t -> unit
    val equal : t -> t -> bool

    val console : unit -> t
    val ref : unit -> t
    val exn : Ty.t -> t
  end

  type t =
    | Eff_var of Var.t  (** Effect variable such as ['e] *)
    | Eff_total
        (** Total effect. Signifies the absence of any effect.
            Assigned to pure mathematical functions *)
    | Eff_row of Label.t * t
        (** Effect row. [Eff_row(lbl, eff)] represents [lbl | eff] *)

  val pp : Format.formatter -> t -> unit

  val equal : t -> t -> bool
  val compare : t -> t -> int
  val sexp_of_t : t -> Sexp.t

  val vars : t -> VarSet.t
  (** Effect variables occuring in an effect *)

  val contains : t -> Label.t -> bool
  (** Check if an effect contains specified label *)
end = struct
  module Label = struct
    type t = Label of Ident.t * Ty.t option [@@deriving eq, ord, sexp_of]

    let pp ppf (Label (Ident name, arg)) =
      let open Stdlib.Format in
      match arg with
      | None ->
          fprintf ppf "%s" name
      | Some arg ->
          fprintf ppf "%s %a" name Ty.pp arg

    let console () = Label (Ident "console", None)
    let ref () = Label (Ident "ref", None)
    let exn ty = Label (Ident "exn", Some ty)
  end

  type t = Eff_var of Var.t | Eff_total | Eff_row of Label.t * t
  [@@deriving eq, ord, sexp_of]

  let pp ppf =
    let open Stdlib.Format in
    let rec helper ppf = function
      | Eff_row (lbl, eff_rest) ->
          fprintf ppf ", %a%a" Label.pp lbl helper eff_rest
      | Eff_total ->
          fprintf ppf ""
      | Eff_var var ->
          fprintf ppf " | %a" Var.pp var
    in
    function
    | Eff_var var ->
        fprintf ppf "-%a" Var.pp var
    | Eff_total ->
        fprintf ppf ""
    | Eff_row (lbl, eff_rest) ->
        fprintf ppf "-[%a%a]" Label.pp lbl helper eff_rest

  let rec vars = function
    | Eff_var var ->
        VarSet.singleton_eff var
    | Eff_row (_, eff_rest) ->
        vars eff_rest
    | Eff_total ->
        VarSet.empty

  let rec contains eff lbl_to_find =
    match eff with
    | Eff_row (lbl, _) when Label.equal lbl_to_find lbl ->
        true
    | Eff_row (_, eff_rest) ->
        contains eff_rest lbl_to_find
    | Eff_var _ | Eff_total ->
        false
end

and Ty : sig
  type t =
    | Ty_var of Var.t  (** A type variable such as ['a] *)
    | Ty_arr of t * Eff.t * t
        (** [Ty_arr(T1, Eff, T2)] represents [T1 -Eff-> T2] *)
    | Ty_tuple of t list
        (** [Ty_tuple([T1 ; ... ; Tn])] represents [T1 * ... * Tn].
            Invariant: [n >= 2].
        *)
    | Ty_con of Ast.Ident.t * t list
        (** [Ty_con(ident, l)] represents:
              - [tconstr]               when [l=[]],
              - [T tconstr]             when [l=[T]],
              - [(T1, ..., Tn) tconstr] when [l=[T1 ; ... ; Tn]].
        *)

  val pp : Format.formatter -> t -> unit

  val equal : t -> t -> bool
  val compare : t -> t -> int
  val sexp_of_t : t -> Sexp.t

  val unit : t
  val int : t
  val bool : t
  val char : t
  val string : t

  val exn : t -> t
  (** Construct exception type. E.g. [_Exc1 exception] *)

  val ref : t -> t
  (** Construct ref type. E.g. [int ref] *)

  val vars : t -> VarSet.t
  (** Type variables occuring in a type *)
end = struct
  type t =
    | Ty_var of Var.t
    | Ty_arr of t * Eff.t * t
    | Ty_tuple of t list
    | Ty_con of Ident.t * t list
  [@@deriving eq, ord, sexp_of]

  let rec pp ppf =
    let open Stdlib.Format in
    let pp_raw = pp in
    let pp ppf ty =
      match ty with
      | Ty_arr _ | Ty_tuple _ ->
          (* wrap arrow and tuple types in parentheses *)
          fprintf ppf "(%a)" pp_raw ty
      | _ ->
          pp_raw ppf ty
    in
    function
    | Ty_var var ->
        Var.pp ppf var
    | Ty_arr (l, eff, r) ->
        fprintf ppf "%a %a-> %a" pp l Eff.pp eff pp_raw r
    | Ty_tuple tys ->
        let pp_tys =
          pp_print_list pp ~pp_sep:(fun out () -> fprintf out " * ")
        in
        fprintf ppf "%a" pp_tys tys
    | Ty_con (Ident name, args) -> (
        let pp_args =
          pp_print_list pp ~pp_sep:(fun out () -> fprintf out ", ")
        in
        match args with
        | [] ->
            fprintf ppf "%s" name
        | [arg] ->
            fprintf ppf "%a %s" pp arg name
        | _ ->
            fprintf ppf "(%a) %s" pp_args args name )

  let unit = Ty_con (Ident "unit", [])
  let int = Ty_con (Ident "int", [])
  let bool = Ty_con (Ident "bool", [])
  let char = Ty_con (Ident "char", [])
  let string = Ty_con (Ident "string", [])
  let exn ty = Ty_con (Ident "exception", [ty])
  let ref ty = Ty_con (Ident "ref", [ty])

  let rec vars = function
    | Ty_var x ->
        VarSet.singleton_ty x
    | Ty_arr (ty1, eff, ty2) ->
        VarSet.union_list [vars ty1; Eff.vars eff; vars ty2]
    | Ty_tuple tys ->
        List.map ~f:vars tys |> VarSet.union_list
    | Ty_con (_, tys) ->
        List.map ~f:vars tys |> VarSet.union_list
end

module Scheme : sig
  (** Type with universally quantified type variables *)
  type t = Forall of VarSet.t * Ty.t

  val pp : Format.formatter -> t -> unit

  val compare : t -> t -> int
  val sexp_of_t : t -> Sexp.t

  val free_vars : t -> VarSet.t
  (** Free type variables in scheme *)
end = struct
  type t = Forall of VarSet.t * Ty.t [@@deriving ord, sexp_of]

  let pp ppf (Forall (quantified, ty)) =
    let open Stdlib.Format in
    if VarSet.is_empty quantified then fprintf ppf "%a" Ty.pp ty
    else fprintf ppf "%a. %a" VarSet.pp quantified Ty.pp ty

  let free_vars (Forall (quantified, ty)) = VarSet.diff (Ty.vars ty) quantified
end
