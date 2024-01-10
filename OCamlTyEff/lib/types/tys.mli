(** Copyright 2023, Danil S, Andrei *)

(** SPDX-License-Identifier: MIT *)

open! Base
open Vars
open Ast

module Eff : sig
  module Label : sig
    type t =
      | Label of Ident.t * Ident.t option
          (** Effect label. E.g. [console], [exn Division_by_zero] *)

    val pp : Format.formatter -> t -> unit

    val equal : t -> t -> bool
    val compare : t -> t -> int
    val sexp_of_t : t -> Sexp.t

    val console : t
    val exn : t
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
end

module Ty : sig
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

  val vars : t -> VarSet.t
  (** Type variables occuring in a type *)
end

module Scheme : sig
  (** Type with universally quantified type variables *)
  type t = Forall of VarSet.t * Ty.t

  val pp : Format.formatter -> t -> unit

  val compare : t -> t -> int
  val sexp_of_t : t -> Sexp.t

  val free_vars : t -> VarSet.t
  (** Free type variables in scheme *)
end
