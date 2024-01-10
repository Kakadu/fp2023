open! Base
open Ast

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
