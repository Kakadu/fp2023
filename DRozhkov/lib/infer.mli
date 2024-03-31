(** Copyright 2021-2023, Kakadu, RozhkovAleksandr *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type typ =
  | TInt (** int type *)
  | TBool (** bools type *)
  | TArrow of typ * typ (** type -> type *)
  | TVar of int (** var type *)
  | TList of typ (* list type *)

val pp_typ : Format.formatter -> typ -> unit

type error =
  | Occurs_check
  | No_variable of string
  | Unification_failed of typ * typ

val pp_error : Format.formatter -> error -> unit

module VarSet : sig
  module Using_comparator = Base.Set.Using_comparator

  type t = (int, Base.Int.comparator_witness) Using_comparator.t
end

type scheme = S of VarSet.t * typ

val run_infer
  :  Ast.statement list
  -> ((string, scheme, Base.String.comparator_witness) Base.Map.t, error) result
