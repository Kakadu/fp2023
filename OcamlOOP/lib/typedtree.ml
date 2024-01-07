(** Copyright 2023, Artem-Rzhankoff *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

module VarSet = struct
  include Stdlib.Set.Make (Int)

  let pp ppf s =
    Format.fprintf ppf "[ ";
    iter (Format.fprintf ppf "%d; ") s;
    Format.fprintf ppf "]"
  ;;
end

type binder = int [@@deriving eq, show { with_path = false }]
type binder_set = VarSet.t [@@deriving show { with_path = false }]

(** For each object, an alias is created during initialization, which is disabled by default (with flag = [false]).
    This is then used for represent rercursive types.
    *Of course this is bad way to implement recursive types in real world, but it's quite suitable for this project *)
type constr = C of binder * bool [@@deriving eq, show { with_path = false }]

type ty =
  | TPrim of string (** Ground types: [int], [bool] *)
  | TVar of binder (** Variables: ['a], ['b] *)
  | TTuple of ty list (** [TTuple [t1;..;tn]] ==> [(t1 * ... * tn)]*)
  | TArrow of ty * ty (** [TArrow (t1, t2)] ==> [e1 -> e2] *)
  | TList of ty (** [['a; 'b; ...]] *)
  | TObject of ty * (string * ty) list * constr
  (** Constr represents a constraint, when types of object are recursive.
      [only self calls and overriding are supported!  ] *)
  | TField of string * ty * ty (** [TField ("foo", t, ts)] ==> [<..; foo : t; ts>] *)
  | TNil (** [TNil] ==> [<...;>] *)
  | TPoly of binder (** Represents anonymous row variable [<;..>] *)
[@@deriving eq, show { with_path = false }]

type scheme = S of binder_set * ty [@@deriving show { with_path = false }]
