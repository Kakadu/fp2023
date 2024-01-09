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

(** Represents type sharing for objects, which displayed using the [as] constract.
    A shared sub-term is represented by a type variable, when object's type is recursive (flag = [true])
    This makes sense since this language doesn't support subtyping *)
type constr = C of binder * bool [@@deriving eq, show { with_path = false }]

type ty =
  | TPrim of string (** Ground types: [int], [bool] *)
  | TVar of binder (** Variables: ['a], ['b] *)
  | TTuple of ty list (** [TTuple [t1;..;tn]] ==> [(t1 * ... * tn)]*)
  | TArrow of ty * ty (** [TArrow (t1, t2)] ==> [e1 -> e2] *)
  | TList of ty (** [['a; 'b; ...]] *)
  | TObject of ty * (string * ty) list * constr
  (** Represents:
      - [< .. > as 'a] when [constr] is [true]
      - [< .. >] when [constr] is [false]

      [TObject(`f1:t1;..;fn:tn',_,_)] ==> [< f1:t1; ...; fn:tn>]
      f1, fn are represented a linked list of types using TField and TNil constructors.

      The second parameter represents the instance variables. *)
  | TField of string * ty * ty (** [TField ("foo", t, ts)] ==> [<..; foo : t; ts>] *)
  | TNil (** [TNil] ==> [<...;>] *)
  | TPoly of binder (** Represents anonymous row variable [<;..>] *)
[@@deriving eq, show { with_path = false }]

type scheme = S of binder_set * ty [@@deriving show { with_path = false }]
