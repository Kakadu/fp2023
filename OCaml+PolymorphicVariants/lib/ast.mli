(** Copyright 2023-2024, Nikita Nemakin *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(** (a-z|_) {a-z|0-9|_}*)
type id = string [@@deriving show { with_path = false }]

(** Used in {let} expr *)
type rec_flag =
  | Rec (** recursive *)
  | Nonrec (** non-recursive *)
[@@deriving show { with_path = false }]

type bin_op =
  | Mul (** * *)
  | Div (** / *)
  | Add (** + *)
  | Sub (** -  *)
  | Eq (** = *)
  | Neq (** <> *)
  | Lt (** < *)
  | Lte (** <= *)
  | Gt (** > *)
  | Gte (** >= *)
  | And (** && *)
  | Or (** || *)
[@@deriving show { with_path = false }]

type const =
  | CInt of int (** 123 *)
  | CBool of bool (** true | false *)
  | CString of string (** "string" *)
  | CUnit (** () *)
  | CNil (** [] *)
[@@deriving show { with_path = false }]

type pattern =
  | PAny (** _ *)
  | PConst of const (** 123, true, "string" *)
  | PVar of id (** x *)
  | PTuple of pattern list (** p1,..., pn *)
  | PCons of pattern * pattern (** p1 :: p2 *)
  | PPoly of id * pattern option (** `Tag_name p *)
[@@deriving show { with_path = false }]

type expr =
  | EConst of const (** 123, true, "string" *)
  | EVar of id (** x *)
  | EBin_op of bin_op * expr * expr (** 1 + 2 *)
  | EIf of expr * expr * expr (** if e1 then e2 else e3 *)
  | EMatch of expr * case list (** match e with p1 -> e1 |...| pn -> en *)
  | ELet of rec_flag * binding * expr (** let x = e1 in e2 *)
  | EFun of pattern * expr (** fun p -> e *)
  | ETuple of expr list (** a, b, c *)
  | ECons of expr * expr (** x :: xs | [x1; x2]*)
  | EApply of expr * expr (** f e *)
  | EPoly of id * expr option (** `Tag_name expr *)
[@@deriving show { with_path = false }]

(** Used in {match} expr *)
and case = pattern * expr [@@deriving show { with_path = false }]

(** Used in {let} expr *)
and binding = pattern * expr [@@deriving show { with_path = false }]

type str_item =
  | SEval of expr (** Some expression *)
  | SValue of rec_flag * binding (** let [rec] p = e *)
[@@deriving show { with_path = false }]

(** Sequence of structure items *)
type structure = str_item list [@@deriving show { with_path = false }]
