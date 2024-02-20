(** Copyright 2023-2024, David Akhmedov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type const =
  | CInt of int (** int *)
  | CBool of bool (** bool *)
  | CNil (** [] *)
  | CUnit (** () *)
[@@deriving eq, show { with_path = false }]

type bin_op =
  | Add (** + *)
  | Sub (** - *)
  | Mul (** * *)
  | Div (** / *)
  | Leq (** <= *)
  | Less (** < *)
  | Geq (** >= *)
  | Gre (** > *)
  | Eq (** == *)
  | Neq (** != *)
  | And (** && *)
  | Or (** || *)
[@@deriving eq, show { with_path = false }]

type unary_op =
  | Plus (** ~+ *)
  | Minus (** ~- *)
  | Not (** ~not *)
[@@deriving eq, show { with_path = false }]

type ident = string [@@deriving eq, show { with_path = false }]

type capitalized_ident = Capitalized_ident of string (** Capitalized idents *)
[@@deriving eq, show { with_path = false }]

type rec_flag =
  | Recursive (** Recursive *)
  | Not_recursive (** Not recursive *)
[@@deriving eq, show { with_path = false }]

type pattern =
  | PId of ident (** x *)
  | PTuple of pattern list (** (x, y) *)
  | PList of pattern * pattern (** x :: xs *)
  | PConst of const (** 3 *)
[@@deriving eq, show { with_path = false }]

type expr =
  | EConst of const (** Const. Examples: 100; true *)
  | EBinop of expr * bin_op * expr
  (** Binary operation. Examples: 2 + 2; (234 * 234) + 234 *)
  | EUnop of unary_op * expr (** Unary operation. Examples: -(1); (+b) *)
  | EId of ident (** Identifier. Examples: a, b, c *)
  | EFun of pattern * expr (** Function. Examples: fun x -> x + 1 *)
  | EApp of expr * expr (** Application. Examples: f (x - 1) *)
  | EIf of expr * expr * expr
  (** If-then-else. Examples: if x >= y then x - y else y - x *)
  | EList of expr * expr (** Lists. Examples: [1; 2; 3] *)
  | ETuple of expr list (** Tuple. Examples: (1, 2, 3) *)
  | EClsr of decl * expr (** Closure. Examples: let inc x = x + 1 in inc 5*)
  | EMatch of expr * (pattern * expr) list
  (** Matching. Examples: match l with | hd::tl -> hd | _ -> [] *)
[@@deriving eq, show { with_path = false }]

and decl = DLet of rec_flag * ident * expr (** Let declarations *)
[@@deriving eq, show { with_path = false }]

type prog = decl list [@@deriving eq, show { with_path = false }]

let pid id = PId id
let ptuple p_list = PTuple p_list
let plist hd tl = PList (hd, tl)
let pconst c = PConst c
let econst c = EConst c
let ebinop e1 op e2 = EBinop (e1, op, e2)
let eunop op e = EUnop (op, e)
let eid i = EId i
let efun pat e = EFun (pat, e)
let eapp f args = EApp (f, args)
let eif e1 e2 e3 = EIf (e1, e2, e3)
let elist hd tl = EList (hd, tl)
let etuple l = ETuple l
let eclsr d e = EClsr (d, e)
let ematch e cl = EMatch (e, cl)
let dlet rf i e_let = DLet (rf, i, e_let)
let prog (d_l : decl list) : prog = d_l
