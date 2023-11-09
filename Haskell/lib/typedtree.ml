type binder = int [@@deriving show { with_path = false }]

module VarSet = struct
  include Stdlib.Set.Make (Int)

  let pp ppf s =
    Format.fprintf ppf "[ ";
    iter (Format.fprintf ppf "%d; ") s;
    Format.fprintf ppf "]"
  ;;
end

type binder_set = VarSet.t [@@deriving show { with_path = false }]

type ty =
  | TyLit of string
  | TyVar of binder
  | TyArrow of ty * ty
  | TyList of ty
  | TyTuple of ty list
[@@deriving show { with_path = false }]


type scheme = S of binder_set * ty [@@deriving show { with_path = false }]

let arrow l r = TyArrow (l, r)
let int_typ = TyLit "int"
let bool_typ = TyLit "bool"
let v x = TyVar x
let list_typ t = TyList t
let tuple_typ ts = TyTuple ts

let ( @-> ) = arrow
