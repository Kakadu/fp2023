(** Copyright 2021-2023, LeonidElkin *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open InterpretTypes
open Format

let rec pp_val fmt = function
  | VInt n -> fprintf fmt "%d" n
  | VBool b -> fprintf fmt "%b" b
  | VChar c -> fprintf fmt "%c" c
  | VString s -> fprintf fmt "%S" s
  | VList l -> fprintf fmt "[%a]" (pp_list "; ") l
  | VTuple t -> fprintf fmt "(%a)" (pp_list ", ") t
  | VFun (_, _, _) -> fprintf fmt "<fun>"
  | VLet (_, b, _) -> if b then fprintf fmt "<let rec>" else fprintf fmt "<let>"

and pp_list sep fmt =
  let helper fmt = pp_list sep fmt in
  function
  | [] -> ()
  | [ h ] -> fprintf fmt "%a" pp_val h
  | h :: tl -> fprintf fmt "%a%S%a" pp_val h sep helper tl
;;

let pp_env fmt (e : enviorment) =
  IdMap.iter (fun key data -> fprintf fmt "%S: %a\n" key pp_val data) e
;;

let pp_fail fmt = function
  | UnboundVariable str -> fprintf fmt "UnboundVariable: %S" str
  | ValueTypeError verr -> fprintf fmt "ValueTypeError: %a" pp_val verr
  | ExprTypeError eerr -> fprintf fmt "TypeError: %S" eerr
  | DivisionByZero -> fprintf fmt "DivisionByZero"
  | ExecError (val1, val2) -> fprintf fmt "ExecError: %a # %a" pp_val val1 pp_val val2
  | PatternError perr -> fprintf fmt "PatternError: %S" perr
;;

let pp_expected_result_item fmt item =
  let item_name, item_value = item in
  fprintf fmt "%S: %a" item_name pp_val item_value
;;

let rec pp_expected_result fmt = function
  | [] -> ()
  | h :: tl -> fprintf fmt "%a\n%a" pp_expected_result_item h pp_expected_result tl
;;
