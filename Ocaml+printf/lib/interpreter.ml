(** Copyright 2023, aartdem *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

let rec convert_to_string = function
  | [] -> ""
  | h :: tl ->
    (match h with
     | Ast.FmtInt -> "%d" ^ convert_to_string tl
     | Ast.FmtBool -> "%B" ^ convert_to_string tl
     | Ast.FmtChar -> "%c" ^ convert_to_string tl
     | Ast.FmtString -> "%s" ^ convert_to_string tl
     | Ast.SimpleStr str -> str ^ convert_to_string tl)
;;

(* this printing method is purely for convenience *)
let pp_fstring ppf fstring = Format.fprintf ppf {|%S format|} (convert_to_string fstring)

type value =
  | VUnit
  | VInt of int
  | VBool of bool
  | VChar of char
  | VString of string
  | VFormat_string of Ast.fstring
  (** string option is None when function is non-recursive
      and Some _ when function is recursive *)
  | VFun of string option * Ast.pattern * Ast.expr * env_values
  | VTuple of value * value list
  | VList of value list

and env_values = (string, value, Base.String.comparator_witness) Base.Map.t

let rec pp_value ppf = function
  | VUnit -> Format.fprintf ppf "()"
  | VInt i -> Format.fprintf ppf "%d" i
  | VBool b -> Format.fprintf ppf "%B" b
  | VChar c -> Format.fprintf ppf {|%C|} c
  | VString s -> Format.fprintf ppf {|%S|} s
  | VFun _ -> Format.fprintf ppf "<fun>"
  | VTuple (h, list) ->
    Format.fprintf ppf "(";
    let rec helper = function
      | [] -> Format.fprintf ppf ")"
      | h :: tl ->
        let fmt =
          match tl with
          | [] -> format_of_string "%a"
          | _ -> format_of_string "%a, "
        in
        Format.fprintf ppf fmt pp_value h;
        helper tl
    in
    helper (h :: list)
  | VList list ->
    Format.fprintf ppf "[";
    let rec helper = function
      | [] -> Format.fprintf ppf "]"
      | h :: tl ->
        let fmt =
          match tl with
          | [] -> format_of_string "%a"
          | _ -> format_of_string "%a; "
        in
        Format.fprintf ppf fmt pp_value h;
        helper tl
    in
    helper list
    (*TODO: maybe change *)
  | VFormat_string t -> Format.fprintf ppf "%a" pp_fstring t
;;

type error =
  [ `Division_by_zero
  | `Matching_failure
  | `Invalid_argument of string
  | `Type_mismatch (* unreachable after type check *)
  | `No_variable (* unreachable after type check *)
  | `Impossible_state of string (* unreachable *)
  ]

let pp_error ppf : error -> unit = function
  | `Division_by_zero -> Format.fprintf ppf {|Division by zero|}
  | `Invalid_argument s -> Format.fprintf ppf {|Invalid argument: %s|} s
  | `Matching_failure -> Format.fprintf ppf {|Matching failure|}
  | `Impossible_state s -> Format.fprintf ppf {|Impossible interpreter state: %s|} s
  | `No_variable -> Format.fprintf ppf {|Undefined variable|}
  | `Type_mismatch -> Format.fprintf ppf {|Type mismatch|}
;;

module MONAD_WRITE_ERROR : sig
  type 'a t

  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val write : string -> unit t
  val fail : error -> 'a t
  val run : 'a t -> string * ('a, error) Base.Result.t

  include Base.Monad.Infix with type 'a t := 'a t

  module Syntax : sig
    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  end
end = struct
  open Base

  type 'a t = string -> string * ('a, error) Result.t

  let return x : 'a t = fun str -> str, Result.return x
  let write new_str : unit t = fun str -> str ^ new_str, Result.return ()
  let fail err : 'a t = fun str -> str, Result.fail err

  let ( >>= ) (m : 'a t) (f : 'a -> 'b t) : 'b t =
    fun str ->
    match m str with
    | str, Result.Error e -> str, Result.fail e
    | str, Result.Ok x -> f x str
  ;;

  let ( >>| ) (m : 'a t) (f : 'a -> 'b) : 'b t =
    fun str ->
    match m str with
    | str, Result.Error e -> str, Result.fail e
    | str, Result.Ok x -> str, Result.return (f x)
  ;;

  let bind x f = x >>= f
  let run m = m ""

  module Syntax = struct
    let ( let* ) x f = bind x f
  end
end

module EnvValues : sig
  val std : env_values
  val empty : env_values
  val find : env_values -> string -> value option
  val update : env_values -> string -> value -> env_values
  val pp_env_values : Stdlib.Format.formatter -> env_values -> unit
end = struct
  open Base

  let empty = Map.empty (module String)
  let find mp key = Map.find mp key
  let update mp key data = Map.update mp key ~f:(function _ -> data)

  let std =
    let open Ast in
    let val_length = VFun (None, Pat_val (LCIdent "s"), Expr_length, empty) in
    let val_get =
      VFun (None, Pat_val (LCIdent "s"), Expr_fun (Pat_val (LCIdent "i"), Expr_get), empty)
    in
    let val_format_of_str =
      VFun (None, Pat_val (LCIdent "s"), Expr_format_of_str, empty)
    in
    let val_printf = VFun (None, Pat_val (LCIdent "fmt"), Expr_printf, empty) in
    let init_env = empty in
    let init_env = update init_env "length" val_length in
    let init_env = update init_env "get" val_get in
    let init_env = update init_env "format_of_string" val_format_of_str in
    let init_env = update init_env "printf" val_printf in
    init_env
  ;;

  let pp_env_values ppf env_values =
    Map.iteri env_values ~f:(fun ~key ~data ->
      match find std key with
      | None ->
        Stdlib.Format.fprintf ppf "val %s = " key;
        pp_value ppf data;
        Stdlib.Format.fprintf ppf "\n"
      | Some _ -> Stdlib.Format.fprintf ppf "")
  ;;
end

open MONAD_WRITE_ERROR
open MONAD_WRITE_ERROR.Syntax

let rec match_pattern env = function
  | Ast.Pat_any, _ -> Some env
  | Ast.Pat_empty_list, VList [] -> Some env
  | Ast.Pat_const (Int i1), VInt i2 when i1 = i2 -> Some env
  | Ast.Pat_const (Bool b1), VBool b2 when b1 = b2 -> Some env
  | Ast.Pat_const (Char c1), VChar c2 when c1 = c2 -> Some env
  | Ast.Pat_const (String s1), VString s2 when s1 = s2 -> Some env
  | Ast.Pat_val (LCIdent name), v -> Some (EnvValues.update env name v)
  | Ast.Pat_tuple (pat1, pat_list), VTuple (v1, val_tuple) ->
    let f1 env p v =
      match env with
      | Some env -> match_pattern env (p, v)
      | None -> None
    in
    List.fold_left2 f1 (match_pattern env (pat1, v1)) pat_list val_tuple
  | Ast.Pat_cons_list (p1, p2), VList (h :: tl) ->
    (match match_pattern env (p1, h) with
     | Some env -> match_pattern env (p2, VList tl)
     | None -> None)
  | _ -> None
;;

let return_int i = return (VInt i)
let return_bool b = return (VBool b)
let return_char c = return (VChar c)
let return_string s = return (VString s)

let eval_binop p v1 v2 =
  let open Ast in
  match p, v1, v2 with
  | Add, VInt i1, VInt i2 -> return_int (i1 + i2)
  | Sub, VInt i1, VInt i2 -> return_int (i1 - i2)
  | Mul, VInt i1, VInt i2 -> return_int (i1 * i2)
  | Div, VInt i1, VInt i2 ->
    if i2 = 0 then fail `Division_by_zero else return_int (i1 / i2)
  | Eq, VInt i1, VInt i2 -> return_bool (i1 = i2)
  | Neq, VInt i1, VInt i2 -> return_bool (i1 <> i2)
  | Leq, VInt i1, VInt i2 -> return_bool (i1 <= i2)
  | Geq, VInt i1, VInt i2 -> return_bool (i1 >= i2)
  | Gre, VInt i1, VInt i2 -> return_bool (i1 > i2)
  | Less, VInt i1, VInt i2 -> return_bool (i1 < i2)
  | And, VBool b1, VBool b2 -> return_bool (b1 && b2)
  | Or, VBool b1, VBool b2 -> return_bool (b1 || b2)
  | Concat, VString s1, VString s2 -> return_string (s1 ^ s2)
  | Concat_format, VFormat_string s1, VFormat_string s2 ->
    return (VFormat_string (List.append s1 s2))
  | _, _, _ -> fail `Type_mismatch
;;

let eval_printf env =
  let open Ast in
  let* fmt =
    match EnvValues.find env "fmt" with
    | Some (VFormat_string fmt) -> return fmt
    | _ -> fail `No_variable
  in
  let rec gen_body num = function
    | [] -> Expr_formatted_printf fmt
    | h :: tl ->
      (match h with
       | FmtInt | FmtChar | FmtString | FmtBool ->
         Expr_fun (Pat_val (LCIdent ("arg" ^ Int.to_string num)), gen_body (num + 1) tl)
       | SimpleStr _ -> gen_body num tl)
  in
  match gen_body 1 fmt with
  | Expr_formatted_printf fmt ->
    let* () = write (convert_to_string fmt) in
    return VUnit
  | Expr_fun (pat, expr) -> return @@ VFun (None, pat, expr, EnvValues.empty)
  | _ -> fail @@ `Impossible_state "gen_body returns wrong value"
;;

let eval_formatted_printf env fmt =
  let open Ast in
  let rec helper num = function
    | [] -> return ""
    | h :: tl ->
      let arg = "arg" ^ Int.to_string num in
      let value = EnvValues.find env arg in
      let* s1, new_num =
        match h, value with
        | FmtInt, Some (VInt i) -> return (Int.to_string i, num + 1)
        | FmtBool, Some (VBool b) -> return (Bool.to_string b, num + 1)
        | FmtChar, Some (VChar c) -> return (Base.Char.to_string c, num + 1)
        | FmtString, Some (VString s) -> return (s, num + 1)
        | SimpleStr s, _ -> return (s, num)
        | _, _ -> fail @@ `No_variable
      in
      let* s2 = helper new_num tl in
      return (s1 ^ s2)
  in
  let* s = helper 1 fmt in
  let* () = write s in
  return VUnit
;;

let eval_get env =
  let s = EnvValues.find env "s" in
  let i = EnvValues.find env "i" in
  match s, i with
  | Some (VString s), Some (VInt i) ->
    if i < String.length s
    then return @@ VChar s.[i]
    else fail @@ `Invalid_argument "Index out of bounds"
  | _, _ -> fail `No_variable
;;

let eval_length env =
  let s = EnvValues.find env "s" in
  match s with
  | Some (VString s) -> return @@ VInt (String.length s)
  | _ -> fail `No_variable
;;

let eval_expr =
  let open Ast in
  let rec helper env = function
    | Expr_const e ->
      (match e with
       | Int i -> return_int i
       | Bool b -> return_bool b
       | Char c -> return_char c
       | String s -> return_string s)
    | Expr_val (LCIdent name) ->
      (match EnvValues.find env name with
       | Some x -> return x
       | None -> fail `No_variable)
    | Expr_empty_list -> return @@ VList []
    | Expr_fstring fmt -> return @@ VFormat_string fmt
    | Un_op (op, e) ->
      let* v = helper env e in
      (match op, v with
       | Un_plus, VInt i -> return_int i
       | Un_minus, VInt i -> return_int (-i)
       | _, _ -> fail `Type_mismatch)
    | Bin_op (op, e1, e2) ->
      let* e1 = helper env e1 in
      let* e2 = helper env e2 in
      eval_binop op e1 e2
    | Expr_ite (e1, e2, e3) ->
      let* e1 = helper env e1 in
      (match e1 with
       | VBool true ->
         let* e2 = helper env e2 in
         return e2
       | VBool false ->
         let* e3 = helper env e3 in
         return e3
       | _ -> fail `Type_mismatch)
    | Expr_let ((false, LCIdent name, e1), e2) ->
      let* v1 = helper env e1 in
      let env = EnvValues.update env name v1 in
      let* v2 = helper env e2 in
      return v2
    | Expr_let ((true, LCIdent name, e1), e2) ->
      let* v1 = helper env e1 in
      let v1 =
        match v1 with
        | VFun (None, p, e, env) -> VFun (Some name, p, e, env)
        | other -> other
      in
      let env = EnvValues.update env name v1 in
      let* v2 = helper env e2 in
      return v2
    | Expr_fun (p, e) -> return (VFun (None, p, e, env))
    | Expr_app (e1, e2) ->
      let* f = helper env e1 in
      let* arg = helper env e2 in
      (match f with
       | VFun (fun_name, pat, e, fun_env) ->
         let* new_env =
           match match_pattern fun_env (pat, arg) with
           | Some env -> return env
           | None -> fail @@ `Matching_failure
         in
         let new_env =
           match fun_name with
           | Some fun_name -> EnvValues.update new_env fun_name f
           | None -> new_env
         in
         helper new_env e
       | _ -> fail `Type_mismatch)
    | Expr_tuple (h, list) ->
      let* value1 = helper env h in
      let* value_list =
        List.fold_left
          (fun acc expr ->
            let* acc = acc in
            let* value = helper env expr in
            return (value :: acc))
          (return [])
          list
      in
      return @@ VTuple (value1, List.rev value_list)
    | Expr_cons_list (h, tl) ->
      let* h = helper env h in
      let* tl = helper env tl in
      (match tl with
       | VList tl -> return @@ VList (h :: tl)
       | _ -> fail `Type_mismatch)
    | Expr_match (e, list) ->
      let* v = helper env e in
      eval_match env v list
    | Expr_seq (e1, e2) ->
      let* _ = helper env e1 in
      let* v2 = helper env e2 in
      return v2
      (* below are the functions with hardcoded implementation *)
    | Expr_format_of_str ->
      let s = EnvValues.find env "s" in
      (match s with
       | Some (VFormat_string s) -> return @@ VFormat_string s
       | _ -> fail `No_variable)
    | Expr_get -> eval_get env
    | Expr_length -> eval_length env
    | Expr_printf -> eval_printf env
    | Expr_formatted_printf fstring -> eval_formatted_printf env fstring
  and eval_match env v = function
    | [] -> fail `Matching_failure
    | (pat, expr) :: tl ->
      let new_env = match_pattern env (pat, v) in
      (match new_env with
       | Some env -> helper env expr
       | None -> eval_match env v tl)
  in
  helper
;;

let eval_toplevel env = function
  | Ast.Let_decl (false, Ast.LCIdent name, expr) ->
    let* v = eval_expr env expr in
    let env = EnvValues.update env name v in
    return (env, v)
  | Ast.Let_decl (true, Ast.LCIdent name, expr) ->
    let* v = eval_expr env expr in
    let v =
      match v with
      | VFun (None, p, e, env) -> VFun (Some name, p, e, env)
      | other -> other
    in
    let env = EnvValues.update env name v in
    return (env, v)
  | Ast.Expr expr ->
    let* v = eval_expr env expr in
    return (env, v)
;;

let eval_program prog =
  List.fold_left
    (fun acc toplevel ->
      let* acc = acc in
      let* env, _ = eval_toplevel acc toplevel in
      return env)
    (return EnvValues.std)
    prog
;;

let run_eval_program prog =
  let out, env = run (eval_program prog) in
  Format.printf "%s" out;
  env
;;

(* it is mainly used for tests *)
let run_eval_expr expr =
  let out, value = run (eval_expr EnvValues.std expr) in
  Format.printf "%s" out;
  value
;;

(* TODO: remove tests *)
