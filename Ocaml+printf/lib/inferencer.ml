(** Copyright 2023, aartdem *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(* Solution template was taken from
   https://gitlab.com/Kakadu/fp2020course-materials/-/blob/master/code/miniml/inferencer.ml?ref_type=heads
   Bug with generalize was fixed
   Lists have been replaced with maps
   Names have been changed in many places to increase readability *)

open Typedtree

type error =
  [ `Occurs_check
  | `No_variable of string
  | `Unification_failed of typ * typ
  | `Multiple_bound of string
  | `Invalid_format_str of string
  | `Ivalid_format_concat of typ * typ
  | `Impossible_state of string
  | `Unexpected_expr of Ast.expr
  ]

let pp_error ppf : error -> unit = function
  | `Occurs_check -> Format.fprintf ppf {|Occurs check failed|}
  | `No_variable s -> Format.fprintf ppf {|Undefined variable "%s"|} s
  | `Unification_failed (l, r) ->
    Format.fprintf ppf {|Unification failed on %a and %a|} Pprint.pp_typ l Pprint.pp_typ r
  | `Multiple_bound s ->
    Format.fprintf ppf {|Variable %s is bound several times in matching|} s
  | `Invalid_format_str s -> Format.fprintf ppf {|Invalid format string "%s"|} s
  | `Ivalid_format_concat (t1, t2) ->
    Format.fprintf
      ppf
      {|Invalid format concatination of "%a" and "%a"|}
      Pprint.pp_typ
      t1
      Pprint.pp_typ
      t2
  | `Impossible_state s ->
    Format.fprintf ppf {|Impossible inference algorithm state: %s|} s
  | `Unexpected_expr expr ->
    Format.fprintf
      ppf
      {|Unexpected expression on type inference stage: %a|}
      Ast.pp_expr
      expr
;;

module R : sig
  type 'a t

  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val fail : error -> 'a t

  include Base.Monad.Infix with type 'a t := 'a t

  module Syntax : sig
    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  end

  module RList : sig
    val fold_left : 'a list -> init:'b t -> f:('b -> 'a -> 'b t) -> 'b t
    val fold_right : 'a list -> init:'b t -> f:('a -> 'b -> 'b t) -> 'b t
  end

  module RMap : sig
    val fold : ('a, 'b, 'c) Base.Map.t -> init:'d t -> f:('d -> 'a -> 'b -> 'd t) -> 'd t
  end

  (** Creation of a fresh name from internal state *)
  val fresh : int t

  (** Running a transformer: getting the inner result value *)
  val run : 'a t -> ('a, error) Base.Result.t
end = struct
  open Base

  (* A compositon: State monad after Result monad *)
  type 'a t = int -> int * ('a, error) Result.t

  let return x : 'a t = fun var -> var, Result.return x
  let fail e : 'a t = fun var -> var, Result.fail e
  let fresh : 'a t = fun var -> var + 1, Result.return var

  let ( >>= ) (m : 'a t) (f : 'a -> 'b t) : 'b t =
    fun var ->
    match m var with
    | var, Result.Error err -> var, Result.fail err
    | var, Result.Ok x -> f x var
  ;;

  let bind x f = x >>= f

  let ( >>| ) (m : 'a t) (f : 'a -> 'b) : 'b t =
    fun var ->
    match m var with
    | var, Result.Error err -> var, Result.fail err
    | var, Result.Ok x -> var, Result.return (f x)
  ;;

  module Syntax = struct
    let ( let* ) x f = bind x f
  end

  module RMap = struct
    let fold mp ~init ~f =
      let open Syntax in
      Map.fold mp ~init ~f:(fun ~key:k ~data:v acc ->
        let* acc = acc in
        f acc k v)
    ;;
  end

  module RList = struct
    let fold_left xs ~init ~f =
      let open Syntax in
      List.fold_left xs ~init ~f:(fun acc x ->
        let* acc = acc in
        f acc x)
    ;;

    let fold_right xs ~init ~f =
      let open Syntax in
      List.fold_right xs ~init ~f:(fun x acc ->
        let* acc = acc in
        f x acc)
    ;;
  end

  (* 2 because we have two functions that accept typed variables (see below) *)
  let run m = snd (m 2)
end

module Type = struct
  let rec occurs_in v = function
    | TVar b -> b = v
    | TArr (l, r) -> occurs_in v l || occurs_in v r
    | TTuple (h, list) ->
      List.fold_left (fun occurs item -> occurs || occurs_in v item) (occurs_in v h) list
    | TList t -> occurs_in v t
    | TFString t -> occurs_in v t
    | _ -> false
  ;;

  let type_vars =
    let rec helper acc = function
      | TVar b -> TypeVarSet.add b acc
      | TArr (l, r) -> helper (helper acc l) r
      | TTuple (h, list) ->
        List.fold_left (fun acc item -> helper acc item) (helper acc h) list
      | TList t -> helper acc t
      | TFString t -> helper acc t
      | _ -> acc
    in
    helper TypeVarSet.empty
  ;;
end

module Subst : sig
  type t

  val empty : t
  val singleton : int -> typ -> t R.t
  val remove : t -> int -> t
  val apply : t -> typ -> typ
  val unify : typ -> typ -> t R.t
  val pp_subst : Format.formatter -> t -> unit

  (** Compositon of substitutions *)
  val compose : t -> t -> t R.t

  val compose_all : t list -> t R.t
end = struct
  open R
  open R.Syntax
  open Base

  type t = (int, typ, Int.comparator_witness) Map.t

  let empty = Map.empty (module Int)

  let singleton k v =
    if Type.occurs_in k v
    then fail `Occurs_check
    else return (Map.singleton (module Int) k v)
  ;;

  let find mp ty = Map.find mp ty
  let remove mp ty = Map.remove mp ty

  let apply sub =
    let rec helper = function
      | TVar tv as ty ->
        (match find sub tv with
         | None -> ty
         | Some x -> x)
      | TArr (l, r) -> TArr (helper l, helper r)
      | TTuple (h, list) -> TTuple (helper h, List.map list ~f:(fun item -> helper item))
      | TList t -> TList (helper t)
      | TFString t -> TFString (helper t)
      | other -> other
    in
    helper
  ;;

  let rec unify l r =
    match l, r with
    | TPrim l, TPrim r when String.equal l r -> return empty
    | TVar a, TVar b when Int.equal a b -> return empty
    | TVar b, t | t, TVar b -> singleton b t
    | TArr (l1, r1), TArr (l2, r2) ->
      let* sub1 = unify l1 l2 in
      let* sub2 = unify (apply sub1 r1) (apply sub1 r2) in
      compose sub1 sub2
    | TUnit, TUnit -> return empty
    | TTuple (h1, list1), TTuple (h2, list2) ->
      let* unified =
        match List.map2 list1 list2 ~f:(fun t1 t2 -> unify t1 t2) with
        | Unequal_lengths ->
          fail (`Unification_failed (TTuple (h1, list1), TTuple (h2, list2)))
        | Ok res -> return res
      in
      List.fold_left unified ~init:(unify h1 h2) ~f:(fun acc s ->
        let* s = s in
        let* acc = acc in
        compose acc s)
    | TList t1, TList t2 -> unify t1 t2
    | TFString t1, TFString t2 -> unify t1 t2
    | _ -> fail (`Unification_failed (l, r))

  and extend sub typ_var ty =
    match Map.find sub typ_var with
    | None ->
      let* new_sub = singleton typ_var (apply sub ty) in
      let upd ~key ~data acc =
        let* acc = acc in
        let ty = apply new_sub data in
        return (Map.update acc key ~f:(function _ -> ty))
      in
      Map.fold sub ~init:(return new_sub) ~f:upd
    | Some finded_type ->
      let* sub2 = unify ty finded_type in
      compose sub sub2

  and compose s1 s2 = RMap.fold s2 ~init:(return s1) ~f:extend

  let compose_all ss = RList.fold_left ss ~init:(return empty) ~f:compose

  let pp_subst ppf sub =
    Base.Map.iteri sub ~f:(fun ~key ~data ->
      Stdlib.Format.fprintf ppf "[%d = %a] " key Pprint.pp_typ data)
  ;;
end

module Scheme = struct
  let free_vars (Scheme (bind_set, t)) = TypeVarSet.diff (Type.type_vars t) bind_set

  let apply sub (Scheme (bind_set, ty)) =
    let sub = TypeVarSet.fold (fun v s -> Subst.remove s v) bind_set sub in
    Scheme (bind_set, Subst.apply sub ty)
  ;;
end

module TypeEnv : sig
  type t

  val empty : t
  val std : t
  val update : t -> string -> scheme -> t
  val remove : t -> string -> t
  val free_vars : t -> TypeVarSet.t
  val apply : Subst.t -> t -> t
  val find : t -> string -> scheme option
  val union : t -> t -> t
  val pp_env : Format.formatter -> t -> unit
end = struct
  open Base

  type t = (string, scheme, String.comparator_witness) Map.t

  (* overwrite existing *)
  let update mp k v = Map.update mp k ~f:(function _ -> v)
  let empty = Map.empty (module String)

  (** Environment with several supported functions with implementation and type are hard coded *)
  let std =
    let init_env = empty in
    let init_env =
      update init_env "length" (Scheme (TypeVarSet.empty, string_typ @-> int_typ))
    in
    let init_env =
      update
        init_env
        "get"
        (Scheme (TypeVarSet.empty, string_typ @-> int_typ @-> char_typ))
    in
    let init_env =
      update
        init_env
        "printf"
        (Scheme (TypeVarSet.singleton 0, format_typ (type_var 0) @-> type_var 0))
    in
    let init_env =
      update
        init_env
        "format_of_string"
        (Scheme
           (TypeVarSet.singleton 1, format_typ (type_var 1) @-> format_typ (type_var 1)))
    in
    init_env
  ;;

  let free_vars env =
    Map.fold env ~init:TypeVarSet.empty ~f:(fun ~key:_ ~data acc ->
      TypeVarSet.union acc (Scheme.free_vars data))
  ;;

  let apply sub env = Map.map env ~f:(Scheme.apply sub)
  let find mp k = Map.find mp k
  let remove mp k = Map.remove mp k

  let union env1 env2 =
    Map.fold env2 ~init:env1 ~f:(fun ~key ~data acc -> update acc key data)
  ;;

  let pp_env ppf env =
    Map.iteri env ~f:(fun ~key ~data ->
      match find std key with
      | None ->
        Stdlib.Format.fprintf ppf "val %s : " key;
        Pprint.pp_scheme_binder ppf data;
        Stdlib.Format.fprintf ppf "\n"
      | Some _ -> Stdlib.Format.fprintf ppf "")
  ;;
end

open R
open R.Syntax

let fresh_var = fresh >>| fun n -> TVar n

let instantiate (Scheme (bind_set, ty)) =
  TypeVarSet.fold
    (fun cur_var acc_typ ->
      let* acc_typ = acc_typ in
      let* f1 = fresh_var in
      let* s = Subst.singleton cur_var f1 in
      return (Subst.apply s acc_typ))
    bind_set
    (return ty)
;;

(* creating a scheme out of a type *)
let generalize env ty =
  let free = TypeVarSet.diff (Type.type_vars ty) (TypeEnv.free_vars env) in
  Scheme (free, ty)
;;

(* add for recursion functions handling *)
let generalize_rec env ty exept =
  let env = TypeEnv.remove env exept in
  let free = TypeVarSet.diff (Type.type_vars ty) (TypeEnv.free_vars env) in
  Scheme (free, ty)
;;

let convert_to_format s : Ast.fstring option =
  let open Ast in
  let rec helper acc j =
    match String.rindex_from_opt s j '%' with
    | Some i when i < j ->
      let str = String.sub s i (j - i + 1) in
      let text = String.sub str 2 (j - i - 1) in
      let type_spec =
        match s.[i + 1] with
        | 'd' -> Some FmtInt
        | 'c' -> Some FmtChar
        | 's' -> Some FmtString
        | 'B' -> Some FmtBool
        | _ -> None
      in
      (match type_spec with
       | Some ty ->
         let ty = if String.length text = 0 then [ ty ] else ty :: [ SimpleStr text ] in
         if i > 0 then helper (List.append ty acc) (i - 1) else Some (List.append ty acc)
       | None -> None)
    | Some _ -> None
    | None -> Some (SimpleStr (String.sub s 0 (j + 1)) :: acc)
  in
  if String.exists (fun c -> c = '%') s
  then helper [] (String.length s - 1)
  else Some [ Ast.SimpleStr s ]
;;

let rec infer_format_type = function
  | [] -> unit_typ
  | h :: tl ->
    (match h with
     | Ast.FmtBool -> bool_typ @-> infer_format_type tl
     | Ast.FmtInt -> int_typ @-> infer_format_type tl
     | Ast.FmtChar -> char_typ @-> infer_format_type tl
     | Ast.FmtString -> string_typ @-> infer_format_type tl
     | Ast.SimpleStr _ -> infer_format_type tl)
;;

let infer_format_concat fstring1 fstring2 =
  let rec helper = function
    | TUnit -> return fstring2
    | TArr (TPrim s, r) ->
      let* r = helper r in
      return @@ TPrim s @-> r
    | _ -> fail @@ `Impossible_state "Try to concatination invalid format strings"
  in
  let* res = helper fstring1 in
  return @@ format_typ res
;;

let infer_const = function
  | Ast.Int _ -> int_typ
  | Ast.Bool _ -> bool_typ
  | Ast.Char _ -> char_typ
  | Ast.String _ -> string_typ
;;

(* inference type of pattern and extend environment with values names *)
let rec infer_pattern env = function
  | Ast.Pat_val (Ast.LCIdent name) ->
    let* fresh = fresh_var in
    (match TypeEnv.find env name with
     | Some _ -> fail (`Multiple_bound name)
     | None ->
       let sheme = Scheme (TypeVarSet.empty, fresh) in
       let env = TypeEnv.update env name sheme in
       return (Subst.empty, fresh, env))
  | Ast.Pat_any ->
    let* fresh = fresh_var in
    return (Subst.empty, fresh, env)
  | Ast.Pat_const c ->
    let fresh = infer_const c in
    return (Subst.empty, fresh, env)
  | Ast.Pat_empty_list ->
    let* fresh = fresh_var in
    let fresh = TList fresh in
    return (Subst.empty, fresh, env)
  | Ast.Pat_cons_list (l1, r1) ->
    let* sub1, typ1, env1 = infer_pattern env l1 in
    let* sub2, typ2, env2 = infer_pattern (TypeEnv.apply sub1 env1) r1 in
    let* fresh = fresh_var in
    let* sub_uni = Subst.unify typ2 (TList fresh) in
    let typ2 = Subst.apply sub_uni typ2 in
    let* sub3 = Subst.unify (TList typ1) typ2 in
    let* final_sub = Subst.compose_all [ sub1; sub2; sub3; sub_uni ] in
    return (final_sub, Subst.apply sub3 typ2, env2)
  | Ast.Pat_tuple (h, list) ->
    let* sub1, typ1, env1 = infer_pattern env h in
    let f1 pat (sub_prev, l, env) =
      let* sub_cur, arg, env = infer_pattern env pat in
      let* sub = Subst.compose sub_prev sub_cur in
      return (sub, arg :: l, env)
    in
    let* sub, arg, env = RList.fold_right list ~init:(return (sub1, [], env1)) ~f:f1 in
    return (sub, TTuple (typ1, arg), env)
;;

(*in addition to type inference, modifying AST by converting
  some string literals to formatted strings in certan places *)
let rec infer_expr env expr =
  let open Ast in
  match expr with
  | Expr_const c ->
    let ty = infer_const c in
    return (Subst.empty, ty, expr)
  | Expr_empty_list ->
    let* ty = fresh_var in
    return (Subst.empty, TList ty, expr)
  | Expr_val (LCIdent name) ->
    (match TypeEnv.find env name with
     | Some scheme ->
       let* ans = instantiate scheme in
       return (Subst.empty, ans, expr)
     | None -> fail @@ `No_variable name)
  | Un_op (_, e) ->
    let* return_type = fresh_var in
    let* sub1, typ1, expr = infer_expr env e in
    let* sub2 = Subst.unify (TArr (typ1, return_type)) int_typ in
    let* final_sub = Subst.compose sub1 sub2 in
    return (final_sub, Subst.apply sub2 return_type, expr)
  | Bin_op (op, e1, e2) -> infer_bin_op env op e1 e2
  | Expr_fun (pattern, e) ->
    let* sub1, typ1, new_env = infer_pattern TypeEnv.empty pattern in
    let env = TypeEnv.union env new_env in
    let* sub2, typ2, expr = infer_expr (TypeEnv.apply sub1 env) e in
    let arg_type = Subst.apply sub2 typ1 in
    let* final_sub = Subst.compose sub1 sub2 in
    return (final_sub, arg_type @-> typ2, Expr_fun (pattern, expr))
  | Expr_app (e1, e2) -> infer_app env e1 e2
  | Expr_let ((false, LCIdent name, e1), e2) ->
    let* sub1, typ1, expr1 = infer_expr env e1 in
    let env = TypeEnv.apply sub1 env in
    let gen_scheme = generalize env typ1 in
    let env = TypeEnv.update env name gen_scheme in
    let* sub2, typ2, expr2 = infer_expr env e2 in
    let* final_sub = Subst.compose sub1 sub2 in
    return (final_sub, typ2, Expr_let ((false, LCIdent name, expr1), expr2))
  | Expr_let ((true, LCIdent name, e1), e2) ->
    let* ty = fresh_var in
    let env = TypeEnv.update env name (Scheme (TypeVarSet.empty, ty)) in
    let* sub1, typ1, expr1 = infer_expr env e1 in
    let* sub2 = Subst.unify typ1 ty in
    let* sub3 = Subst.compose sub1 sub2 in
    let env = TypeEnv.apply sub3 env in
    let typ1 = Subst.apply sub3 typ1 in
    let gen_scheme = generalize_rec env typ1 name in
    let env = TypeEnv.update env name gen_scheme in
    let* sub4, typ2, expr2 = infer_expr env e2 in
    let* final_sub = Subst.compose sub3 sub4 in
    return (final_sub, typ2, Expr_let ((true, LCIdent name, expr1), expr2))
  | Expr_ite (e1, e2, e3) ->
    let* sub1, typ1, expr1 = infer_expr env e1 in
    let* sub2, typ2, expr2 = infer_expr (TypeEnv.apply sub1 env) e2 in
    let* sub3, typ3, expr3 = infer_expr (TypeEnv.apply sub2 env) e3 in
    let* sub_cond = Subst.unify typ1 bool_typ in
    let* sub_branches = Subst.unify typ2 typ3 in
    let* final_sub = Subst.compose_all [ sub1; sub2; sub3; sub_cond; sub_branches ] in
    return (final_sub, Subst.apply sub_branches typ2, Expr_ite (expr1, expr2, expr3))
  | Expr_tuple (expr1, list) ->
    let* sub1, typ1, expr1 = infer_expr env expr1 in
    let env = TypeEnv.apply sub1 env in
    let f1 prev cur_expr =
      let* prev_env, prev_subst, prev_typ, prev_expr = prev in
      let* cur_subst, cur_typ, cur_expr = infer_expr prev_env cur_expr in
      let* next_subst = Subst.compose prev_subst cur_subst in
      let next_env = TypeEnv.apply next_subst prev_env in
      return (next_env, next_subst, cur_typ :: prev_typ, cur_expr :: prev_expr)
    in
    let* _, final_sub, typ_list, expr_list =
      List.fold_left f1 (return (env, sub1, [], [])) list
    in
    let typ_list =
      List.map (fun item -> Subst.apply final_sub item) (List.rev typ_list)
    in
    let expr_list = List.rev expr_list in
    return
      ( final_sub
      , TTuple (Subst.apply final_sub typ1, typ_list)
      , Expr_tuple (expr1, expr_list) )
  | Expr_cons_list (e1, e2) ->
    let* sub1, typ1, expr1 = infer_expr env e1 in
    let* sub2, typ2, expr2 = infer_expr (TypeEnv.apply sub1 env) e2 in
    let* sub3 = Subst.unify (TList typ1) typ2 in
    let* final_sub = Subst.compose_all [ sub1; sub2; sub3 ] in
    return (final_sub, Subst.apply sub3 typ2, Expr_cons_list (expr1, expr2))
  | Expr_match (e, list) ->
    let* sub, arg_typ, e = infer_expr env e in
    let* ret_typ = fresh_var in
    let env = TypeEnv.apply sub env in
    let f1 (cur_pat, cur_expr) (last_sub, last_pat_typ, last_ret_typ, pat_expr_list) =
      let* last_sub = last_sub in
      let* cur_sub, cur_arg_typ, new_env = infer_pattern TypeEnv.empty cur_pat in
      let env = TypeEnv.union env new_env in
      let* sub_ret, cur_ret_typ, cur_expr =
        infer_expr (TypeEnv.apply cur_sub env) cur_expr
      in
      let cur_arg_typ = Subst.apply sub_ret cur_arg_typ in
      let* sub_uni1 = Subst.unify cur_arg_typ last_pat_typ in
      let* sub_uni2 = Subst.unify cur_ret_typ last_ret_typ in
      return
        ( Subst.compose_all [ last_sub; cur_sub; sub_ret; sub_uni1; sub_uni2 ]
        , Subst.apply sub_uni1 cur_arg_typ
        , Subst.apply sub_uni2 cur_ret_typ
        , (cur_pat, cur_expr) :: pat_expr_list )
    in
    let* sub, _, ret_typ, pat_expr_list =
      RList.fold_right list ~init:(return (return sub, arg_typ, ret_typ, [])) ~f:f1
    in
    let* sub = sub in
    return (sub, Subst.apply sub ret_typ, Expr_match (e, pat_expr_list))
  | Expr_seq (e1, e2) ->
    let* sub1, typ1, expr1 = infer_expr env e1 in
    let* sub2 = Subst.unify typ1 unit_typ in
    let* sub3, typ2, expr2 = infer_expr env e2 in
    let* final_sub = Subst.compose_all [ sub1; sub2; sub3 ] in
    return (final_sub, typ2, Expr_seq (expr1, expr2))
  | expr -> fail @@ `Unexpected_expr expr

and infer_bin_op env op e1 e2 =
  let open Ast in
  let* return_type = fresh_var in
  let* sub1, typ1, expr1 =
    match op, e1 with
    (* first case when we need to convert string literal *)
    | Concat_format, Expr_const (String s1) ->
      (match convert_to_format s1 with
       | Some fmt_s1 ->
         return
         @@ (Subst.empty, format_typ (infer_format_type fmt_s1), Expr_fstring fmt_s1)
       | None -> fail @@ `Invalid_format_str s1)
    | _ -> infer_expr env e1
  in
  let* sub2, typ2, expr2 =
    match op, e2 with
    (* second case when we need to convert string literal *)
    | Concat_format, Expr_const (String s2) ->
      (match convert_to_format s2 with
       | Some fmt_s2 ->
         return (Subst.empty, format_typ (infer_format_type fmt_s2), Expr_fstring fmt_s2)
       | None -> fail @@ `Invalid_format_str s2)
    | _ -> infer_expr (TypeEnv.apply sub1 env) e2
  in
  let* op_typ =
    match op with
    | Add | Sub | Mul | Div -> return (int_typ @-> int_typ @-> int_typ)
    | Eq | Neq | Leq | Geq | Gre | Less -> return (int_typ @-> int_typ @-> bool_typ)
    | And | Or -> return (bool_typ @-> bool_typ @-> bool_typ)
    | Concat -> return (string_typ @-> string_typ @-> string_typ)
    | Concat_format ->
      (match typ1, typ2 with
       | TFString fmt_s1, TFString fmt_s2 ->
         let* res_typ = infer_format_concat fmt_s1 fmt_s2 in
         return (format_typ fmt_s1 @-> format_typ fmt_s2 @-> res_typ)
       | _ -> fail @@ `Ivalid_format_concat (typ1, typ2))
  in
  let* sub3 = Subst.unify (typ1 @-> typ2 @-> return_type) op_typ in
  let* final_sub = Subst.compose_all [ sub1; sub2; sub3 ] in
  return (final_sub, Subst.apply sub3 return_type, Bin_op (op, expr1, expr2))

and infer_app env e1 e2 =
  let open Ast in
  let* return_type = fresh_var in
  let* sub1, typ1, expr1 = infer_expr env e1 in
  let* sub2, typ2, expr2 =
    match typ1, e2 with
    (* case of applying function that expects format string as argument,
       need to convert when argument is string literal *)
    | TArr (TFString _, _), Expr_const (String s) ->
      let fmt_s = convert_to_format s in
      (match fmt_s with
       | None -> fail @@ `Invalid_format_str s
       | Some fmt_s ->
         let ty = format_typ (infer_format_type fmt_s) in
         return (Subst.empty, ty, Expr_fstring fmt_s))
    | _, e2 -> infer_expr (TypeEnv.apply sub1 env) e2
  in
  let typ1 = Subst.apply sub2 typ1 in
  let* sub3 = Subst.unify typ1 (typ2 @-> return_type) in
  let* final_sub = Subst.compose_all [ sub1; sub2; sub3 ] in
  return (final_sub, Subst.apply sub3 return_type, Expr_app (expr1, expr2))
;;

let infer_toplevel env = function
  | Ast.Let_decl (false, Ast.LCIdent name, expr) ->
    let* sub, ty, expr = infer_expr env expr in
    let env = TypeEnv.apply sub env in
    let gen_scheme = generalize env ty in
    let env = TypeEnv.update env name gen_scheme in
    return (env, ty, Ast.Let_decl (false, Ast.LCIdent name, expr))
  | Ast.Let_decl (true, Ast.LCIdent name, expr) ->
    let* ty = fresh_var in
    let env = TypeEnv.update env name (Scheme (TypeVarSet.empty, ty)) in
    let* sub1, typ1, expr = infer_expr env expr in
    let* sub2 = Subst.unify typ1 ty in
    let* sub3 = Subst.compose sub1 sub2 in
    let env = TypeEnv.apply sub3 env in
    let typ1 = Subst.apply sub3 typ1 in
    let gen_scheme = generalize_rec env typ1 name in
    let env = TypeEnv.update env name gen_scheme in
    return (env, Subst.apply sub2 typ1, Ast.Let_decl (true, Ast.LCIdent name, expr))
  | Ast.Expr expr ->
    let* _, ty, expr = infer_expr env expr in
    return (env, ty, Ast.Expr expr)
;;

let infer_program prog =
  let rec helper env = function
    | [] -> return (env, [])
    | h :: tl ->
      let* new_env, _, new_ast = infer_toplevel env h in
      let* env, ast_list = helper new_env tl in
      return (env, new_ast :: ast_list)
  in
  helper TypeEnv.std prog
;;

let run_infer_expr e =
  Result.map (fun (_, ty, ast) -> ty, ast) (run (infer_expr TypeEnv.std e))
;;

let run_infer_program p = run (infer_program p)
