(** Copyright 2023-2024, tepa46 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(* The solution for inferring types of the mini-OCaml language with ADT *)

(* Template: https://gitlab.com/Kakadu/fp2020course-materials/-/blob/master/code/miniml/inferencer.ml?ref_type=heads *)

open InferencerTypes
open Format

module R : sig
  type 'a t

  val bind : 'a t -> f:('a -> 'b t) -> 'b t
  val return : 'a -> 'a t
  val fail : error -> 'a t

  include Base.Monad.Infix with type 'a t := 'a t

  module Syntax : sig
    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  end

  module RList : sig
    val fold_left : 'a list -> init:'b t -> f:('b -> 'a -> 'b t) -> 'b t
  end

  (** Creation of a fresh name from internal state *)
  val fresh : int t

  (** Running a transformer: getting the inner result value *)
  val run : 'a t -> ('a, error) Base.Result.t
end = struct
  open Base

  (* A compositon: State monad after Result monad *)
  type 'a t = int -> int * ('a, error) Base.Result.t

  let ( >>= ) : 'a 'b. 'a t -> ('a -> 'b t) -> 'b t =
    fun m f st ->
    let last, r = m st in
    match r with
    | Result.Error x -> last, Error x
    | Ok a -> f a last
  ;;

  let fail e st = st, Base.Result.fail e
  let return x last = last, Base.Result.return x
  let bind x ~f = x >>= f

  let ( >>| ) : 'a 'b. 'a t -> ('a -> 'b) -> 'b t =
    fun x f st ->
    match x st with
    | st, Ok x -> st, Ok (f x)
    | st, Result.Error e -> st, Result.Error e
  ;;

  module Syntax = struct
    let ( let* ) x f = bind x ~f
  end

  module RList = struct
    let fold_left xs ~init ~f =
      Base.List.fold_left xs ~init ~f:(fun acc x ->
        let open Syntax in
        let* acc = acc in
        f acc x)
    ;;
  end

  let fresh : int t = fun last -> last + 1, Result.Ok last
  let run m = snd (m 0)
end

type fresh = int

module Type = struct
  type t = ty

  let rec occurs_in v = function
    | TVar b -> b = v
    | TPrim _ -> false
    | TArr (l, r) -> occurs_in v l || occurs_in v r
    | TTuple t -> List.fold_left (fun acc h -> acc || occurs_in v h) false t
    | TList l -> occurs_in v l
    | TAdt _ -> false
  ;;

  let free_vars =
    let rec helper acc = function
      | TVar b -> VarSet.add b acc
      | TPrim _ -> acc
      | TArr (l, r) -> helper (helper acc l) r
      | TTuple t -> List.fold_left (fun acc h -> helper acc h) acc t
      | TList l -> helper acc l
      | TAdt _ -> acc
    in
    helper VarSet.empty
  ;;
end

module Subst : sig
  type t

  val empty : t
  val singleton : fresh -> ty -> t R.t
  val find : fresh -> t -> ty option
  val apply : t -> ty -> ty
  val unify : ty -> ty -> t R.t

  (** Compositon of substitutions *)
  val compose : t -> t -> t R.t

  val compose_all : t list -> t R.t
  val remove : fresh -> t -> t
end = struct
  open R
  open R.Syntax
  open Base

  type t = ty VarMap.t

  let empty = VarMap.empty
  let mapping k vm = if Type.occurs_in k vm then fail `Occurs_check else return (k, vm)

  let singleton k vm =
    let* k, vm = mapping k vm in
    return (VarMap.singleton k vm)
  ;;

  let find k vm = VarMap.find_opt k vm
  let remove k vm = VarMap.remove k vm

  let apply s =
    let rec helper = function
      | TVar b as ty ->
        (match find b s with
         | None -> ty
         | Some ty -> ty)
      | TPrim _ as ty -> ty
      | TArr (l, r) -> TArr (helper l, helper r)
      | TTuple t -> TTuple (List.map t ~f:(fun elm -> helper elm))
      | TList l -> TList (helper l)
      | TAdt _ as ty -> ty
    in
    helper
  ;;

  let fold mp init f =
    VarMap.fold
      (fun k vm acc ->
        let* acc = acc in
        f k vm acc)
      mp
      init
  ;;

  let rec unify l r =
    match l, r with
    | TVar a, TVar b when Int.equal a b -> return empty
    | TVar b, t | t, TVar b -> singleton b t
    | TPrim l, TPrim r when String.equal l r -> return empty
    | TArr (l1, r1), TArr (l2, r2) ->
      let* subst1 = unify l1 l2 in
      let* subst2 = unify (apply subst1 r1) (apply subst1 r2) in
      compose subst1 subst2
    | TTuple ty_lst1, TTuple ty_lst2 ->
      let* subst =
        match
          List.fold2 ty_lst1 ty_lst2 ~init:(return empty) ~f:(fun subst h1 h2 ->
            let* subst = subst in
            let* subst2 = unify (apply subst h1) (apply subst h2) in
            compose subst subst2)
        with
        | Ok res -> res
        | Unequal_lengths -> fail (`Unification_failed (l, r))
      in
      return subst
    | TList l, TList r -> unify l r
    | TAdt lname, TAdt rname when String.equal lname rname -> return empty
    | _ -> fail (`Unification_failed (l, r))

  and extend k v s =
    match VarMap.find_opt k s with
    | None ->
      let v = apply s v in
      let* s2 = singleton k v in
      fold s (return s2) (fun k v acc ->
        let v = apply s2 v in
        let* k, v = mapping k v in
        return (VarMap.add k v acc))
    | Some v2 ->
      let* s2 = unify v v2 in
      compose s s2

  and compose s1 s2 = fold s2 (return s1) extend

  let compose_all ss = RList.fold_left ss ~init:(return empty) ~f:compose
end

module VarSet = struct
  include VarSet

  let pp_varset fmt st = VarSet.iter (fun key -> fprintf fmt "'%d " key) st
end

module Scheme = struct
  type t = scheme

  let occurs_in v = function
    | Scheme (xs, t) -> (not (VarSet.mem v xs)) && Type.occurs_in v t
  ;;

  let free_vars = function
    | Scheme (bs, t) -> VarSet.diff (Type.free_vars t) bs
  ;;

  let apply subst (Scheme (names, ty)) =
    let s2 = VarSet.fold (fun k s -> Subst.remove k s) names subst in
    Scheme (names, Subst.apply s2 ty)
  ;;

  let pp_scheme fmt = function
    | Scheme (st, typ) ->
      if VarSet.is_empty st
      then fprintf fmt "%a" pp_ty typ
      else fprintf fmt "%a. %a" VarSet.pp_varset st pp_ty typ
  ;;
end

module TypeEnv = struct
  type t = scheme StringMap.t

  let fold f init mp = StringMap.fold (fun k v acc -> f k v acc) mp init
  let extend k v mp = StringMap.add k v mp
  let empty = StringMap.empty

  let free_vars : t -> VarSet.t =
    fold (fun _ s acc -> VarSet.union acc (Scheme.free_vars s)) VarSet.empty
  ;;

  let apply s env = StringMap.map (Scheme.apply s) env
  let find name xs = StringMap.find_opt name xs

  let pp_env fmt environment =
    StringMap.iter
      (fun key data -> fprintf fmt "%S: %a\n" key Scheme.pp_scheme data)
      environment
  ;;
end

module AdtEnv = struct
  type t = (string * string * ty option) StringMap.t (* lname * uname * ty *)

  let extend lname uname typ mp = StringMap.add uname (lname, uname, typ) mp
  let find uname mp = StringMap.find_opt uname mp
  let empty = StringMap.empty

  let pp_constr_elm fmt elm =
    let uname, typ = elm in
    match typ with
    | Some typ -> fprintf fmt "| %s of %a" uname pp_ty typ
    | None -> fprintf fmt "| %s" uname
  ;;

  let rec pp_constr_lst fmt = function
    | [] -> ()
    | [ h ] -> fprintf fmt "%a" pp_constr_elm h
    | h :: tl -> fprintf fmt "%a %a" pp_constr_elm h pp_constr_lst tl
  ;;

  let pp_env fmt adt_environment =
    let adt_types =
      StringMap.fold
        (fun uname (lname, _, typ) acc ->
          let lst = StringMap.find_opt lname acc in
          match lst with
          | Some lst -> StringMap.add lname ((uname, typ) :: lst) acc
          | None -> StringMap.add lname ((uname, typ) :: []) acc)
        adt_environment
        StringMap.empty
    in
    StringMap.iter
      (fun lname lst -> fprintf fmt "type %s = %a\n" lname pp_constr_lst lst)
      adt_types
  ;;
end

open R
open R.Syntax

let fresh_var = fresh >>| fun n -> TVar n

let instantiate : scheme -> ty R.t =
  fun (Scheme (bs, t)) ->
  VarSet.fold
    (fun name typ ->
      let* typ = typ in
      let* f1 = fresh_var in
      let* s = Subst.singleton name f1 in
      return (Subst.apply s typ))
    bs
    (return t)
;;

let generalize : TypeEnv.t -> Type.t -> Scheme.t =
  fun env ty ->
  let free = VarSet.diff (Type.free_vars ty) (TypeEnv.free_vars env) in
  Scheme (free, ty)
;;

let rec infer_pattern pattern adt_env env =
  match pattern with
  | Ast.PNill ->
    let* fresh_var = fresh_var in
    return (env, tlist fresh_var)
  | Ast.PWild ->
    let* fresh_var = fresh_var in
    return (env, fresh_var)
  | Ast.PString _ -> return (env, tprim_string)
  | Ast.PBool _ -> return (env, tprim_bool)
  | Ast.PInt _ -> return (env, tprim_int)
  | Ast.PVar (LName var_name) ->
    let* fresh_var = fresh_var in
    return (TypeEnv.extend var_name (Scheme (VarSet.empty, fresh_var)) env, fresh_var)
  | Ast.PCons (pattern1, pattern2) ->
    let* env, typ1 = infer_pattern pattern1 adt_env env in
    let* env, typ2 = infer_pattern pattern2 adt_env env in
    let* subst = Subst.unify (tlist typ1) typ2 in
    let env = TypeEnv.apply subst env in
    return (env, Subst.apply subst typ2)
  | Ast.PTuple pattern_lst ->
    let* env, typ_lst =
      List.fold_left
        (fun acc pattern ->
          let* env, typ_lst = acc in
          let* env, typ = infer_pattern pattern adt_env env in
          return (env, typ :: typ_lst))
        (return (env, []))
        pattern_lst
    in
    return (env, ttuple (List.rev typ_lst))
  | Ast.PAdt (UName constr_name, pattern1) ->
    let* type_name, _, args =
      match AdtEnv.find constr_name adt_env with
      | Some x -> return x
      | None -> fail (`Unbound_adt_type constr_name)
    in
    (match pattern1, args with
     | Some pattern1, Some args ->
       let* env, typ = infer_pattern pattern1 adt_env env in
       let* subst = Subst.unify typ args in
       let env = TypeEnv.apply subst env in
       return (env, tadt type_name)
     | None, None -> return (env, tadt type_name)
     | _ -> fail `Pattern_matching_failed)
  | _ -> fail `Pattern_matching_failed
;;

let rec infer_exp exp adt_env env =
  match exp with
  | Ast.EEmptyList ->
    let* ty = fresh_var in
    return (Subst.empty, tlist ty)
  | Ast.EInt _ -> return (Subst.empty, tprim_int)
  | Ast.EString _ -> return (Subst.empty, tprim_string)
  | Ast.EBool _ -> return (Subst.empty, tprim_bool)
  | Ast.EVar (Ast.LName decl_name) ->
    (match TypeEnv.find decl_name env with
     | None -> fail (`Unbound_variable decl_name)
     | Some x ->
       let* ty = instantiate x in
       return (Subst.empty, ty))
  | Ast.ETuple exp_lst ->
    let* subst, typ_lst =
      List.fold_left
        (fun acc exp ->
          let* subst, typ_lst = acc in
          let* subst1, typ1 = infer_exp exp adt_env env in
          let* subst = Subst.compose subst subst1 in
          return (subst, typ1 :: typ_lst))
        (return (Subst.empty, []))
        exp_lst
    in
    let typ_lst = List.rev_map (fun x -> Subst.apply subst x) typ_lst in
    return (subst, ttuple typ_lst)
  | Ast.EBinop (binop, exp1, exp2) ->
    let* subst1, typ1 = infer_exp exp1 adt_env env in
    let* subst2, typ2 = infer_exp exp2 adt_env (TypeEnv.apply subst1 env) in
    let* exp1_type, exp2_type, return_type =
      match binop with
      | Add | Sub | Mul | Div -> return (tprim_int, tprim_int, tprim_int)
      | Eq | Neq | Les | Leq | Gre | Geq ->
        let* fresh_var = fresh_var in
        return (fresh_var, fresh_var, tprim_bool)
      | And | Or -> return (tprim_bool, tprim_bool, tprim_bool)
      | Cons ->
        let* fresh_var = fresh_var in
        return (fresh_var, tlist fresh_var, tlist fresh_var)
    in
    let* subst3 = Subst.unify (Subst.apply subst2 typ1) exp1_type in
    let* subst4 = Subst.unify (Subst.apply subst3 typ2) exp2_type in
    let* subst = Subst.compose_all [ subst1; subst2; subst3; subst4 ] in
    return (subst, Subst.apply subst return_type)
  | Ast.EFun (pattern, exp) ->
    let* env, typ1 = infer_pattern pattern adt_env env in
    let* subst, typ2 = infer_exp exp adt_env env in
    return (subst, Subst.apply subst typ1 @-> typ2)
  | Ast.EConstr (UName constr_name, decl_exp) ->
    let* type_name, _, args =
      match AdtEnv.find constr_name adt_env with
      | Some x -> return x
      | None -> fail (`Unbound_adt_type constr_name)
    in
    let* subst =
      match args, decl_exp with
      | Some args, Some decl_exp ->
        let* subst, typ_exp = infer_exp decl_exp adt_env env in
        let* subst2 =
          match typ_exp, args with
          | (TVar _ as ty1), (_ as ty2)
          | (TPrim _ as ty1), (_ as ty2)
          | (TArr _ as ty1), (_ as ty2)
          | (TTuple _ as ty1), (_ as ty2)
          | (TList _ as ty1), (_ as ty2)
          | (TAdt _ as ty1), (_ as ty2) -> Subst.unify ty1 ty2
        in
        Subst.compose subst subst2
      | None, None -> return Subst.empty
      | _ -> fail `Wrong_type
    in
    return (subst, tadt type_name)
  | Ast.ELet ((DRec false, LName let_name, exp1), exp2) ->
    let* subst1, typ1 = infer_exp exp1 adt_env env in
    let envs = TypeEnv.apply subst1 env in
    let get_scheme = generalize envs typ1 in
    let* subst2, typ2 =
      infer_exp exp2 adt_env (TypeEnv.extend let_name get_scheme envs)
    in
    let* subst = Subst.compose subst1 subst2 in
    return (subst, typ2)
  | Ast.ELet ((DRec true, LName let_name, let_exp), exp) ->
    let* fresh_var = fresh_var in
    let new_env = TypeEnv.extend let_name (Scheme (VarSet.empty, fresh_var)) env in
    let* subst, ty = infer_exp let_exp adt_env new_env in
    let* subst2 = Subst.unify ty fresh_var in
    let* subst3 = Subst.compose subst subst2 in
    let ty = Subst.apply subst3 ty in
    let new_env = TypeEnv.apply subst3 new_env in
    (* generalizing on old env without let_name declaration *)
    let gen_scheme = generalize (TypeEnv.apply subst3 env) ty in
    let new_env = TypeEnv.extend let_name gen_scheme new_env in
    let* subb, typp = infer_exp exp adt_env new_env in
    return (subb, typp)
  | Ast.EApp (exp1, exp2) ->
    let* return_type = fresh_var in
    let* subst1, typ1 = infer_exp exp1 adt_env env in
    let* subst2, typ2 = infer_exp exp2 adt_env (TypeEnv.apply subst1 env) in
    let* subst3 = Subst.unify (Subst.apply subst2 typ1) (typ2 @-> return_type) in
    let* subst = Subst.compose_all [ subst1; subst2; subst3 ] in
    return (subst, Subst.apply subst return_type)
  | Ast.EMatch (exp1, lst) ->
    let* subst1, typ1 = infer_exp exp1 adt_env env in
    let env = TypeEnv.apply subst1 env in
    let* fresh_var = fresh_var in
    let* subst, typ =
      List.fold_left
        (fun acc (pattern, exp) ->
          let* subst, typ = acc in
          let* pat_env, pat_type = infer_pattern pattern adt_env env in
          let* subst2 = Subst.unify typ1 pat_type in
          let pat_env = TypeEnv.apply subst2 pat_env in
          let* exp_subst, exp_typ = infer_exp exp adt_env pat_env in
          let* return_subst = Subst.unify exp_typ typ in
          let* subst = Subst.compose_all [ subst; subst2; exp_subst; return_subst ] in
          return (subst, Subst.apply subst typ))
        (return (subst1, fresh_var))
        lst
    in
    return (subst, typ)
  | Ast.EIf (exp1, exp2, exp3) ->
    let* subst1, typ1 = infer_exp exp1 adt_env env in
    let* subst2, typ2 = infer_exp exp2 adt_env (TypeEnv.apply subst1 env) in
    let* subst3, typ3 = infer_exp exp3 adt_env (TypeEnv.apply subst2 env) in
    let* subst4 = Subst.unify typ1 tprim_bool in
    let* subst5 = Subst.unify typ2 typ3 in
    let* subst = Subst.compose_all [ subst1; subst2; subst3; subst4; subst5 ] in
    return (subst, Subst.apply subst typ2)
  | _ -> fail `Wrong_exp
;;

let infer_let (let_decl : Ast.let_decl) adt_env env =
  match let_decl with
  | DRec false, LName let_name, exp ->
    let* subst, ty = infer_exp exp adt_env env in
    let env = TypeEnv.apply subst env in
    let get_scheme = generalize env ty in
    let env = TypeEnv.extend let_name get_scheme env in
    return env
  | DRec true, LName let_name, exp ->
    let* fresh_var = fresh_var in
    let new_env = TypeEnv.extend let_name (Scheme (VarSet.empty, fresh_var)) env in
    let* subst, ty = infer_exp exp adt_env new_env in
    let* subst2 = Subst.unify ty fresh_var in
    let* subst3 = Subst.compose subst subst2 in
    let ty = Subst.apply subst3 ty in
    let new_env = TypeEnv.apply subst3 new_env in
    (* generalizing on old env without let_name declaration *)
    let gen_scheme = generalize (TypeEnv.apply subst3 env) ty in
    let new_env = TypeEnv.extend let_name gen_scheme new_env in
    return new_env
  | _ -> fail `Wrong_exp
;;

let rec convert_type (typ : Ast.decl_type) =
  match typ with
  | Ast.DType typ ->
    let* typ = convert_type typ in
    return typ
  | Ast.TEmptyType -> return None
  | Ast.TInt -> return (Some tprim_int)
  | Ast.TString -> return (Some tprim_string)
  | Ast.TBool -> return (Some tprim_bool)
  | Ast.TFun (l, r) ->
    let* left_type = convert_type l in
    let* right_type = convert_type r in
    (match left_type, right_type with
     | Some left_type, Some right_type -> return (Some (left_type @-> right_type))
     | _ -> fail `Wrong_type)
  | Ast.TVar (LName typ) -> return (Some (tadt typ))
  | Ast.TList t ->
    let* t = convert_type t in
    (match t with
     | Some t -> return (Some (tlist t))
     | _ -> fail `Wrong_type)
  | Ast.TTuple lst ->
    let* lst =
      List.fold_left
        (fun acc l ->
          let* lst = acc in
          let* typ = convert_type l in
          match typ with
          | Some typ -> return (typ :: lst)
          | _ -> fail `Wrong_type)
        (return [])
        lst
    in
    return (Some (ttuple (List.rev lst)))
  | _ -> fail `Wrong_type
;;

let infer_program (program : Ast.decl list) =
  List.fold_left
    (fun acc decl ->
      match decl with
      | Ast.DLet let_decl ->
        let* adt_env, env = acc in
        let* new_env = infer_let let_decl adt_env env in
        return (adt_env, new_env)
      | Ast.DType (LName type_name, type_lst) ->
        List.fold_left
          (fun acc (adt_name, adt_type) ->
            let* adt_name =
              match adt_name with
              | Ast.UName adt_name -> return adt_name
              | Ast.LName _ -> fail `Wrong_type
            in
            let* adt_env, env = acc in
            let* adt_type = convert_type adt_type in
            let adt_env = AdtEnv.extend type_name adt_name adt_type adt_env in
            return (adt_env, env))
          acc
          type_lst
      | _ -> fail `Wrong_exp)
    (return (AdtEnv.empty, TypeEnv.empty))
    program
;;

let run_infer_program (program : Ast.decl list) = run (infer_program program)
