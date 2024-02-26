(** Copyright 2023-2024, Nikita Nemakin *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(* Based on
   https://gitlab.com/Kakadu/fp2020course-materials/-/blob/master/code/miniml/inferencer.ml?ref_type=heads
   with new features added and bugs fixed *)

open Ast
open Typedtree
open Base

type error =
  [ `Occurs_check
  | `No_variable of id
  | `Unification_failed of ty * ty
  | `Pattern_matching_error
  | `Not_implemented (** polymorphic variants are not supported *)
  ]

let pp_error ppf : error -> _ =
  let open Stdlib.Format in
  function
  | `Occurs_check -> fprintf ppf "Occurs check failed"
  | `No_variable s -> fprintf ppf "Unbound variable '%s'" s
  | `Unification_failed (l, r) ->
    fprintf ppf "Unification failed on %a and %a" pp_typ l pp_typ r
  | `Pattern_matching_error -> fprintf ppf "Pattern matching error"
  | `Not_implemented -> fprintf ppf "Not implemented"
;;

type id = string

module VarSet = struct
  include Stdlib.Set.Make (Int)
end

type scheme = S of VarSet.t * ty (** \forall a1 a2 ... an . ty *)

module R : sig
  type 'a t

  val bind : 'a t -> f:('a -> 'b t) -> 'b t
  val return : 'a -> 'a t
  val fail : error -> 'a t

  include Monad.Infix with type 'a t := 'a t

  module Syntax : sig
    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  end

  (** Creation of a fresh name from internal state *)
  val fresh : int t

  (** Running a transformer: getting the inner result value *)
  val run : 'a t -> ('a, error) Result.t

  module RMap : sig
    val fold : ('a, 'b, 'c) Map.t -> init:'d t -> f:('a -> 'b -> 'd -> 'd t) -> 'd t
  end
end = struct
  (** A composition: State monad after Result monad *)
  type 'a t = int -> int * ('a, error) Result.t

  let ( >>= ) : 'a 'b. 'a t -> ('a -> 'b t) -> 'b t =
    fun m f st ->
    let last, r = m st in
    match r with
    | Result.Error x -> last, Result.fail x
    | Result.Ok a -> f a last
  ;;

  let fail e st = st, Result.fail e
  let return x last = last, Result.return x
  let bind x ~f = x >>= f

  let ( >>| ) : 'a 'b. 'a t -> ('a -> 'b) -> 'b t =
    fun m f st ->
    match m st with
    | st, Ok x -> st, Result.return (f x)
    | st, Result.Error e -> st, Result.fail e
  ;;

  module Syntax = struct
    let ( let* ) x f = bind x ~f
  end

  let fresh : int t = fun last -> last + 1, Result.return last
  let run m = snd (m 0)

  module RMap = struct
    let fold m ~init ~f =
      Map.fold m ~init ~f:(fun ~key ~data acc ->
        let open Syntax in
        let* acc = acc in
        f key data acc)
    ;;
  end
end

type fresh = int

module Type = struct
  type t = ty

  let rec occurs_in v = function
    | TVar b -> b = v
    | TArrow (l, r) -> occurs_in v l || occurs_in v r
    | TTuple tl -> Base.List.exists tl ~f:(occurs_in v)
    | TList t -> occurs_in v t
    | TPrim _ -> false
  ;;

  let free_vars =
    let rec helper acc = function
      | TVar b -> VarSet.add b acc
      | TArrow (l, r) -> helper (helper acc l) r
      | TTuple tl -> List.fold_left tl ~init:acc ~f:helper
      | TList t -> helper acc t
      | TPrim _ -> acc
    in
    helper VarSet.empty
  ;;
end

module Subst : sig
  type t

  val empty : t
  val singleton : fresh -> ty -> t R.t
  val find : t -> fresh -> ty option
  val remove : t -> fresh -> t
  val apply : t -> ty -> ty
  val unify : ty -> ty -> t R.t
  val compose : t -> t -> t R.t
  val compose_all : t list -> t R.t
end = struct
  open R
  open R.Syntax

  type t = (fresh, ty, Int.comparator_witness) Map.t

  let empty = Map.empty (module Int)
  let mapping k v = if Type.occurs_in k v then fail `Occurs_check else return (k, v)

  let singleton k v =
    let* k, v = mapping k v in
    return (Map.singleton (module Int) k v)
  ;;

  let find s k = Map.find s k
  let remove s k = Map.remove s k

  let apply s =
    let rec helper = function
      | TVar b as ty ->
        (match find s b with
         | None -> ty
         | Some x -> x)
      | TArrow (l, r) -> arrow (helper l) (helper r)
      | TList t -> list_typ (helper t)
      | TTuple ts -> tuple_typ (List.map ~f:helper ts)
      | other -> other
    in
    helper
  ;;

  let rec unify l r =
    match l, r with
    | TPrim l, TPrim r when String.equal l r -> return empty
    | TVar a, TVar b when Int.equal a b -> return empty
    | TVar b, t | t, TVar b -> singleton b t
    | TArrow (l1, r1), TArrow (l2, r2) ->
      let* s1 = unify l1 l2 in
      let* s2 = unify (apply s1 r1) (apply s1 r2) in
      compose s1 s2
    | TList t1, TList t2 -> unify t1 t2
    | TTuple ts1, TTuple ts2 ->
      (match
         List.fold2
           ts1
           ts2
           ~f:(fun acc t1 t2 ->
             let* acc = acc in
             let* s = unify (apply acc t1) (apply acc t2) in
             compose acc s)
           ~init:(return empty)
       with
       | Unequal_lengths -> fail (`Unification_failed (l, r))
       | Ok s -> s)
    | _ -> fail (`Unification_failed (l, r))

  and extend k v s =
    match find s k with
    | None ->
      let v = apply s v in
      let* s2 = singleton k v in
      RMap.fold s ~init:(return s2) ~f:(fun k v acc ->
        let v = apply s2 v in
        let* k, v = mapping k v in
        return (Map.update acc k ~f:(fun _ -> v)))
    | Some v2 ->
      let* s2 = unify v v2 in
      compose s s2

  and compose s1 s2 = RMap.fold s2 ~init:(return s1) ~f:extend

  let compose_all ss =
    List.fold_left ss ~init:(return empty) ~f:(fun acc s ->
      let* acc = acc in
      compose acc s)
  ;;
end

module Scheme = struct
  type t = scheme

  let occurs_in v (S (xs, t)) = (not (VarSet.mem v xs)) && Type.occurs_in v t
  let free_vars (S (xs, t)) = VarSet.diff (Type.free_vars t) xs

  let apply s (S (xs, t)) =
    let s2 = VarSet.fold (fun k s -> Subst.remove s k) xs s in
    S (xs, Subst.apply s2 t)
  ;;
end

module TypeEnv = struct
  type t = (id, scheme, String.comparator_witness) Map.t

  let extend e k v = Map.update e k ~f:(fun _ -> v)
  let remove e k = Map.remove e k
  let empty = Map.empty (module String)

  let free_vars : t -> VarSet.t =
    Map.fold ~init:VarSet.empty ~f:(fun ~key:_ ~data:s acc ->
      VarSet.union acc (Scheme.free_vars s))
  ;;

  let apply s env = Map.map env ~f:(Scheme.apply s)
  let find x env = Map.find env x
end

open R
open R.Syntax

let fresh_var = fresh >>| fun n -> TVar n

let instantiate : scheme -> ty R.t =
  fun (S (xs, ty)) ->
  VarSet.fold
    (fun name typ ->
      let* typ = typ in
      let* f1 = fresh_var in
      let* s = Subst.singleton name f1 in
      return (Subst.apply s typ))
    xs
    (return ty)
;;

let generalize env ty =
  let free = VarSet.diff (Type.free_vars ty) (TypeEnv.free_vars env) in
  S (free, ty)
;;

let generalize_rec env ty x =
  let env = TypeEnv.remove env x in
  generalize env ty
;;

let infer_pat =
  let rec helper env = function
    | PAny ->
      let* fresh = fresh_var in
      return (env, fresh)
    | PConst c ->
      (match c with
       | CInt _ -> return (env, int_typ)
       | CBool _ -> return (env, bool_typ)
       | CString _ -> return (env, string_typ)
       | CUnit -> return (env, unit_typ)
       | CNil ->
         let* fresh = fresh_var in
         return (env, list_typ fresh))
    | PVar x ->
      let* fresh = fresh_var in
      let env = TypeEnv.extend env x (S (VarSet.empty, fresh)) in
      return (env, fresh)
    | PCons (p1, p2) ->
      let* env1, t1 = helper env p1 in
      let* env2, t2 = helper env1 p2 in
      let* sub = Subst.unify (list_typ t1) t2 in
      let env = TypeEnv.apply sub env2 in
      return (env, Subst.apply sub t2)
    | PTuple pl ->
      let* env, tl =
        List.fold_left
          ~f:(fun acc pat ->
            let* env1, tl = acc in
            let* env2, t = helper env1 pat in
            return (env2, t :: tl))
          ~init:(return (env, []))
          pl
      in
      return (env, tuple_typ (List.rev tl))
    | _ -> fail `Pattern_matching_error
  in
  helper
;;

let infer_exp =
  let rec helper env = function
    | EConst c ->
      (match c with
       | CInt _ -> return (Subst.empty, int_typ)
       | CBool _ -> return (Subst.empty, bool_typ)
       | CString _ -> return (Subst.empty, string_typ)
       | CUnit -> return (Subst.empty, unit_typ)
       | CNil ->
         let* fresh = fresh_var in
         return (Subst.empty, list_typ fresh))
    | EVar x ->
      (match TypeEnv.find x env with
       | Some s ->
         let* t = instantiate s in
         return (Subst.empty, t)
       | None -> fail (`No_variable x))
    | EBin_op (op, e1, e2) ->
      let* sub1, t1 = helper env e1 in
      let* sub2, t2 = helper (TypeEnv.apply sub1 env) e2 in
      let* e1t, e2t, et =
        match op with
        | Mul | Div | Add | Sub -> return (int_typ, int_typ, int_typ)
        | Eq | Neq | Lt | Lte | Gt | Gte ->
          let* fresh = fresh_var in
          return (fresh, fresh, bool_typ)
        | And | Or -> return (bool_typ, bool_typ, bool_typ)
      in
      let* sub3 = Subst.unify (Subst.apply sub2 t1) e1t in
      let* sub4 = Subst.unify (Subst.apply sub3 t2) e2t in
      let* sub = Subst.compose_all [ sub1; sub2; sub3; sub4 ] in
      return (sub, Subst.apply sub et)
    | EIf (i, t, e) ->
      let* sub1, t1 = helper env i in
      let* sub2, t2 = helper (TypeEnv.apply sub1 env) t in
      let* sub3, t3 = helper (TypeEnv.apply sub2 env) e in
      let* sub4 = Subst.unify t1 bool_typ in
      let* sub5 = Subst.unify t2 t3 in
      let* sub = Subst.compose_all [ sub1; sub2; sub3; sub4; sub5 ] in
      return (sub, Subst.apply sub t2)
    | EMatch (e, cl) ->
      let* sub1, t1 = helper env e in
      let env = TypeEnv.apply sub1 env in
      let* fresh = fresh_var in
      let* sub, t =
        List.fold_left
          ~f:(fun acc (pat, exp) ->
            let* sub1, t = acc in
            let* env1, pt = infer_pat env pat in
            let* sub2 = Subst.unify t1 pt in
            let env2 = TypeEnv.apply sub2 env1 in
            let* sub3, t' = helper env2 exp in
            let* sub4 = Subst.unify t' t in
            let* sub = Subst.compose_all [ sub1; sub2; sub3; sub4 ] in
            return (sub, Subst.apply sub t))
          ~init:(return (sub1, fresh))
          cl
      in
      return (sub, t)
    | ELet (Nonrec, (PVar x, e1), e2) ->
      let* s1, t1 = helper env e1 in
      let env = TypeEnv.apply s1 env in
      let s = generalize env t1 in
      let* s2, t2 = helper (TypeEnv.extend env x s) e2 in
      let* s = Subst.compose s1 s2 in
      return (s, t2)
    | ELet (Rec, (PVar x, e1), e2) ->
      let* fresh = fresh_var in
      let env1 = TypeEnv.extend env x (S (VarSet.empty, fresh)) in
      let* s, t = helper env1 e1 in
      let* s1 = Subst.unify t fresh in
      let* s2 = Subst.compose s s1 in
      let env = TypeEnv.apply s2 env in
      let t = Subst.apply s2 t in
      let s = generalize_rec env t x in
      let env = TypeEnv.extend env x s in
      let* sub, t = helper env e2 in
      let* sub = Subst.compose s2 sub in
      return (sub, t)
    | EFun (p, e) ->
      let* env, t = infer_pat env p in
      let* sub, t1 = helper env e in
      return (sub, Subst.apply sub (arrow t t1))
    | ETuple el ->
      let* sub, t =
        List.fold_left
          ~f:(fun acc e ->
            let* sub, t = acc in
            let* sub1, t1 = helper env e in
            let* sub2 = Subst.compose sub sub1 in
            return (sub2, t1 :: t))
          ~init:(return (Subst.empty, []))
          el
      in
      return (sub, tuple_typ (List.rev_map ~f:(Subst.apply sub) t))
    | ECons (e1, e2) ->
      let* s1, t1 = helper env e1 in
      let* s2, t2 = helper env e2 in
      let* sub = Subst.unify (list_typ t1) t2 in
      let t = Subst.apply sub t2 in
      let* sub = Subst.compose_all [ s1; s2; sub ] in
      return (sub, t)
    | EApply (e1, e2) ->
      let* fresh = fresh_var in
      let* s1, t1 = helper env e1 in
      let* s2, t2 = helper (TypeEnv.apply s1 env) e2 in
      let* s3 = Subst.unify (arrow t2 fresh) (Subst.apply s2 t1) in
      let* sub = Subst.compose_all [ s1; s2; s3 ] in
      let t = Subst.apply sub fresh in
      return (sub, t)
    | _ -> fail `Not_implemented
  in
  helper
;;

let infer_str_item env = function
  | SValue (Rec, (PVar x, e)) ->
    let* fresh = fresh_var in
    let sc = S (VarSet.empty, fresh) in
    let env = TypeEnv.extend env x sc in
    let* s1, t1 = infer_exp env e in
    let* s2 = Subst.unify t1 fresh in
    let* s3 = Subst.compose s1 s2 in
    let env = TypeEnv.apply s3 env in
    let t2 = Subst.apply s3 t1 in
    let sc = generalize_rec env t2 x in
    let env = TypeEnv.extend env x sc in
    return env
  | SValue (Nonrec, (PVar x, e)) ->
    let* s, t = infer_exp env e in
    let env = TypeEnv.apply s env in
    let sc = generalize env t in
    let env = TypeEnv.extend env x sc in
    return env
  | SEval e ->
    let* _, _ = infer_exp env e in
    return env
  | _ -> fail `Not_implemented
;;

let infer_structure (structure : structure) =
  List.fold_left
    ~f:(fun acc item ->
      let* env = acc in
      let* env = infer_str_item env item in
      return env)
    ~init:(return TypeEnv.empty)
    structure
;;

let run_infer s = run (infer_structure s)

let test_infer s =
  let open Stdlib.Format in
  match Parser.parse s with
  | Ok parsed ->
    (match run_infer parsed with
     | Ok env ->
       Base.Map.iteri env ~f:(fun ~key ~data:(S (_, ty)) ->
         printf "val %s : %a\n" key pp_typ ty)
     | Error e -> printf "Infer error: %a\n" pp_error e)
  | Error e -> printf "Parsing error: %s\n" e
;;
