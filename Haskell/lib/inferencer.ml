(** Copyright 2023-2024, Danil P, Kakadu *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Typedtree

let use_logging = false

let log fmt =
  if use_logging
  then Format.kasprintf (fun s -> Format.printf "%s\n%!" s) fmt
  else Format.ifprintf Format.std_formatter fmt
;;

type error =
  [ `Occurs_check
  | `No_variable of string
  | `Unification_failed of ty * ty
  | `Type_error of string
  ]
  [@@deriving show { with_path = false }]

module R : sig
  type 'a t

  val bind : 'a t -> f:('a -> 'b t) -> 'b t
  val return : 'a -> 'a t
  val fail : error -> 'a t

  include Base.Monad.Infix with type 'a t := 'a t

  module Syntax : sig
    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  end

  module RMap : sig
    val fold_left :
    ('a, 'b, 'c) Base.Map.t ->
    init:'d t -> f:('a -> 'b -> 'd -> 'd t) -> 'd t
  end

  (** Creation of a fresh name from internal state *)
  val fresh : int t

  (** Running a transformer: getting the inner result value *)
  val run : 'a t -> ('a, error) Result.t
end = struct
  (* A compositon: State monad after Result monad *)
  type 'a t = int -> int * ('a, error) Result.t

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

  module RMap = struct
    let fold_left map ~init ~f =
      Base.Map.fold map ~init ~f:(fun ~key ~data acc ->
        let open Syntax in
        let* acc = acc in
        f key data acc)
    ;;
  end

  let fresh : int t = fun last -> last + 1, Result.Ok last
  let run m = snd (m 0)
end

type fresh = int

module Type = struct
  type t = ty

  let rec occurs_in v = function
    | TyVar b -> b = v
    | TyArrow (l, r) -> occurs_in v l || occurs_in v r
    | TyLit _ -> false
    | TyList ty -> occurs_in v ty
    | TyTuple types -> List.exists (fun ty -> occurs_in v ty) types
  ;;

  let free_vars =
    let rec helper acc = function
      | TyVar b -> VarSet.add b acc
      | TyArrow (l, r) -> helper (helper acc l) r
      | TyLit _ -> acc
      | TyList ty -> helper acc ty
      | TyTuple types -> List.fold_left (fun acc ty -> helper acc ty) acc types
    in
    helper VarSet.empty
  ;;
end

module Subst : sig
  type t

  val empty : t
  val singleton : fresh -> ty -> t R.t

  (** Getting value from substitution. May raise [Not_found] *)
  val find_exn : t -> fresh -> ty

  val find : t -> fresh -> ty option
  val apply : t -> ty -> ty
  val unify : ty -> ty -> t R.t

  (** Compositon of substitutions *)
  val compose : t -> t -> t R.t

  val compose_all : t list -> t R.t
  val remove : t -> fresh -> t
end = struct
  open R
  open R.Syntax
  open Base

  type t = (fresh, ty, Int.comparator_witness) Map.t

  let empty = Map.empty (module Base.Int)

  let singleton k v =
    if Type.occurs_in k v then fail `Occurs_check else return @@ Map.singleton (module Base.Int) k v
  ;;

  let find_exn = Map.find_exn
  let find = Map.find
  let remove = Map.remove

  let apply s =
    let rec helper = function
      | TyVar b as ty ->
        (match find s b with
         | None -> ty
         | Some x -> x)
      | TyArrow (l, r) -> TyArrow (helper l, helper r)
      | TyList ty -> TyList (helper ty)
      | other -> other
    in
    helper
  ;;

  let rec unify l r =
    match l, r with
    | TyLit l, TyLit r when String.equal l r -> return empty
    | TyLit _, TyLit _ -> fail (`Unification_failed (l, r))
    | TyVar a, TyVar b when Int.equal a b -> return empty
    | TyVar b, t | t, TyVar b -> singleton b t
    | TyArrow (l1, r1), TyArrow (l2, r2) ->
      let* subs1 = unify l1 l2 in
      let* subs2 = unify (apply subs1 r1) (apply subs1 r2) in
      compose subs1 subs2
    | TyList t1, TyList t2 -> unify t1 t2
    | _ -> fail (`Unification_failed (l, r))

  and extend k v s =
    match find s k with
    | None ->
      let v = apply s v in
      let* s2 = singleton k v in
      RMap.fold_left s ~init:(return s2) ~f:(fun k v acc ->
        let v = apply s2 v in
        return (Map.update acc k ~f:(fun _ -> v)))
    | Some v2 ->
      let* s2 = unify v v2 in
      compose s s2

  and compose s1 s2 = RMap.fold_left s2 ~init:(return s1) ~f:extend

  let compose_all ss =
    List.fold_left ss ~init:(return empty) ~f:(fun acc s ->
      let* acc = acc in
      compose acc s)
    ;;
end


module VarSet = struct
  include VarSet

  let fold_left_m f acc set =
    fold
      (fun x acc ->
        let open R.Syntax in
        let* acc = acc in
        f acc x)
      acc
      set
  ;;
end

module Scheme = struct
  type t = scheme

  let occurs_in v = function
    | S (xs, t) -> (not (VarSet.mem v xs)) && Type.occurs_in v t
  ;;

  let free_vars = function
    | S (bs, t) -> VarSet.diff (Type.free_vars t) bs
  ;;

  let apply sub (S (names, ty)) =
    let s2 = VarSet.fold (fun k s -> Subst.remove s k) names sub in
    S (names, Subst.apply s2 ty)
  ;;

end

module TypeEnv = struct
  open Base
  open Ast

  type t = (id, scheme, String.comparator_witness) Map.t

  let extend env (id, scheme) = Map.set env ~key:id ~data:scheme
  let empty = Map.empty (module String)

  let free_vars  =
    Map.fold ~init:VarSet.empty ~f:(fun ~key:_ ~data:s acc ->
      VarSet.union acc (Scheme.free_vars s))
  ;;

  let apply s env = Map.map env ~f:(Scheme.apply s)
  let find_exn name map = Map.find_exn map name
end

open R
open R.Syntax

let unify = Subst.unify
let fresh_var = fresh >>| fun n -> TyVar n

let instantiate : scheme -> ty R.t =
  fun (S (bs, t)) ->
  VarSet.fold_left_m
    (fun typ name ->
      let* f1 = fresh_var in
      let* s = Subst.singleton name f1 in
      return (Subst.apply s typ))
    bs
    (return t)
;;

let generalize : TypeEnv.t -> Type.t -> Scheme.t =
  fun env ty ->
  let free = VarSet.diff (Type.free_vars ty) (TypeEnv.free_vars env) in
  S (free, ty)
;;

let lookup_env id map =
  match Base.Map.find map id with
  | None -> fail (`No_variable id)
  | Some scheme ->
    let* ans = instantiate scheme in
    return (Subst.empty, ans)
;;


let infer_pattern =
  let open Ast in
  let rec (helper : TypeEnv.t -> pat -> (TypeEnv.t * ty) R.t) =
    fun env -> function
    | PatWild ->
      let* tv = fresh_var in
      return (env, tv)
    | PatNil ->
      let* tv = fresh_var in
      return (env, TyList tv)
    | PatLit l -> 
      (match l with
      | LitInt _ -> return (env, TyLit "int")
      | LitString _ -> return (env, TyLit "string")
      (* TODO: more types*))
    | PatVar x ->
      (match Base.Map.find env x with
        | None ->
          let* tv = fresh_var in
          let env = TypeEnv.extend env (x, S (VarSet.empty, tv)) in
          return (env, tv)
        | Some _ -> fail `Occurs_check)
        (* TODO: change error type *)
  in 
  helper
;;
let infer =
  let open Ast in
  let rec (helper : TypeEnv.t -> expr -> (Subst.t * ty) R.t) =
    fun env -> function
    (* TODO: make it more concise*)
    | ExprBinOp (op, e1, e2) -> 
      let* s1, t1 = helper env e1 in
      let* s2, t2 = helper env e2 in
      (match op with
      | Add | Sub | Mul | Div ->
        let* s3 = unify t1 (TyLit "int") in
        let* s4 = unify t2 (TyLit "int") in
        let* final_subst = Subst.compose_all [ s1; s2; s3; s4 ] in
        return (final_subst, TyLit "int")
      | And | Or -> 
        let* s3 = unify t1 (TyLit "bool") in
        let* s4 = unify t2 (TyLit "bool") in
        let* final_subst = Subst.compose_all [ s1; s2; s3; s4 ] in
        return (final_subst, TyLit "bool")
      | Eq | Neq | Lt | Gt | Leq | Geq -> 
        let* s3 = unify t1 (TyLit "int") in
        let* s4 = unify t2 (TyLit "int") in
        let* final_subst = Subst.compose_all [ s1; s2; s3; s4 ] in
        return (final_subst, TyLit "bool"))
    | ExprVar x -> lookup_env x env
    | ExprFunc (x, e1) ->
      let* env2, tv = infer_pattern env x in
      let* s, ty = helper env2 e1 in
      let trez = TyArrow (Subst.apply s tv, ty) in
      return (s, trez)
    | ExprApp (e1, e2) ->
      let* s1, t1 = helper env e1 in
      let* s2, t2 = helper (TypeEnv.apply s1 env) e2 in
      let* tv = fresh_var in
      let* s3 = unify (Subst.apply s2 t1) (TyArrow (t2, tv)) in
      let trez = Subst.apply s3 tv in
      let* final_subst = Subst.compose_all [ s3; s2; s1 ] in
      return (final_subst, trez)
    | ExprLit (LitInt _) -> return (Subst.empty, TyLit "int")
    | ExprLit (LitString _) -> return (Subst.empty, TyLit "string")
    | ExprIf (c, th, el) ->
      let* s1, t1 = helper env c in
      let* s2, t2 = helper env th in
      let* s3, t3 = helper env el in
      let* s4 = unify t1 (TyLit "bool") in
      let* s5 = unify t2 t3 in
      let* final_subst = Subst.compose_all [ s5; s4; s3; s2; s1 ] in
      R.return (final_subst, Subst.apply s5 t2)
    | ExprTuple es ->
      let rec infer_elements env es subst_acc types_acc = 
        match es with
        | [] -> return (subst_acc, types_acc)
        | e :: tl ->
          let* subst, ty = helper env e in
          let env' = TypeEnv.apply subst env in
          let* subst_acc' = Subst.compose subst subst_acc in
          infer_elements env' tl subst_acc' (ty :: types_acc)
      in
      let* final_subst, types = infer_elements env es Subst.empty [] in
      return (final_subst, TyTuple (List.rev types))
    | ExprNil ->
      let* tv = fresh_var in
      return (Subst.empty, TyList tv)
    | ExprCons (hd, tl) ->
      let* s1, t1 = helper env hd in
      let* s2, t2 = helper (TypeEnv.apply s1 env) tl in
      let list_ty = TyList t1 in
      let* s3 = unify list_ty t2 in
      let final_ty = Subst.apply s3 list_ty in
      let* final_subst = Subst.compose_all [s3; s2; s1] in
      return (final_subst, final_ty)
    | ExprLet (bindings, main_expr) ->
      let rec process_bindings env bindings_acc subst_acc = function
        | [] -> return (env, subst_acc)
        | (x, _) :: tl ->
          let* (env2, _) = infer_pattern env x in
          process_bindings env2 bindings_acc subst_acc tl
      in
      let* env', subst_acc = process_bindings env [] Subst.empty bindings in
      let* subst, ty = helper env' main_expr in
      let* final_subst = Subst.compose subst subst_acc in
      return (final_subst, ty)
  in
  helper
;;




let w e = Result.map snd (run (infer TypeEnv.empty e))

let pp_infer e =
  match w e with
  | Ok ty -> Format.printf "%a" pp_ty ty
  | Error err -> Format.printf "%a" pp_error err
;;

let pp_parse_and_infer input =
  match Angstrom.parse_string ~consume:All Parser.pexpr input with
  | Result.Ok e -> pp_infer e
  | _ -> print_endline "Failed to parse"
;;

let%expect_test _ =
  pp_parse_and_infer {|\x -> [x]|};
  [%expect {| (TyArrow ((TyVar 1), (TyList (TyVar 1)))) |}]
;;


let%expect_test _ =
  pp_parse_and_infer {|\x -> [x, 5]|};
  [%expect {| (TyArrow ((TyLit "int"), (TyList (TyLit "int")))) |}]
;;

let%expect_test _ =
  pp_parse_and_infer {|\x y -> (x, y)|};
  [%expect {|
    (TyArrow ((TyVar 0), (TyArrow ((TyVar 1), (TyTuple [(TyVar 0); (TyVar 1)])))
       )) |}]
;;


let infer_decl env (Ast.DeclLet (x,  expr)) = 
  let* (env2, _) = infer_pattern env x in

  (* TODO: переделать; какая-то грязь тут *)
  let* subst, inferred_type = infer env2 expr in
  let scheme = generalize env2 inferred_type in
  return (subst, scheme)


let w e = Result.map snd (run (infer_decl TypeEnv.empty e))

let pp_infer e =
  match w e with
  | Ok ty -> Format.printf "%a" pp_scheme ty
  | Error err -> Format.printf "%a" pp_error err
;;

let pp_parse_and_infer input =
  match Angstrom.parse_string ~consume:All Parser.pdecl input with
  | Result.Ok e -> pp_infer e
  | _ -> print_endline "Failed to parse"
;;

let%expect_test _ =
  pp_parse_and_infer {| fact n = if (n < 2) then 1 else fact (n - 1) * n|};
  [%expect {| (S ([ ], (TyArrow ((TyLit "int"), (TyLit "int"))))) |}]
;;
let%expect_test _ =
  pp_parse_and_infer {| dam x y = (x, y)|};
  [%expect {|
    (S ([ 1; 2; ],
       (TyArrow ((TyVar 1),
          (TyArrow ((TyVar 2), (TyTuple [(TyVar 1); (TyVar 2)])))))
       )) |}]
;;

let%expect_test _ =
  pp_parse_and_infer {|   f a = 
  let x = 3
      y = 5
      z = 7
  in x + y + z + a|};
  [%expect {|
    (S ([ ], (TyArrow ((TyLit "int"), (TyLit "int"))))) |}]
;;