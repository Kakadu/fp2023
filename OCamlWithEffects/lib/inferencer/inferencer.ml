(** Copyright 2021-2024, DmitryPilyuk and raf-nr *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Typedtree
open Auxiliary
open Inf_errors

module R : sig
  type 'a t

  val return : 'a -> 'a t
  val bind : 'a t -> f:('a -> 'b t) -> 'b t
  val fail : error -> 'a t

  include Base.Monad.Infix with type 'a t := 'a t

  module Syntax : sig
    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  end

  module RList : sig
    val fold_left : 'a list -> init:'b t -> f:('b -> 'a -> 'b t) -> 'b t
  end

  module RMap : sig
    val fold_left
      :  ('a, 'b, 'c) Base.Map.t
      -> init:'d t
      -> f:('a -> 'b -> 'd -> 'd t)
      -> 'd t
  end

  val fresh : int t
  val run : 'a t -> ('a, error) Result.t
end = struct
  type 'a t = int -> int * ('a, error) Result.t (* State and Result monad composition *)

  let ( >>= ) : 'a 'b. 'a t -> ('a -> 'b t) -> 'b t =
    fun m f s ->
    match m s with
    | s, Result.Error e -> s, Error e
    | s, Result.Ok v -> f v s
  ;;

  let ( >>| ) : 'a 'b. 'a t -> ('a -> 'b) -> 'b t =
    fun m f s ->
    match m s with
    | s, Result.Error e -> s, Error e
    | s, Result.Ok v -> s, Base.Result.return @@ f v
  ;;

  let return v last = last, Base.Result.return v
  let fail e state = state, Base.Result.fail e
  let bind x ~f = x >>= f
  let fresh last = last + 1, Result.Ok last (* Get new state *)

  module Syntax = struct
    let ( let* ) x f = bind x ~f (* Syntax sugar for bind *)
  end

  module RMap = struct
    (* Classic map folding. *)
    let fold_left mp ~init ~f =
      let open Syntax in
      Base.Map.fold mp ~init ~f:(fun ~key ~data acc ->
        let* acc = acc in
        f key data acc)
    ;;
  end

  module RList = struct
    (* Classic list folding. *)
    let fold_left lt ~init ~f =
      let open Syntax in
      Base.List.fold_left lt ~init ~f:(fun acc item ->
        let* acc = acc in
        f acc item)
    ;;
  end

  (* Run and get the internal value. *)
  let run m = snd (m 0)
end

module Type = struct
  type t = typ

  (* Checks whether the passed type variable is contained in the passed type. *)
  let rec occurs_in v = function
    | TVar b -> b = v
    | TArr (left, right) -> occurs_in v left || occurs_in v right
    | TList typ -> occurs_in v typ
    | TTuple typ_list ->
      List.fold_left (fun acc item -> acc || occurs_in v item) false typ_list
    | TEffect typ -> occurs_in v typ
    | TPrim _ -> false
    | _ -> false
  ;;

  (* Combines all type variables contained in a type into one set. *)
  let type_vars =
    let rec helper acc = function
      | TVar n -> TVarSet.add n acc
      | TArr (left, right) -> helper (helper acc left) right
      | TList typ -> helper acc typ
      | TTuple typ_list -> List.fold_left helper acc typ_list
      | TEffect typ -> helper acc typ
      | TPrim _ -> acc
      | _ -> acc
    in
    helper TVarSet.empty
  ;;
end

module Subst : sig
  type t

  val empty : t
  val singleton : int -> typ -> t R.t
  val find : t -> int -> typ option
  val remove : t -> int -> t
  val apply : t -> typ -> typ
  val unify : typ -> typ -> t R.t
  val compose : t -> t -> t R.t
  val compose_all : t list -> t R.t
end = struct
  open R
  open R.Syntax

  (* A substitution is an map, where the first element of each list element is what needs to be replaced,
     the second is what it should be replaced with. *)
  type t = (int, typ, Base.Int.comparator_witness) Base.Map.t

  let empty = Base.Map.empty (module Base.Int)

  (* Creates a pair if no error occurs. *)
  let mapping k v = if Type.occurs_in k v then fail occurs_check else return (k, v)

  let singleton k v =
    let* k, v = mapping k v in
    return (Base.Map.singleton (module Base.Int) k v)
  ;;

  let find sub k = Base.Map.find sub k
  let remove sub k = Base.Map.remove sub k

  (* Replace all type variables in a type with values ​​from the substitution. *)
  let apply sub =
    let rec helper = function
      | TVar n ->
        (match find sub n with
         | None -> tvar n
         | Some v -> v)
      | TArr (left, right) -> tarrow (helper left) (helper right)
      | TList typ -> tlist (helper typ)
      | TTuple t_list -> ttuple (Base.List.map t_list ~f:helper)
      | other -> other
    in
    helper
  ;;

  (* Try to unify two types into a single type. *)
  let rec unify l r =
    match l, r with
    | TPrim l, TPrim r when l = r -> return empty
    | TVar a, TVar b when a = b -> return empty
    | TVar a, t | t, TVar a -> singleton a t
    | TArr (left1, right1), TArr (left2, right2) ->
      let* sub1 = unify left1 left2 in
      let* sub2 = unify (apply sub1 right1) (apply sub1 right2) in
      compose sub1 sub2
    | TList typ1, TList typ2 -> unify typ1 typ2
    | TTuple t_list1, TTuple t_list2 ->
      (match
         Base.List.fold2 t_list1 t_list2 ~init:(return empty) ~f:(fun acc it1 it2 ->
           let* sub1 = acc in
           let* sub2 = unify (apply sub1 it1) (apply sub1 it2) in
           compose sub1 sub2)
       with
       | Ok r -> r
       | _ -> fail (unification_failed l r))
    | TEffect typ1, TEffect typ2 -> unify typ1 typ2
    | TContinuation _, TContinuation _ -> return empty
    | _ -> fail (unification_failed l r)

  (* Expanding the substitution with a new key-value. *)
  and extend k v sub =
    match find sub k with
    | None ->
      let v = apply sub v in
      let* new_sub = singleton k v in
      let f1 ~key ~data acc =
        let* acc = acc in
        let new_data = apply new_sub data in
        return (Base.Map.update acc key ~f:(fun _ -> new_data))
      in
      Base.Map.fold sub ~init:(return new_sub) ~f:f1
    | Some vl ->
      let* new_sub = unify v vl in
      compose sub new_sub

  (* Two substitution's composition. *)
  and compose sub1 sub2 = RMap.fold_left sub2 ~init:(return sub1) ~f:extend

  (* Composition of an arbitrary number of substitutions. *)
  let compose_all sub_list = RList.fold_left sub_list ~init:(return empty) ~f:compose
end

module Scheme = struct
  (* Free vars - all unquantified type variables in a type. *)
  let free_vars = function
    | Scheme (bind_vars, ty) -> TVarSet.diff (Type.type_vars ty) bind_vars
  ;;

  (* Apply substitution to all unquantified type variables in a type. *)
  let apply sub = function
    | Scheme (bind_vars, ty) ->
      let sub2 = TVarSet.fold (fun sub key -> Subst.remove key sub) bind_vars sub in
      Scheme (bind_vars, Subst.apply sub2 ty)
  ;;
end

module TypeEnv = struct
  (* A type enviroment is a map, the key of each element of which is a string,
     which is the name of the let-binding or effect-declration,
     and the key is the schema of the type of expression to which the name is bound. *)
  type t = (string, scheme, Base.String.comparator_witness) Base.Map.t

  let empty : t = Base.Map.empty (module Base.String)

  (* Free vars of a type environment is the set of all non-quantified
     type variables of all expressions in a given environment. *)
  let free_vars env =
    Base.Map.fold
      ~init:TVarSet.empty
      ~f:(fun ~key:_ ~data acc -> TVarSet.union acc (Scheme.free_vars data))
      env
  ;;

  (* Apply the substitution to each scheme from the enviroment. *)
  let apply env sub = Base.Map.map env ~f:(Scheme.apply sub)
  let extend env key schema = Base.Map.update ~f:(fun _ -> schema) env key
  let find env key = Base.Map.find env key
end

open R
open R.Syntax

(* Take out a new state, which is a new “type variable”
   from the monad and wrap it in a type variable constructor. *)
let fresh_var = fresh >>| fun name -> tvar name (* *)

(* Create an expression type by using the altered scheme as follows:
   we take all the quantified variables in the type and replace them
   one by one with some type variable. *)
let instantiate : scheme -> typ R.t =
  fun (Scheme (bind_var, ty)) ->
  TVarSet.fold
    (fun var_name acc ->
      let* acc = acc in
      let* fv = fresh_var in
      let* sub = Subst.singleton var_name fv in
      return (Subst.apply sub acc))
    bind_var
    (return ty)
;;

(* Reverse process: create a scheme by using a given type and environment. *)
let generalize : TypeEnv.t -> Type.t -> scheme =
  fun env ty ->
  let free = TVarSet.diff (Type.type_vars ty) (TypeEnv.free_vars env) in
  Scheme (free, ty)
;;

let lookup_env env name =
  (* If the passed name is defined in the enviroment,
     create its type according to the scheme and return it.
     Otherwise issue an error. *)
  match TypeEnv.find env name with
  | Some scheme ->
    let* ty = instantiate scheme in
    return (Subst.empty, ty)
    (* An empty substitution is needed here only for type matching. *)
  | None ->
    if is_upper name.[0] then fail (unbound_effect name) else fail (unbound_variable name)
;;

let annotation_to_type =
  (* Convert a type annotation to a real type. *)
  let rec helper = function
    | AInt -> tint
    | ABool -> tbool
    | AChar -> tchar
    | AString -> tstring
    | AUnit -> tunit
    | AArrow (l, r) -> helper l @-> helper r
    | AList a -> tlist (helper a)
    | ATuple a -> ttuple @@ List.map (fun x -> helper x) a
    | AEffect a -> teffect (helper a)
  in
  helper
;;

let check_unique_vars pattern =
  (* Checks that all variables in the pattern are unique.
     Used to detect severeal bound errors in tuple patterns,
     list constructor patterns, and effects with arguments. *)
  let rec helper var_set = function
    | PVal v ->
      if VarSet.mem v var_set
      then
        (* If at least one variable is found twice, we raise an error. *)
        fail (several_bounds v)
      else return (VarSet.add v var_set)
    | PAny -> return var_set
    | PNill -> return var_set
    | PConst _ -> return var_set
    | PTuple pattern_list -> RList.fold_left pattern_list ~init:(return var_set) ~f:helper
    | PListCons (l, r) ->
      let* left_set = helper var_set l in
      helper left_set r
    | PEffectWithoutArguments _ -> return var_set
    | PEffectWithArguments (_, arg_pattern) -> helper var_set arg_pattern
  in
  helper VarSet.empty pattern
;;

let infer_const c =
  let ty =
    match c with
    | Ast.Int _ -> tint
    | Ast.Bool _ -> tbool
    | Ast.Char _ -> tchar
    | Ast.String _ -> tstring
    | Ast.Unit -> tunit
  in
  return (Subst.empty, ty)
;;

let infer_id env id =
  (* '_' - reserved for expressions whose result is not important to us. *)
  match id with
  | "_" ->
    let* fv = fresh_var in
    return (Subst.empty, fv)
  | _ -> lookup_env env id
;;

let infer_pattern =
  (* The result of the function is the type of the passed pattern
     and the type environment in which this pattern is defined. *)
  let rec helper env = function
    | PVal v ->
      let* fv = fresh_var in
      let schema = Scheme (TVarSet.empty, fv) in
      let env = TypeEnv.extend env v schema in
      return (fv, env)
    | PAny ->
      let* fv = fresh_var in
      return (fv, env)
    | PNill ->
      let* fv = fresh_var in
      let ty = tlist fv in
      return (ty, env)
    | PConst c ->
      let* _, ty = infer_const c in
      return (ty, env)
    | PTuple pattern_list as tuple_p ->
      let* _ = check_unique_vars tuple_p in
      (* Check several bounds *)
      let* ty, env =
        (* Here is the list of types in reverse order. *)
        RList.fold_left
          pattern_list
          ~init:(return ([], env))
          ~f:(fun (acc, env) pattern ->
            let* ty1, env1 = helper env pattern in
            return (ty1 :: acc, env1))
      in
      let ty = ttuple (List.rev ty) in
      return (ty, env)
    | PListCons (l, r) as list_cons ->
      let* _ = check_unique_vars list_cons in
      (* Check several bounds *)
      let* ty1, env1 = helper env l in
      let* ty2, env2 = helper env1 r in
      let* fv = fresh_var in
      let* sub1 = Subst.unify (tlist ty1) fv in
      let* sub2 = Subst.unify ty2 fv in
      let* sub3 = Subst.compose sub1 sub2 in
      let env = TypeEnv.apply env2 sub3 in
      let ty3 = Subst.apply sub3 fv in
      return (ty3, env)
    | PEffectWithoutArguments name ->
      let* _, typ = lookup_env env name in
      return (typ, env)
    | PEffectWithArguments (name, arg_pattern) as eff ->
      let* _ = check_unique_vars eff in
      (* Check several bounds *)
      let* _, effect_typ = lookup_env env name in
      (match effect_typ with
       | TArr (arg_ty, TEffect res_ty) ->
         let* pat_ty, env' = helper env arg_pattern in
         let* sub = Subst.unify pat_ty arg_ty in
         let env'' = TypeEnv.apply env' sub in
         let ty = arg_ty @-> teffect res_ty in
         let ty = Subst.apply sub ty in
         return (ty, env'')
       | _ -> fail (not_effect_with_args name))
  in
  helper
;;

(* By binary operator determine:
   (type of the left operand, type of the right operand, type of the result). *)
let binary_operator_type = function
  | Eq | NEq | Gt | Gte | Lt | Lte ->
    let* fv = fresh_var in
    return (fv, fv, tbool)
  | Add | Sub | Mul | Div -> return (tint, tint, tint)
  | And | Or -> return (tbool, tbool, tbool)
;;

(* By unary operator determine:
   (type of operand, type of the result). *)
let unary_operator_type = function
  | Minus -> return (tint, tint)
  | Plus -> return (tint, tint)
  | Not -> return (tbool, tbool)
;;

let infer_expr =
  let rec helper env = function
    | EConst c -> infer_const c
    | EIdentifier id -> infer_id env id
    | EUnaryOperation (op, expr) ->
      let* arg_type, expr_type = unary_operator_type op in
      let* sub1, ty = helper env expr in
      let* sub2 = Subst.unify ty arg_type in
      let* sub3 = Subst.compose sub1 sub2 in
      return (sub3, expr_type)
    | EBinaryOperation (op, expr1, expr2) ->
      let* arg_type1, arg_type2, expr_type = binary_operator_type op in
      let* sub1, ty1 = helper env expr1 in
      let* sub2, ty2 = helper env expr2 in
      let* sub3 = Subst.unify ty1 arg_type1 in
      let* sub4 = Subst.unify (Subst.apply sub1 ty2) arg_type2 in
      let* sub = Subst.compose_all [ sub1; sub2; sub3; sub4 ] in
      return (sub, expr_type)
    | EFun (pattern, expr) ->
      let* ty1, env1 = infer_pattern env pattern in
      let* sub, ty2 = helper env1 expr in
      let ty = ty1 @-> ty2 in
      let result = Subst.apply sub ty in
      return (sub, result)
    | EApplication (func_expr, arg_expr) ->
      let* sub1, func_ty = helper env func_expr in
      let env' = TypeEnv.apply env sub1 in
      let* sub2, arg_ty = helper env' arg_expr in
      let* result_type = fresh_var in
      let ty1 = Subst.apply sub2 func_ty in
      let ty2 = arg_ty @-> result_type in
      let* sub3 = Subst.unify ty1 ty2 in
      let* sub = Subst.compose_all [ sub1; sub2; sub3 ] in
      let ty = Subst.apply sub result_type in
      return (sub, ty)
    | EIfThenElse (cond, branch1, branch2) ->
      let* sub1, ty1 = helper env cond in
      let* sub2, ty2 = helper env branch1 in
      let* sub3, ty3 = helper env branch2 in
      let* sub4 = Subst.unify ty1 tbool in
      let* sub5 = Subst.unify ty2 ty3 in
      let* sub = Subst.compose_all [ sub1; sub2; sub3; sub4; sub5 ] in
      let ty = Subst.apply sub ty3 in
      return (sub, ty)
    | ELetIn (name, expr1, expr2) ->
      let* sub1, ty1 = helper env expr1 in
      let env2 = TypeEnv.apply env sub1 in
      let schema = generalize env2 ty1 in
      let env2 = TypeEnv.extend env2 name schema in
      let* sub2, t2 = helper env2 expr2 in
      let* sub3 = Subst.compose sub1 sub2 in
      return (sub3, t2)
    | ERecLetIn (name, expr1, expr2) ->
      let* fv = fresh_var in
      let env2 = TypeEnv.extend env name (Scheme (TVarSet.empty, fv)) in
      let* sub1, ty1 = helper env2 expr1 in
      let* sub2 = Subst.unify ty1 fv in
      let* sub3 = Subst.compose sub1 sub2 in
      let ty3 = Subst.apply sub3 fv in
      let env2 = TypeEnv.apply env sub3 in
      let schema = generalize env ty3 in
      let env3 = TypeEnv.extend env2 name schema in
      let* sub4, ty4 = helper env3 expr2 in
      let* sub5 = Subst.compose sub3 sub4 in
      return (sub5, ty4)
    | EEffectWithoutArguments name -> lookup_env env name
    | EEffectWithArguments (name, expr) ->
      let* sub1, ty1 = lookup_env env name in
      (* (Type arg -> effect) from declaration. *)
      let* sub2, ty2 = helper env expr in
      (* Actual arguments type. *)
      (match ty1 with
       | TArr (arg_ty, eff) ->
         let* sub3 = Subst.unify arg_ty ty2 in
         let* sub = Subst.compose_all [ sub1; sub2; sub3 ] in
         return (sub, eff)
       | _ -> fail (not_effect_with_args name))
    | EListCons (l, r) ->
      let* sub1, ty1 = helper env l in
      let env' = TypeEnv.apply env sub1 in
      let* sub2, ty2 = helper env' r in
      let* fv = fresh_var in
      let* sub3 = Subst.unify (tlist ty1) fv in
      let* sub4 = Subst.unify ty2 fv in
      let* sub = Subst.compose_all [ sub1; sub2; sub3; sub4 ] in
      let ty = Subst.apply sub fv in
      return (sub, ty)
    | EList expr_list ->
      let* fv = fresh_var in
      let rec infer_list acc = function
        | [] -> return (acc, tlist fv)
        | hd :: tl ->
          let* sub1, ty1 = helper env hd in
          let* sub2 = Subst.unify ty1 fv in
          let* sub = Subst.compose sub1 sub2 in
          infer_list (sub :: acc) tl
      in
      let* sub, ty = infer_list [] expr_list in
      let* sub = Subst.compose_all sub in
      let ty = Subst.apply sub ty in
      return (sub, ty)
    | ETuple expr_list ->
      let rec infer_tuple acc = function
        | [] -> return acc
        | hd :: tl ->
          let* sub1, ty1 = helper env hd in
          let acc_sub, acc_ty = acc in
          let* sub2 = Subst.compose sub1 acc_sub in
          let new_acc = sub2, ty1 :: acc_ty in
          infer_tuple new_acc tl
      in
      let acc = Subst.empty, [] in
      let* sub, ty = infer_tuple acc expr_list in
      let ty_list = List.rev_map (Subst.apply sub) ty in
      let ty = ttuple ty_list in
      return (sub, ty)
    | EMatchWith (expr, cases) ->
      let* sub1, ty1 = helper env expr in
      let env2 = TypeEnv.apply env sub1 in
      let* fv = fresh_var in
      let f acc case =
        let acc_sub, acc_ty = acc in
        let pat, expr = case in
        let* pat_ty, pat_env = infer_pattern env2 pat in
        let* sub2 = Subst.unify ty1 pat_ty in
        let env3 = TypeEnv.apply pat_env sub2 in
        let* expr_sub, expr_ty = helper env3 expr in
        let* sub3 = Subst.unify expr_ty acc_ty in
        let* sub = Subst.compose_all [ acc_sub; expr_sub; sub2; sub3 ] in
        let ty = Subst.apply sub acc_ty in
        return (sub, ty)
      in
      RList.fold_left cases ~init:(return (sub1, fv)) ~f
    | ETryWith (expr, body) ->
      (* Find the expr type, and then check each handler for correctness.
         If everything is correct, we return expr as the type. *)
      let* sub, typ = helper env expr in
      let* result =
        RList.fold_left body ~init:(return []) ~f:(fun acc handler ->
          let* _, handler_typ = infer_handler env handler in
          match handler_typ with
          | TContinuation cont_ty ->
            let* _ = Subst.unify cont_ty typ in
            return acc
          | _ -> return [ handler ])
      in
      (match result with
       | [] -> return (sub, typ)
       | _ -> fail handler_without_continue)
    | EEffectContinue (cont, expr) ->
      (match cont with
       | Continue k ->
         (* Check that the continuation variable is defined in the environment. *)
         let cont_var = eidentifier k in
         let* sub1, typ = helper env cont_var in
         (match typ with
          (* Check that the found variable value is a continuation point. *)
          | TContinuePoint ->
            let* sub2, ty_expr = helper env expr in
            let* sub = Subst.compose sub1 sub2 in
            return (sub, tcontinuation ty_expr)
          | _ -> fail (not_continue_val k)))
    | EEffectPerform expr ->
      let* sub1, ty1 = helper env expr in
      (match ty1 with
       | TEffect _ | TVar _ ->
         (* TVar is needed here to handle cases where a function argument is passed to perform. *)
         let* eff_ty = fresh_var in
         let* sub2 = Subst.unify ty1 (teffect eff_ty) in
         let* sub = Subst.compose sub1 sub2 in
         let ty = Subst.apply sub eff_ty in
         return (sub, ty)
       | _ -> fail perform_with_no_effect)
  and infer_handler env handler =
    (* Check that the effect handler is defined correctly.
       If everything is correct, it returns the continuation type. *)
    let handler_type env expr = function
      | Continue k ->
        let schm = Scheme (TVarSet.empty, tcontinue_point) in
        let env' = TypeEnv.extend env k schm in
        let* env'', typ = helper env' expr in
        return (env'', typ)
    in
    match handler with
    | EffectHandler (pat, expr, cont) ->
      (match pat with
       | PEffectWithArguments (name, pat_n) ->
         let* sub1, ty1 = lookup_env env name in
         let* ty2, env' = infer_pattern env pat_n in
         (match ty1 with
          | TArr (arg_ty, _) ->
            (* Check that the thoring value will correspond to the expected one. *)
            let* sub2 = Subst.unify arg_ty ty2 in
            let* sub = Subst.compose_all [ sub1; sub2 ] in
            let env'' = TypeEnv.apply env' sub in
            (* Check the pattern effect for several bound. *)
            let* _ = infer_pattern env pat in
            handler_type env'' expr cont
          | _ -> fail (not_effect_with_args name))
       | PEffectWithoutArguments name ->
         let* _, ty = lookup_env env name in
         (match ty with
          | TEffect _ ->
            let* _ = infer_pattern env pat in
            (* Check the pattern effect for several bound. *)
            handler_type env expr cont
          | _ -> fail (not_effect_without_args name))
       | _ -> fail handler_without_effect)
  in
  helper
;;

let update_name_list name names_list = name :: List.filter (( <> ) name) names_list
(* Add a new name to the beginning of the list. If it is already there, delete it and add it to the beginning. *)

let infer_program env program =
  (* Go through the list and take turns making expressions.
     Since map is strictly ordered to output all names in the order in which they
     were declared, provide a list of let binding names and the effect of declarations. *)
  let rec helper acc = function
    | [] -> acc
    | hd :: tl ->
      let* acc_env, acc_names = acc in
      (match hd with
       | SDeclaration decl ->
         (match decl with
          | DDeclaration (name, expr) ->
            let* _, ty = infer_expr acc_env expr in
            let new_acc = TypeEnv.extend acc_env name (Scheme (TVarSet.empty, ty)) in
            let new_names_list = update_name_list name acc_names in
            helper (return (new_acc, new_names_list)) tl
          | DRecDeclaration (name, expr) ->
            let* fv = fresh_var in
            let env2 = TypeEnv.extend acc_env name (Scheme (TVarSet.empty, fv)) in
            let* sub1, ty1 = infer_expr env2 expr in
            let* sub2 = Subst.unify ty1 fv in
            let* sub3 = Subst.compose sub1 sub2 in
            let ty3 = Subst.apply sub3 fv in
            let new_acc = TypeEnv.extend acc_env name (Scheme (TVarSet.empty, ty3)) in
            let new_names_list = update_name_list name acc_names in
            helper (return (new_acc, new_names_list)) tl
          | DEffectDeclaration (name, annot) ->
            let* ty =
              match annot with
              | AEffect _ as a -> return (annotation_to_type a) (* Effect without args. *)
              | AArrow (l, r) ->
                let l_ty = annotation_to_type l in
                let r_ty = annotation_to_type r in
                let ty = l_ty @-> r_ty in
                (match r_ty with
                 | TEffect _ -> return ty (* At the end there is an effect type. *)
                 | _ -> fail (wrong_effect_type name ty))
              | _ ->
                (* effect E : primititve type *)
                let ty = annotation_to_type annot in
                fail (wrong_effect_type name ty)
            in
            let new_acc = TypeEnv.extend acc_env name (Scheme (TVarSet.empty, ty)) in
            let new_names_list = update_name_list name acc_names in
            helper (return (new_acc, new_names_list)) tl)
       | SExpression expr ->
         let* _, _ = infer_expr acc_env expr in
         acc)
  in
  (* Start *)
  let* env, names = helper (return (env, [])) program in
  return (env, List.rev names)
;;

let start_env = TypeEnv.empty
let run_expr_inferencer expr = Result.map snd (run (infer_expr start_env expr))
let run_program_inferencer program = run (infer_program start_env program)
(* Here program <-> [SDeclarations ..., SDeclaration ..., ...] *)
