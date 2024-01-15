(** Copyright 2023-2024, Danil S, Andrei *)

(** SPDX-License-Identifier: MIT *)

open! Base
open Misc
open Types
open Constraints
open Ast

module Assumptions = struct
  type t = (Ident.t, Var.t list, Ident.comparator_witness) Map.t

  let empty = Map.empty (module Ident)
  let singleton name tv = Map.singleton (module Ident) name [tv]

  let lookup = Map.find_multi
  let remove = Map.remove
  let merge = Map.merge_skewed ~combine:(fun ~key:_ v1 v2 -> List.append v1 v2)

  let idents = Map.keys
  let fold = Map.fold
end

module ConArityAssumpt = struct
  type arity = NoArgs | SomeArgs [@@deriving eq]

  type t = (Ident.t, arity, Ident.comparator_witness) Map.t

  let empty = Map.empty (module Ident)

  let set map con_id arity = Map.set map ~key:con_id ~data:arity
  let find = Map.find
end

module GenMonad = struct
  module ConstrSetMonoid = struct
    type t = ConstrSet.t
    let zero = ConstrSet.empty
    let plus = ConstrSet.union
    let ( @@ ) = plus
    let concat = ConstrSet.union_list
  end

  type gen_state = {con_assumpt: ConArityAssumpt.t; fresh_count: int}

  include
    MakeRWSEMonad (VarSet) (ConstrSetMonoid)
      (struct
        type t = gen_state
      end)
      (TyError)

  let run m =
    (* start with empty set of monomoprhic variables & 0 in fresh vars counter *)
    run m VarSet.empty {con_assumpt= ConArityAssumpt.empty; fresh_count= 0}
    |> Result.map ~f:(fun (ret, constrs, {con_assumpt; _}) ->
           (ret, constrs, con_assumpt) )

  module Gen = struct
    let fail = Error.fail

    let varset = Reader.read
    let extend_varset vars m =
      Reader.local
        (fun cur ->
          VarSet.union cur @@ VarSet.of_list
          @@ Base.List.map vars ~f:(fun var -> VarSet.Elt.Var_ty var) )
        m

    let add_constrs constrs = Writer.write @@ ConstrSet.of_list constrs
    let add_con_assumpt con_id arity =
      let* ({con_assumpt; _} as st) = State.get in
      (* check for arity mismatch *)
      let* () =
        match ConArityAssumpt.find con_assumpt con_id with
        | None ->
            return ()
        | Some ar ->
            if ConArityAssumpt.equal_arity arity ar then return ()
            else fail @@ ConstructorArityMismatch con_id
      in
      (* set arity for constructor *)
      let* () =
        State.put
          {st with con_assumpt= ConArityAssumpt.set con_assumpt con_id arity}
      in
      return ()

    let fresh_var =
      let* ({fresh_count; _} as st) = State.get in
      let* () = State.put {st with fresh_count= fresh_count + 1} in

      (* "gen" prefix is important to avoid collision
         with vars created in solve monad *)
      return @@ Var.Var ("gen" ^ Int.to_string fresh_count)

    let fresh_eff =
      let* var = fresh_var in
      return @@ Eff.Eff_var var
  end
end

let ( ! ) tv = Ty.Ty_var tv
let ( == ) t1 t2 = Constr.TyEqConstr (t1, t2, Unify_eff)
let ( === ) eff1 eff2 = Constr.EffEqConstr (eff1, eff2, EffEq_Normal)
let ( ++ ) = Assumptions.merge
let ( -- ) asm = List.fold ~init:asm ~f:Assumptions.remove

let type_of_constant = function
  | Const_integer _ ->
      Ty.int
  | Const_char _ ->
      Ty.char
  | Const_string _ ->
      Ty.string

open GenMonad.Gen
open GenMonad.Let_syntax

let check_rec_rhs bound_id = function
  | Exp_fun _ | Exp_function _ | Exp_constant _ ->
      return ()
  | Exp_ident id when not (Ident.equal id bound_id) ->
      return ()
  | exp ->
      fail @@ NotAllowedRHSRec exp
