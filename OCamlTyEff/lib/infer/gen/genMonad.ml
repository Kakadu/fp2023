open! Base
open Types
open Constraints

open Containers
open Utils

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
    Reader.local (fun cur -> VarSet.union cur @@ VarSet.of_list vars) m

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
end
