open! Base
open Utils
open Types
open Constraints

module ConstrSetMonoid = struct
  type t = ConstrSet.t

  let zero = ConstrSet.empty

  let plus = ConstrSet.union

  let ( @@ ) = plus

  let concat = ConstrSet.union_list
end

include MakeRWSMonad (VarSet) (ConstrSetMonoid) (Int)

let run m =
  (* start with empty set of monomoprhic variables & 0 in fresh vars counter *)
  let ret, constrs, _ = run m VarSet.empty 0 in
  (ret, constrs)

module Gen = struct
  let varset = Reader.read

  let extend_varset vars m =
    Reader.local (fun cur -> VarSet.union cur @@ VarSet.of_list vars) m

  let add_constrs constrs = Writer.write @@ ConstrSet.of_list constrs

  let fresh_var =
    let* count = State.get in
    let* () = State.put (count + 1) in
    (* "gen" prefix is important to avoid collision
       with vars created in solve monad *)
    return @@ Var.Var ("gen" ^ Int.to_string count)
end
