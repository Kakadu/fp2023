module Assumptions = Common.Assumptions
module ConArityAssumpt = Common.ConArityAssumpt

open! Base

let gen str_item =
  GenMonad.run @@ Structure.gen str_item
  |> Result.map ~f:(fun ((asm, ty), constrs, con_assumpt) ->
         (asm, ty, constrs, con_assumpt) )
