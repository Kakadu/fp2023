open! Base
open Ast

module Label = struct
  type t = Label of Ident.t * Ident.t option [@@deriving eq, ord, sexp_of]

  let pp ppf (Label (Ident name, arg)) =
    let open Stdlib.Format in
    match arg with
    | None ->
        fprintf ppf "%s" name
    | Some (Ident arg) ->
        fprintf ppf "%s %s" name arg

  let console = Label (Ident "console", None)
  let exn = Label (Ident "exn", None)
end

type t = Eff_var of Var.t | Eff_total | Eff_row of Label.t * t
[@@deriving eq, ord, sexp_of]

let pp ppf =
  let open Stdlib.Format in
  let rec helper ppf = function
    | Eff_row (lbl, eff_rest) ->
        fprintf ppf ", %a%a" Label.pp lbl helper eff_rest
    | Eff_total ->
        fprintf ppf ""
    | Eff_var var ->
        fprintf ppf " | %a" Var.pp var
  in
  function
  | Eff_var var ->
      fprintf ppf "-%a" Var.pp var
  | Eff_total ->
      fprintf ppf ""
  | Eff_row (lbl, eff_rest) ->
      fprintf ppf "-[%a%a]" Label.pp lbl helper eff_rest

let rec vars = function
  | Eff_var var ->
      VarSet.singleton_eff var
  | Eff_row (_, eff_rest) ->
      vars eff_rest
  | Eff_total ->
      VarSet.empty
