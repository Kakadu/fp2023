(** Copyright 2023-2024, Danil S, Andrei *)

(** SPDX-License-Identifier: MIT *)

open! Base
open LTerm_text
open Lwt

open Misc
open Types
open Eval

module TyEnv = Types.Env
module EvalEnv = Eval.Env

type env = TyEnv.t * EvalEnv.t

exception ParseError
exception TypeError of string
exception EvalError of LTerm_text.t

let ( let* ) x f = bind x f

let err_color = LTerm_style.lred

let std_env =
  let prelude =
    Parse.parse Builtin.prelude
    |> Option.value_exn ~message:"Failed to parse prelude"
  in
  List.fold prelude ~init:(Builtin.ty_env, Builtin.eval_env)
    ~f:(fun (ty_env, eval_env) str_item ->
      let ty_env, _, _ =
        Infer.infer_structure_item ty_env str_item
        |> Result.map_error ~f:(fun _ -> "Failed to infer prelude")
        |> Result.ok_or_failwith
      in
      let eval_env, _, _ =
        Eval.eval_structure_item ~printer:(fun _ -> ()) eval_env str_item
        |> Result.map_error ~f:(fun _ -> "Failed to eval prelude")
        |> Result.ok_or_failwith
      in
      (ty_env, eval_env) )

let parse str =
  match Parse.parse str with Some res -> return res | None -> fail ParseError

let ty_error_to_string = function
  | Infer.TyError.UnificationMismatch ->
      "Unification mismatch"
  | UnificationFailTy (ty1, ty2) ->
      Format.asprintf "Failed to unify %a and %a" Ty.pp ty1 Ty.pp ty2
  | UnificationFailEff (eff1, eff2) ->
      Format.asprintf "Failed to unify effects %a and %a" Eff.pp eff1 Eff.pp
        eff2
  | UnboundVariable (Ident name) ->
      Format.sprintf "Unbound value %s" name
  | OccursInTy (var, ty) ->
      Format.asprintf "The type variable %a occurs inside %a" Var.pp var Ty.pp
        ty
  | OccursInEff (var, eff) ->
      Format.asprintf "The effect variable %a occurs inside %a" Var.pp var
        Eff.pp eff
  | RecursiveEffRows ->
      "Recursive effect rows"
  | PatVarBoundSeveralTimes (Ident name) ->
      Format.sprintf "Variable %s is bound several times" name
  | ConstructorArityMismatch (Ident name) ->
      Format.sprintf
        "The constructor %s expects more/less arguments than applied here" name
  | NotVarLHSRec ->
      "Only variables are allowed as left-hand side of `let rec`"
  | NotAllowedRHSRec _ ->
      "The expression is not allowed as right-hand side of `let rec`"
  | UnboundTypeVariable name ->
      Format.sprintf "The type variable '%s is unbound in the type declaration"
        name
  | UnboundType (Ident name) ->
      Format.sprintf "Unbound type %s" name
  | TypeArityMismatch (Ident name) ->
      Format.sprintf "The type %s expects more/less arguments than applied here"
        name
  | NotImplemented desc ->
      Format.sprintf {|"%s" is not yet implemented :(|} desc

let infer_item ty_env str_item =
  match Infer.infer_structure_item ty_env str_item with
  | Ok res ->
      return res
  | Error err ->
      fail @@ TypeError (ty_error_to_string err)

let eval_error_to_text eval_env = function
  | EvalError.TypeError ->
      of_utf8 "Type error occured during evaluation 0_0"
  | Exception exc ->
      let exc_str = Format.asprintf "%a" (Val.pp eval_env) exc in
      eval [B_fg err_color; S "Exception: "; E_fg; S exc_str]
  | NotImplemented desc ->
      of_utf8 @@ Format.sprintf {|"%s" is not yet implemented :(|} desc

let eval_item ~term eval_env str_item =
  match
    Eval.eval_structure_item
      ~printer:(fun str ->
        let (_ : unit t) = LTerm.fprint term str in
        () )
      eval_env str_item
  with
  | Ok res ->
      return res
  | Error err ->
      fail @@ EvalError (eval_error_to_text eval_env err)

let print_value ~term eval_env name (Scheme.Forall (_, ty)) value =
  let ty_str = Format.asprintf "%a" Ty.pp ty in
  let val_str =
    match value with
    | Some value ->
        Format.asprintf " = %a" (Val.pp eval_env) value
    | None ->
        ""
  in
  LTerm.fprintlf term "%s : %s%s" name ty_str val_str

let interpret_exn ~term env str () =
  let* structure = parse str in
  Lwt_list.fold_left_s
    (fun (ty_env, eval_env) str_item ->
      let* ty_env, bound_vars, sc = infer_item ty_env str_item in
      let* eval_env, _, value = eval_item ~term eval_env str_item in

      let* () =
        match (sc, value) with
        | Some sc, value ->
            print_value ~term eval_env "-" sc value
        | _ ->
            return ()
      in

      let* () =
        Lwt_list.iter_s
          (fun (Ident.Ident name as id) ->
            let sc = TyEnv.find_exn ty_env id in
            let value = Bounds.find (EvalEnv.get_bounds eval_env) id in
            print_value ~term eval_env name sc value )
          bound_vars
      in

      return (ty_env, eval_env) )
    env structure

let interpret ~term env str =
  Lwt.catch (interpret_exn ~term env str) (function
    | ParseError ->
        let* () =
          LTerm.fprintls term (eval [B_fg err_color; S "Syntax error"; E_fg])
        in
        return env
    | TypeError err ->
        let* () =
          LTerm.fprintls term (eval [B_fg err_color; S "Error: "; E_fg; S err])
        in
        return env
    | EvalError err ->
        let* () = LTerm.fprintls term err in
        return env
    | exn ->
        fail exn )
