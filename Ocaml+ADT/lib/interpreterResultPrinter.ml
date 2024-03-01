(** Copyright 2023-2024, tepa46 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open InferencerTypes
open Inferencer
open Format

let pp_env_res fmt (infer_env, interpret_env) =
  StringMap.iter
    (fun key data ->
      let scheme = StringMap.find_opt key infer_env in
      match scheme with
      | Some scheme ->
        fprintf
          fmt
          "%S: %a = %a\n"
          key
          Scheme.pp_scheme
          scheme
          InterpreterTypes.pp_value
          data
      | None -> fprintf fmt "%S = %a\n" key InterpreterTypes.pp_value data)
    interpret_env
;;

let pp_interpret_res adt_env env res =
  printf "%a%a" AdtEnv.pp_env adt_env pp_env_res (env, res)
;;
