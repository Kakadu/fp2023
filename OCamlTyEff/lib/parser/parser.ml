(** Copyright 2023 Danil S, Andrei *)

(** SPDX-License-Identifier: MIT *)

open! Base

let parse str =
  Angstrom.parse_string ~consume:Angstrom.Consume.All Structure.parse_structure
    str
  |> Result.ok
