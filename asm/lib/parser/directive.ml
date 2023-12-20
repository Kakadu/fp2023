(** Copyright 2023-2024, Vadim Yakshigulov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Common
open Angstrom
open Ast

let parse_section =
  let sections = [ ".data"; ".text" ] in
  let parser = choice (List.map string sections) in
  section <$> string_ci "section" *> trim parser
;;

let parse_global =
  let parser = label_name in
  global <$> string_ci "global" *> trim parser
;;

let parse_directive = choice [ parse_global; parse_section ]
