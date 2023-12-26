(** Copyright 2023-2024, Vadim Yakshigulov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

val is_space : char -> bool
val is_reserved : string -> bool
val is_end_of_line : char -> bool
val is_decimal_digit : char -> bool
val is_letter : char -> bool
val space : unit Angstrom.t
val spaces : unit Angstrom.t
val trim : 'a Angstrom.t -> 'a Angstrom.t
val comma : char Angstrom.t
val comment : unit Angstrom.t
val between : 'a Angstrom.t -> 'b Angstrom.t -> 'c Angstrom.t -> 'c Angstrom.t
val skip_empty_lines : unit Angstrom.t
val label_name : string Angstrom.t
