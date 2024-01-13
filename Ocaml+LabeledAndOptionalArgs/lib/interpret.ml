(** Copyright 2021-2023, LeonidElkin *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Base
open Ast
open Parser
open Format

module type MONAD = sig
  include Base.Monad.S2

  val fail : 'e -> (_, 'e) t
end

module Interpret (M : MONAD) = struct end
