(** Copyright 2021-2023, Georgy Sichkar *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(* ==================== Bugs | Different behavior  ===================== *)
(*
   TODO: Using constructor with a fild initialization inside the constructor.
(This behavior is currently allowed, although it shouldn't be)

   Example:
    class A {
        B classb;
        A(){
            classb = new B();
        }
    }
*)

val interpret : string -> (Env_types.Eval_env.t_env_value option, Errors.error) result
