(** Copyright 2021-2023, Georgy Sichkar *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(* ==================== Bugs | Different behavior  ===================== *)
(*
   TODO: Define fields based on other fields.
(This behavior is currently allowed, although it shouldn't be)

   Example:
    int fild_a = 10;
    int fild_b = fild_a;

   TODO: Accessing an uninitialized variable.
( At the type checking stage, this is considered correct behavior )

   Example:
    int a;
    int b = 2 + a;

   TODO: Not supported overloading identifiers of any kind.

  Example: 
    int fild_a = 10;
    ...
    void Main() {
      bool fild_a = 2 + a;
      }

   TODO: Calling a class constructor using a dot is undefined behavior.
( Depending on whether the constructor is defined by the user or not, 
the program may either crash with a typecheck error or run correctly. )

  Example: 
    class A{}
    class B{
      A fild_a = new A.A();
    }

   TODO: Calling a class constructor using a dot is undefined behavior.
( Depending on whether the constructor is defined by the user or not, 
the program may either crash with a typecheck error or run correctly. )

  Example: 
    class A{}
    class B{
      A fild_a = new A.A();
    }

   TODO: New + constructor doesn't chech.
( At this stage, errors of using a constructor without the operation new 
and new without a constructor are not checked. )

  Example: 
    class A{}
    class B{
      A fild_a = new A.A();
    }

   TODO: Steps problem.
( Below is the incorrect behavior )
  Example: 
    for(int a = 10;;){}
    int a = 4;

   TODO: Calling a method.
( Below is the incorrect behavior )
  Example: 
    <TReturn _> method_a();
*)

val check_expr
  :  Ast.expr
  -> Env_types.Type_check_env.type_check_ctx
  -> Env_types.Type_check_env.type_check_ctx * (Env_types.Type_check_env.t_env_value option, Errors.error) result

val check_statement
  :  Ast.statement
  -> Env_types.Type_check_env.type_check_ctx
  -> Env_types.Type_check_env.type_check_ctx * (Env_types.Type_check_env.tp_checked, Errors.error) result

val type_check : Ast.tast -> Env_types.Type_check_env.type_check_ctx * (unit, Errors.error) result
