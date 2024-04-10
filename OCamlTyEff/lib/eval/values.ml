(** Copyright 2023-2024, Danil S, Andrei *)

(** SPDX-License-Identifier: MIT *)

open! Base
open Monads.Std
open Misc
open Ast

module rec Val : sig
  type t =
    | Val_const of constant
    | Val_tuple of t list
    | Val_con of Ident.t * t option
    | Val_fun of funct
    | Val_ref of Env.Link.t
        (** Reference to a value.
            Contains [Link] that's used to lookup / reassign a value *)

  and funct =
    | Fun_cases of case list * Bounds.t lazy_t
    | Fun_closure of pattern list * expression * Bounds.t lazy_t
    | Fun_builtin of (t -> t EvalMonad.t)

  val pp : Env.t -> Format.formatter -> t -> unit

  val bool_true : t
  val bool_false : t
  val unit : t
end = struct
  type t =
    | Val_const of constant
    | Val_tuple of t list
    | Val_con of Ident.t * t option
    | Val_fun of funct
    | Val_ref of Env.Link.t

  and funct =
    | Fun_cases of case list * Bounds.t lazy_t
    | Fun_closure of pattern list * expression * Bounds.t lazy_t
    | Fun_builtin of (t -> t EvalMonad.t)

  let rec pp env ppf =
    let open Format in
    function
    | Val_const const -> (
      match const with
      | Const_char ch ->
          fprintf ppf "'%c'" ch
      | Const_integer int ->
          fprintf ppf "%i" int
      | Const_string str ->
          fprintf ppf {|"%s"|} str )
    | Val_tuple values ->
        let pp_values =
          pp_print_list (pp env) ~pp_sep:(fun out () -> fprintf out ", ")
        in
        fprintf ppf "(%a)" pp_values values
    | Val_fun _ ->
        fprintf ppf "<fun>"
    | Val_con (Ident name, Some (Val_con (_, Some _) as arg)) ->
        fprintf ppf "%s (%a)" name (pp env) arg
    | Val_con (Ident name, Some arg) ->
        fprintf ppf "%s %a" name (pp env) arg
    | Val_con (Ident name, None) ->
        fprintf ppf "%s" name
    | Val_ref link ->
        let ref_value =
          Env.deref env link |> Option.value_exn ~message:"Invalid ref"
        in
        fprintf ppf "ref { %a }" (pp env) ref_value

  let bool_true = Val_con (Ident "true", None)
  let bool_false = Val_con (Ident "false", None)
  let unit = Val_con (Ident "()", None)
end

and Bounds : sig
  (** Local bounds. Maps identifiers to values *)
  type t

  val empty : t
  val singleton : Ident.t -> Val.t -> t

  val idents : t -> Ident.t list

  val set : t -> key:Ident.t -> data:Val.t -> t
  val find : t -> Ident.t -> Val.t option
  val find_exn : t -> Ident.t -> Val.t

  val merge : t -> t -> t
end = struct
  type t = (Ident.t, Val.t, Ident.comparator_witness) Map.t

  let empty = Map.empty (module Ident)
  let singleton k = Map.singleton (module Ident) k

  let idents = Map.keys

  let set = Map.set
  let find = Map.find
  let find_exn = Map.find_exn

  let merge = Map.merge_skewed ~combine:(fun ~key:_ _ v2 -> v2)
end

and Env : sig
  module Link : sig
    (** Link to a value *)
    type t

    val equal : t -> t -> bool
  end

  (** Evaluation environment *)
  type t

  val empty : t

  val get_bounds : t -> Bounds.t
  val set_bounds : t -> Bounds.t -> t

  val fresh_link : t -> Val.t -> t * Link.t
  (** Create fresh link for a value *)

  val assign : t -> Link.t -> Val.t -> t
  (** Assign a new value by link *)

  val deref : t -> Link.t -> Val.t option
  (** Get value by link *)
end = struct
  module Link = struct
    module T = struct
      type t = Link of int [@@deriving eq, ord, sexp_of]
    end

    include T
    include Comparator.Make (T)
  end

  type t =
    { local_bounds: Bounds.t
    ; links: (Link.t, Val.t, Link.comparator_witness) Map.t
    ; link_counter: int }

  let empty =
    {local_bounds= Bounds.empty; links= Map.empty (module Link); link_counter= 0}

  let get_bounds env = env.local_bounds
  let set_bounds env local_bounds = {env with local_bounds}

  let fresh_link env value =
    let link = Link.Link env.link_counter in
    let links = Map.set env.links ~key:link ~data:value in
    ({env with links; link_counter= env.link_counter + 1}, link)

  let assign env link value =
    {env with links= Map.set env.links ~key:link ~data:value}

  let deref env = Map.find env.links
end

and EvalError : sig
  type t =
    | TypeError
    | Exception of Val.t
    | NotImplemented of string
        (** Feature is not yet implemented.
        The argument contains the feature description *)

  val pp : Env.t -> Format.formatter -> t -> unit

  val exc : string -> t
end = struct
  type t = TypeError | Exception of Val.t | NotImplemented of string

  let pp env ppf =
    let open Format in
    function
    | TypeError ->
        fprintf ppf "TypeError\n"
    | Exception exc ->
        fprintf ppf "Exception: %a\n" (Val.pp env) exc
    | NotImplemented desc ->
        fprintf ppf "NotImplemented: %s\n" desc

  let exc name = Exception (Val.Val_con (Ident name, None))
end

and EvalMonad : sig
  include Monad.S

  val run :
       printer:(string -> unit)
    -> 'a t
    -> Env.t
    -> ('a * Env.t, EvalError.t) result

  module Eval : sig
    val fail : EvalError.t -> 'a t
    val catch : 'a t -> (EvalError.t -> 'a t) -> 'a t

    val get_env : Env.t t
    val get_printer : (string -> unit) t

    val set_env : Env.t -> unit t

    val with_bounds : Bounds.t -> 'a t -> 'a t
    (** Run computation in modified environment with provided bounds *)

    val extend_bounds : Bounds.t -> 'a t -> 'a t
    (** Run computation in modified environment extended with provided bounds *)
  end
end = struct
  module StateT = struct
    type t = {env: Env.t; printer: string -> unit}
  end

  include MakeESMonad (EvalError) (StateT)

  let run ~printer m env =
    run m {env; printer}
    |> Result.map ~f:(fun (ret, {StateT.env; _}) -> (ret, env))

  module Eval = struct
    let fail = Error.fail
    let catch = Error.catch

    let get_env =
      let* {env; _} = State.get in
      return env

    let get_printer =
      let* {printer; _} = State.get in
      return printer

    let set_env env =
      let* st = State.get in
      State.put {st with env}

    let with_bounds new_bounds m =
      let* {env; printer} = State.get in
      let prev_bounds = Env.get_bounds env in

      match run ~printer m (Env.set_bounds env new_bounds) with
      | Ok (res, new_env) ->
          let* () = set_env (Env.set_bounds new_env prev_bounds) in
          return res
      | Error err ->
          fail err

    let extend_bounds extend_with m =
      let* env = get_env in
      with_bounds (Bounds.merge (Env.get_bounds env) extend_with) m
  end
end
