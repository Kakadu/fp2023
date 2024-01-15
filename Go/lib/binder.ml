open Base

(* open Monad *)
open Ast
open Binded_ast
(* open Ident *)

module Scope = struct
  type scope =
    { depth : int
    ; parent_scope : scope option
    ; content : (string, int, String.comparator_witness) Map.t
    }

  let find_name scope name = Map.find scope.content name

  let rec name_resolve scope name =
    match find_name scope name with
    | Some int -> Some int
    | _ ->
      (match scope.parent_scope with
       | None -> None
       | Some parent_scope -> name_resolve parent_scope name)
  ;;

  type res =
    | NotDeclared of int * scope
    | Declared

  let name_declare scope name =
    match find_name scope name with
    | Some _ -> Declared
    | None ->
      NotDeclared
        ( scope.depth
        , { scope with content = Map.set scope.content ~key:name ~data:scope.depth } )
  ;;

  let make_empty_scope scope depth =
    { depth; parent_scope = Some scope; content = Map.empty (module String) }
  ;;
end

type current_state =
  { internal_id : int
  ; cur_scope : Scope.scope
  ; errors : string list
  }

module Step = Monades.STATE_MONAD (struct
    type t = current_state
  end)

open Step

let add_err s =
  get
  >>= fun state ->
  let state = { state with errors = s :: state.errors } in
  put state
;;

let name_declare name =
  get
  >>= fun state ->
  match Scope.name_declare state.cur_scope name with
  | NotDeclared (depth, scope) ->
    put { state with cur_scope = scope } >>= fun _ -> return depth
  | Declared ->
    add_err ("Identifier " ^ name ^ " already declared in this scope")
    >>= fun _ -> return (-2)
;;

(* | None -> *)

let name_resolve name =
  get
  >>= fun state ->
  match Scope.name_resolve state.cur_scope name with
  | Some depth -> return depth
  | None ->
    add_err ("Identifier " ^ name ^ " already declared in this scope")
    >>= fun _ -> return (-2)
;;

let get_new_scope =
  get
  >>= fun state ->
  let depth = state.internal_id in
  let current_scope = state.cur_scope in
  let internal_scope = Scope.make_empty_scope current_scope depth in
  put { state with internal_id = depth + 1; cur_scope = internal_scope }
  >>= fun _ -> return current_scope
;;

let save_scope scope = get >>= fun state -> put { state with cur_scope = scope }

let rec lookup_b_expr = function
  | Const c -> return (ConstB c)
  | Identifier name ->
    name_resolve name >>= fun depth -> return (IdentifierB (depth, name))
  | FunCall (f, args) ->
    lookup_b_expr f
    >>= fun f -> lookup_many_expr args >>= fun args -> return (FunCallB (f, args))
(* | AnonFunc (sign, b) -> *)

(* | BinaryOp (l op r) -> bind_expr *)
and lookup_many_expr expr = many expr ~f:lookup_b_expr

(* and lookup_b_declare_func sign b = get_new_scope >>= fun cur_scope -> lookup_b_declare_sign >>= fun sign -> *)

(* and lookup_b_stmt = function
   | *)

and lookup_b_declare_sign { args; ret } =
  let lookup_b_declare_arg (name, t) =
    name_declare name >>= fun depth -> return (depth, name, t)
  in
  many args ~f:lookup_b_declare_arg >>= fun argsB -> return { argsB; retB = ret }
;;
