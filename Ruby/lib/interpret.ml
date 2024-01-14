(** Copyright 2023-2024, Ermolovich Anna *)

(** SPDX-License-Identifier: CC0-1.0 *)

open Ast

module type MONAD = sig
  type 'a t

  val return : 'a -> 'a t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
end

module type MONADERROR = sig
  include MONAD

  val fail : string -> 'a t
end

module Result = struct
  type 'a t = ('a, string) Result.t

  let ( >>= ) = Result.bind
  let ( let* ) = ( >>= )
  let return = Result.ok
  let fail = Result.error
end

module Eval (M : MONADERROR) = struct
  open M

  (* Context *)
  type var_ctx =
    { id : id
    ; value : value
    }

  type func_ctx =
    { id : id
    ; args : id list
    ; stmts : statement list
    }

  type class_ctx =
    { id : id
    ; vars : var_ctx list
    ; funcs : func_ctx list
    }

  type global_ctx =
    { id : id
    ; vars : var_ctx list
    ; sub_ctx : global_ctx list
    ; funcs : func_ctx list
    ; classes : class_ctx list
    ; signal : signal
    ; last_return : value
    ; output : string list (** holds values to be printed on a console *)
    }

  let tmp_ctx =
    { id = Id "tmp"
    ; vars = []
    ; sub_ctx = []
    ; funcs = []
    ; classes = []
    ; signal = Work
    ; last_return = Null
    ; output = []
    }
  ;;

  let tmp_class_ctx = { id = Id "tmp"; vars = []; funcs = [] }

  let main_ctx =
    { id = Id "main"
    ; vars = []
    ; sub_ctx = [ tmp_ctx ]
    ; funcs = []
    ; classes = []
    ; signal = Work
    ; last_return = Null
    ; output = []
    }
  ;;

  let if_var_exists_in_ctx i ctx = List.exists (fun (x : var_ctx) -> i = x.id) ctx.vars
  let if_func_exists_in_ctx i ctx = List.exists (fun (x : func_ctx) -> i = x.id) ctx.funcs

  let if_class_exists_in_ctx i ctx =
    List.exists (fun (x : class_ctx) -> i = x.id) ctx.classes
  ;;

  let if_func_exists_in_class_ctx i (ctx : class_ctx) =
    List.exists (fun (x : func_ctx) -> i = x.id) ctx.funcs
  ;;

  let add_var_in_ctx id value ctx = { ctx with vars = { id; value } :: ctx.vars }

  let change_var_in_ctx i value ctx =
    let new_vars =
      List.map (fun (x : var_ctx) -> if i = x.id then { x with value } else x) ctx.vars
    in
    { ctx with vars = new_vars }
  ;;

  let add_func_in_ctx id args stmts ctx =
    { ctx with funcs = { id; args; stmts } :: ctx.funcs }
  ;;

  let get_var_ctx_from_ctx i ctx = List.find (fun (x : var_ctx) -> i = x.id) ctx.vars
  let get_func_ctx_from_ctx i ctx = List.find (fun (x : func_ctx) -> i = x.id) ctx.funcs

  let get_class_ctx_from_ctx i ctx =
    List.find (fun (x : class_ctx) -> i = x.id) ctx.classes
  ;;

  let get_func_ctx_from_class_ctx i (ctx : class_ctx) =
    match if_func_exists_in_class_ctx i ctx with
    | true -> Some (List.find (fun (x : func_ctx) -> i = x.id) ctx.funcs)
    | false -> None
  ;;

  let multliple_str_unpacker = function
    | Int x -> Int.to_string x
    | Float x -> Float.to_string x
    | Str x -> x
    | Bool x -> Bool.to_string x
    | Null | ListExpr _ | Object _ | Lambda _ -> ""
  ;;

  let unpack_identifier_in_value = function
    | Object x -> return x
    | _ -> fail "object was expected"
  ;;

  let unpack_string_in_identifier = function
    | Id x -> return x
  ;;

  let unpack_int_in_value = function
    | Int i -> return i
    | _ -> fail "not a integer"
  ;;

  let unpack_list_in_value = function
    | ListExpr l -> return l
    | _ -> fail "not a List"
  ;;

  let if_list i ctx =
    match (get_var_ctx_from_ctx i ctx).value with
    | ListExpr _ -> true
    | _ -> false
  ;;

  let combine_args_and_params args params =
    List.map (fun x -> { id = fst x; value = snd x }) (List.combine args params)
  ;;

  let get_identifiers_from_args =
    List.map (function
      | Var (LocalVar, i) -> return i
      | _ -> fail "Local variables only")
  ;;

  let update_or_add_var ctx (x : var_ctx) =
    match if_var_exists_in_ctx x.id ctx with
    | true -> change_var_in_ctx x.id x.value ctx
    | false -> { ctx with vars = x :: ctx.vars }
  ;;

  let update_or_add_var_list = List.fold_left update_or_add_var
  let form_var_ctx id value = { id; value }

  let rec mmap f = function
    | [] -> return []
    | h :: tl ->
      let* c = f h in
      let* lst = mmap f tl in
      return (c :: lst)
  ;;

  let rec fold_left f init = function
    | [] -> return init
    | hd :: tl ->
      let* init = f init hd in
      fold_left f init tl
  ;;

  let rec expr stmt (e_env : global_ctx) ex =
    let rec doer1 c s =
      if c.signal = Return
      then return c.last_return
      else (
        match s with
        | [] -> return Null
        | hd :: tl ->
          let* x = stmt c hd in
          doer1 x tl)
    in
    let rec doer2 c s =
      if c.signal = Return
      then return c.last_return
      else (
        match s with
        | [] -> return Null
        | hd :: tl ->
          (match hd with
           | Expr e ->
             let* last_return = expr stmt c e in
             expr stmt { c with signal = Return; last_return } e
           | _ ->
             let* x = stmt c hd in
             doer2 x tl))
    in
    match ex with
    | Const x -> return x
    | Plus (l, r) ->
      let* l = expr stmt e_env l in
      let* r = expr stmt e_env r in
      (match l, r with
       | Int l1, Int r1 -> return @@ Int (l1 + r1)
       | Float l1, Float r1 -> return @@ Float (l1 +. r1)
       | Float l1, Int r1 -> return @@ Float (l1 +. Int.to_float r1)
       | Int l1, Float r1 -> return @@ Float (Int.to_float l1 +. r1)
       | _ -> fail "L or R has unsupported type")
    | Minus (l, r) ->
      let* l = expr stmt e_env l in
      let* r = expr stmt e_env r in
      (match l, r with
       | Int l1, Int r1 -> return @@ Int (l1 - r1)
       | Float l1, Float r1 -> return @@ Float (l1 -. r1)
       | Float l1, Int r1 -> return @@ Float (l1 -. Int.to_float r1)
       | Int l1, Float r1 -> return @@ Float (Int.to_float l1 -. r1)
       | _ -> fail "L or R has unsupported type")
    | Mult (l, r) ->
      let* l = expr stmt e_env l in
      let* r = expr stmt e_env r in
      (match l, r with
       | Int l1, Int r1 -> return @@ Int (l1 * r1)
       | Float l1, Float r1 -> return @@ Float (l1 *. r1)
       | Float l1, Int r1 -> return @@ Float (l1 *. Int.to_float r1)
       | Int l1, Float r1 -> return @@ Float (Int.to_float l1 *. r1)
       | _ -> fail "L or R has unsupported type")
    | Div (l, r) ->
      let* l = expr stmt e_env l in
      let* r = expr stmt e_env r in
      (match l, r with
       | Int l1, Int r1 -> return @@ Int (l1 / r1)
       | Float l1, Float r1 -> return @@ Float (l1 /. r1)
       | Float l1, Int r1 -> return @@ Float (l1 /. Int.to_float r1)
       | Int l1, Float r1 -> return @@ Float (Int.to_float l1 /. r1)
       | _ -> fail "L or R has unsupported type")
    | ModOp (l, r) ->
      let* l = expr stmt e_env l in
      let* r = expr stmt e_env r in
      (match l, r with
       | Int l1, Int r1 -> return @@ Int (l1 mod r1)
       | _ -> fail "L or R has unsupported type")
    | Equal (l, r) ->
      let* l = expr stmt e_env l in
      let* r = expr stmt e_env r in
      return (Bool (l = r))
    | NotEqual (l, r) ->
      let* l = expr stmt e_env l in
      let* r = expr stmt e_env r in
      return (Bool (l != r))
    | Less (l, r) ->
      let* l = expr stmt e_env l in
      let* r = expr stmt e_env r in
      return (Bool (l < r))
    | LessOrEqual (l, r) ->
      let* l = expr stmt e_env l in
      let* r = expr stmt e_env r in
      return (Bool (l <= r))
    | Greater (l, r) ->
      let* l = expr stmt e_env l in
      let* r = expr stmt e_env r in
      return (Bool (l > r))
    | GreaterOrEqual (l, r) ->
      let* l = expr stmt e_env l in
      let* r = expr stmt e_env r in
      return (Bool (l >= r))
    | AndOp (l, r) ->
      let* l = expr stmt e_env l in
      let* r = expr stmt e_env r in
      return (Bool (l = r))
    | OrOp (l, r) ->
      let* l = expr stmt e_env l in
      let* r = expr stmt e_env r in
      return (Bool (l = Bool true || r = Bool true))
    | Var (_, i) ->
      (match if_var_exists_in_ctx i e_env with
       | true -> return @@ (get_var_ctx_from_ctx i e_env).value
       | false -> fail "undefined Var")
    | ListAccess (i, e) ->
      (match if_var_exists_in_ctx i e_env with
       | false -> fail "undefined Var"
       | true ->
         (match if_list i e_env with
          | false -> fail "not a List"
          | true ->
            let l = unpack_list_in_value (get_var_ctx_from_ctx i e_env).value in
            let* x = expr stmt e_env e in
            let* index = unpack_int_in_value x in
            let* l = l in
            let* x = expr stmt e_env (List.nth l index) in
            return x))
    | FuncMonoCall (i, e) ->
      (match if_func_exists_in_ctx i e_env with
       | false -> fail "undefined Function"
       | true ->
         let* x = mmap (fun x -> expr stmt e_env x) e in
         doer1
           { (update_or_add_var_list
                e_env
                (combine_args_and_params (get_func_ctx_from_ctx i e_env).args x))
             with
             id = Id "tmp"
           ; sub_ctx = [ { tmp_ctx with vars = e_env.vars } ]
           ; funcs = [ get_func_ctx_from_ctx i e_env ]
           }
           (get_func_ctx_from_ctx i e_env).stmts)
    | FuncPolyCall (i1, i2, e) ->
      (match if_var_exists_in_ctx i1 e_env with
       | false -> fail "undefined Var"
       | true ->
         (match i2 with
          | Id "call" ->
            (match (get_var_ctx_from_ctx i1 e_env).value with
             | Lambda (il, sl) ->
               let* x = mmap (fun x -> expr stmt e_env x) e in
               doer2
                 (update_or_add_var_list
                    (update_or_add_var_list e_env (combine_args_and_params il x))
                    (List.hd e_env.sub_ctx).vars)
                 sl
             | _ -> fail "lambda was expected")
          | Id "to_s" ->
            return @@ Str (multliple_str_unpacker (get_var_ctx_from_ctx i1 e_env).value)
          | _ ->
            let obj_class =
              unpack_identifier_in_value (get_var_ctx_from_ctx i1 e_env).value
            in
            let* x = obj_class in
            let obj_class_ctx = get_class_ctx_from_ctx x e_env in
            (match if_class_exists_in_ctx x e_env with
             | false -> fail "undefined Class"
             | true ->
               (match get_func_ctx_from_class_ctx i2 obj_class_ctx with
                | Some func ->
                  let* x = mmap (fun x -> expr stmt e_env x) e in
                  doer1
                    { tmp_ctx with
                      vars = obj_class_ctx.vars @ combine_args_and_params func.args x
                    ; funcs = obj_class_ctx.funcs
                    }
                    func.stmts
                | None ->
                  (match
                     get_func_ctx_from_class_ctx (Id "method_missing") obj_class_ctx
                   with
                   | None -> fail "undefined Class Function"
                   | Some mm_func ->
                     let* x = unpack_string_in_identifier i2 in
                     doer1
                       { tmp_ctx with
                         vars =
                           obj_class_ctx.vars
                           @ combine_args_and_params [ List.hd mm_func.args ] [ Str x ]
                           @ combine_args_and_params
                               [ List.hd (List.rev (List.tl mm_func.args)) ]
                               [ ListExpr e ]
                       ; funcs = obj_class_ctx.funcs
                       }
                       mm_func.stmts)))))
    | CallLambda (e1, s1, e2) ->
      let* x = mmap (fun x -> expr stmt e_env x) e2 in
      doer2 (update_or_add_var_list e_env (combine_args_and_params e1 x)) s1
  ;;

  let rec stmt (s_env : global_ctx) = function
    | Break -> return { s_env with signal = Break }
    | Continue -> return { s_env with signal = Next }
    | Expr _ -> return s_env
    | Return e ->
      let* rtrn = expr stmt s_env e in
      return { s_env with signal = Return; last_return = rtrn }
    | Assign (l, r) ->
      (match l with
       | Var (LocalVar, id) ->
         let* value = expr stmt s_env r in
         let updated_env = update_or_add_var s_env { id; value } in
         return updated_env
       | _ -> fail "L has unsupported type")
    | MultiAssign (ll, rl) ->
      let id_list =
        mmap
          (function
            | Var (LocalVar, i) -> return i
            | _ -> fail "L has unsupported type")
          ll
      in
      let val_list = mmap (fun x -> expr stmt s_env x) rl in
      let* id_list = id_list in
      let* val_list = val_list in
      let var_list = List.map2 form_var_ctx id_list val_list in
      return @@ update_or_add_var_list s_env var_list
    | IfElse (e, s1, s2) ->
      let fl = fold_left (fun x -> stmt x) s_env in
      let* result = expr stmt s_env e in
      (match result with
       | Bool true -> fl s1
       | Bool false -> fl s2
       | _ -> fail "condition was expected")
    | Puts x ->
      let* x = expr stmt s_env x in
      return @@ add_output s_env x
    | While (e, s) ->
      let rec checker ctx =
        let* result = expr stmt ctx e in
        match result with
        | Bool false -> return ctx
        | _ -> doer ctx s
      and doer ctx1 s =
        match ctx1.signal with
        | Break -> return { ctx1 with signal = Work }
        | Next -> checker { ctx1 with signal = Work }
        | Return -> return ctx1
        | _ ->
          (match s with
           | [] -> checker ctx1
           | hd :: tl ->
             let* x = stmt ctx1 hd in
             doer x tl)
      in
      checker s_env
    | Func (i, a, s) -> return @@ add_func_in_ctx i a s s_env
    | Class (id, s) ->
      let outer_ctx = fold_left (fun x -> stmt x) tmp_ctx s in
      let cl_ctx =
        let* outer_ctx = outer_ctx in
        return { tmp_class_ctx with vars = outer_ctx.vars; funcs = outer_ctx.funcs }
      in
      (match if_class_exists_in_ctx id s_env with
       | true -> return s_env
       | false ->
         let* cl_ctx = cl_ctx in
         return { s_env with classes = { cl_ctx with id } :: s_env.classes })

  and add_output ctx = function
    | Int x -> { ctx with output = Int.to_string x :: ctx.output }
    | Float x -> { ctx with output = Float.to_string x :: ctx.output }
    | Str x -> { ctx with output = x :: ctx.output }
    | Bool x -> { ctx with output = Bool.to_string x :: ctx.output }
    | Null -> { ctx with output = "" :: ctx.output }
    | Object (Id x) -> { ctx with output = ("obj:" ^ x) :: ctx.output }
    | Lambda _ | ListExpr _ -> ctx
  ;;

  let init_main_ctx = fold_left stmt main_ctx
end

open Eval (Result)

let run_ruby str =
  let x = init_main_ctx @@ Parser.parser_result_to_stmt_list str in
  let print_output = List.iter (Printf.printf "%s\n") in
  match x with
  | Ok res -> print_output (List.rev res.output)
  | Error _ -> Format.printf "interpreter error"
;;
