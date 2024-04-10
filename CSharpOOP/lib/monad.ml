(** Copyright 2023-2024, Julia Kononova *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type typecheck_error =
  | Occurs_check
  | Mismatch
  | Access_error
  | Impossible_result of string
  | Other of string
[@@deriving show { with_path = false }]

type interpreter_error =
  | No_variable of string
  | Address_not_found of int
  | Var_declared of string
  | Mismatch
  | Impossible_result of string
  | Other of string
[@@deriving show { with_path = false }]

type error =
  | Typecheck_error of typecheck_error
  | Interpret_error of interpreter_error
[@@deriving show { with_path = false }]

module Monad_SE = struct
  type ('st, 'a) t = 'st -> 'st * ('a, error) Result.t

  let return : 'a -> ('st, 'a) t = fun x st -> st, Result.Ok x
  let fail : 'a -> ('st, 'b) t = fun er st -> st, Result.Error er

  let ( >>= ) : ('st, 'a) t -> ('a -> ('st, 'b) t) -> ('st, 'b) t =
    fun x f st ->
    let st, x = x st in
    match x with
    | Result.Ok x -> f x st
    | Result.Error er -> fail er st
  ;;

  let ( >>| ) : ('st, 'a) t -> ('a -> 'b) -> ('st, 'b) t =
    fun x f st ->
    let st, x = x st in
    match x with
    | Result.Ok x -> return (f x) st
    | Result.Error er -> fail er st
  ;;

  let ( <|> ) : ('st, 'a) t -> ('st, 'a) t -> ('st, 'a) t =
    fun x1 x2 st ->
    let st, x = x1 st in
    match x with
    | Result.Ok x -> return x st
    | Result.Error _ -> x2 st
  ;;

  let ( *> ) : ('st, 'a) t -> ('st, 'b) t -> ('st, 'b) t = fun x1 x2 -> x1 >>= fun _ -> x2

  let ( <* ) : ('st, 'a) t -> ('st, 'b) t -> ('st, 'a) t =
    fun x1 x2 -> x1 >>= fun x -> x2 >>| fun _ -> x
  ;;

  let lift2 : ('a -> 'b -> 'c) -> ('st, 'a) t -> ('st, 'b) t -> ('st, 'c) t =
    fun f a b -> a >>= fun r_a -> b >>= fun r_b -> return @@ f r_a r_b
  ;;

  let lift3
    : ('a -> 'b -> 'c -> 'd) -> ('st, 'a) t -> ('st, 'b) t -> ('st, 'c) t -> ('st, 'd) t
    =
    fun f a b c -> lift2 f a b >>= fun f -> c >>| f
  ;;

  let lift4
    :  ('a -> 'b -> 'c -> 'd -> 'e) -> ('st, 'a) t -> ('st, 'b) t -> ('st, 'c) t
    -> ('st, 'd) t -> ('st, 'e) t
    =
    fun f a b c d -> lift3 f a b c >>= fun f -> d >>| f
  ;;

  let read : ('st, 'st) t = fun st -> return st st
  let write : 'st -> ('st, unit) t = fun new_st _ -> new_st, Result.Ok ()
  let run : ('st, 'a) t -> 'st -> 'st * ('a, error) Result.t = fun f st -> f st

  let map : ('a -> ('st, 'b) t) -> 'a list -> ('st, 'b list) t =
    fun f list ->
    let f acc el = acc >>= fun acc -> f el >>= fun el -> return (el :: acc) in
    List.fold_left f (return []) list >>| List.rev
  ;;

  let fold_left : ('a -> 'b -> ('st, 'a) t) -> 'a -> 'b list -> ('st, 'a) t =
    fun f acc l ->
    let f' acc a = acc >>= fun acc -> f acc a >>= return in
    List.fold_left f' (return acc) l
  ;;

  let iter : ('a -> ('st, unit) t) -> 'a list -> ('st, unit) t =
    fun f list ->
    let f acc el = acc *> f el *> return () in
    List.fold_left f (return ()) list
  ;;
end

module Monad_TypeCheck = struct
  open Ast
  open State_type.TypeCheck
  open State_type
  include Monad_SE

  type 'a t = (TypeCheck.st_type_check, 'a) Monad_SE.t

  let return_with_fail = function
    | Some x -> return x
    | None -> fail (Typecheck_error Occurs_check)
  ;;

  let read_local : 'a MapName.t t =
    read
    >>= function
    | _, l, _, _, _ -> return l
  ;;

  let read_local_el name f = read_local >>= fun l -> MapName.find_opt name l |> f
  let read_local_el_opt name = read_local_el name return
  let read_local_el name = read_local_el name return_with_fail

  let read_global : 'a MapName.t t =
    read
    >>= function
    | g, _, _, _, _ -> return g
  ;;

  let read_global_el name f = read_global >>= fun g -> MapName.find_opt name g |> f
  let read_global_el_opt name = read_global_el name return
  let read_global_el name = read_global_el name return_with_fail

  let get_cur_class_name : cur_class_name t =
    read
    >>= function
    | _, _, Some n, _, _ -> return n
    | _ ->
      fail
        (Typecheck_error
           (Impossible_result "Current class can be 'none' only before running"))
  ;;

  let read_meth_type : meth_type option t =
    read
    >>= function
    | _, _, _, m_t, _ -> return m_t
  ;;

  let read_main_class : class_with_main_method option t =
    read
    >>= function
    | _, _, _, _, main -> return main
  ;;

  let write_local n_l =
    read
    >>= function
    | g, _, n, m, main -> write (g, n_l, n, m, main)
  ;;

  let write_local_el el_name el_ctx =
    read_local >>= fun l -> write_local (MapName.add el_name el_ctx l)
  ;;

  let write_meth_type_opt m_t =
    read
    >>= function
    | g, l, n, _, main -> write (g, l, n, m_t, main)
  ;;

  let write_meth_type m_t = write_meth_type_opt (Some m_t)

  let write_main_class main =
    read
    >>= function
    | g, l, n, m_t, _ -> write (g, l, n, m_t, main)
  ;;

  let write_global n_g =
    read
    >>= function
    | _, l, n, m, main -> write (n_g, l, n, m, main)
  ;;

  let write_global_el el_name el_ctx =
    read_global >>= fun g -> write_global (MapName.add el_name el_ctx g)
  ;;

  let write_cur_class_name n =
    read
    >>= function
    | g, l, _, t, main -> write (g, l, Some n, t, main)
  ;;
end

module Monad_Interpreter = struct
  open Ast
  open State_type
  open State_type.St_Interpreter

  type ('a, 'r) runtime_signal =
    | Pipe of 'a
    | Return of 'r

  type ('a, 'r, 'e) result =
    | Signal of ('a, 'r) runtime_signal
    | IError of 'e

  type st = State_type.St_Interpreter.st_interpreter
  type ('a, 'r) t = st -> st * ('a, 'r, error) result

  let return : 'a -> ('a, 'r) t = fun x st -> st, Signal (Pipe x)
  let fun_return : 'r -> ('a, 'r) t = fun x st -> st, Signal (Return x)
  let fail : 'e -> ('a, 'r) t = fun er st -> st, IError er

  let ( >>= ) : ('a, 'r) t -> ('a -> ('b, 'r) t) -> ('b, 'r) t =
    fun x f st ->
    let st, x = x st in
    match x with
    | Signal (Pipe x) -> f x st
    | Signal (Return r) -> fun_return r st
    | IError er -> fail er st
  ;;

  let ( |>>= ) x f st =
    let st, x = x st in
    match x with
    | Signal sign -> f sign st
    | IError er -> fail er st
  ;;

  let ( >>| ) : ('a, 'r) t -> ('a -> 'b) -> ('b, 'r) t =
    fun x f st ->
    let st, x = x st in
    match x with
    | Signal (Pipe x) -> return (f x) st
    | Signal (Return r) -> fun_return r st
    | IError er -> fail er st
  ;;

  let ( <|> ) : ('a, 'r) t -> ('a, 'r) t -> ('a, 'r) t =
    fun x1 x2 st ->
    let st, x = x1 st in
    match x with
    | Signal (Pipe x) -> return x st
    | IError _ -> x2 st
    | Signal (Return r) -> fun_return r st
  ;;

  let ( *> ) : ('a, 'r) t -> ('b, 'r) t -> ('b, 'r) t = fun x1 x2 -> x1 >>= fun _ -> x2

  let ( <* ) : ('a, 'r) t -> ('b, 'r) t -> ('a, 'r) t =
    fun x1 x2 -> x1 >>= fun x -> x2 >>| fun _ -> x
  ;;

  let read : (st, 'r) t = fun st -> return st st
  let write : st -> (unit, 'r) t = fun new_st _ -> new_st, Signal (Pipe ())

  let run : ('a, 'r) t -> st * ('a, 'r, error) result =
    fun f -> f (MapName.empty, (Idx 0, MapName.empty), Adr 0, (Adr 0, MapAdr.empty))
  ;;

  let lift2 f a b = a >>= fun r_a -> b >>= fun r_b -> return @@ f r_a r_b
  let lift3 f a b c = lift2 f a b >>= fun f -> c >>| f
  let lift4 f a b c d = lift3 f a b c >>= fun f -> d >>| f

  let map f list =
    let f' acc el = acc >>= fun acc -> f el >>= fun el -> return (el :: acc) in
    List.fold_left f' (return []) list >>| List.rev
  ;;

  let iter f list =
    let f' acc el = acc *> f el *> return () in
    List.fold_left f' (return ()) list
  ;;

  let fold_left f acc l =
    let f' acc a = acc >>= fun acc -> f acc a >>= return in
    List.fold_left f' (return acc) l
  ;;

  let iter2 f l1 l2 =
    let f' acc el1 el2 = acc *> f el1 el2 *> return () in
    List.fold_left2 f' (return ()) l1 l2
  ;;

  (* State read/write operations *)
  let pipe_name_with_fail (Name n) = function
    | Some x -> return x
    | None -> fail (Interpret_error (No_variable n))
  ;;

  let pipe_adr_with_fail (Adr a) = function
    | Some x -> return x
    | None -> fail (Interpret_error (Address_not_found a))
  ;;

  let read_local =
    read
    >>= function
    | _, l, _, _ -> return l
  ;;

  let read_local_el f name = read_local >>= fun (_, l) -> MapName.find_opt name l |> f
  let read_local_el_opt name = read_local_el return name
  let read_local_el name = read_local_el (pipe_name_with_fail name) name
  let read_idx = read_local >>= fun (idx, _) -> return idx

  let read_global =
    read
    >>= function
    | g, _, _, _ -> return g
  ;;

  let read_global_el name =
    read_global >>= fun g -> MapName.find_opt name g |> pipe_name_with_fail name
  ;;

  let read_local_adr =
    read
    >>= function
    | _, _, adr, _ -> return adr
  ;;

  let read_memory =
    read
    >>= function
    | _, _, _, m -> return m
  ;;

  let read_cur_adr = read_memory >>= fun (a, _) -> return a

  let read_memory_obj adr =
    read_memory >>= fun (_, m) -> MapAdr.find_opt adr m |> pipe_adr_with_fail adr
  ;;

  let write_local n_l =
    read
    >>= function
    | g, _, adr, m -> write (g, n_l, adr, m)
  ;;

  let write_idx new_idx =
    read_local >>= fun (_, l) -> write_local (new_idx, l) *> return new_idx
  ;;

  let write_local_el el_name el_ctx =
    read_local >>= fun (idx, l) -> write_local (idx, MapName.add el_name el_ctx l)
  ;;

  let write_new_local_el (Name el_name) el_ctx =
    read_local_el_opt (Name el_name)
    >>= function
    | Some _ -> fail (Interpret_error (Var_declared el_name))
    | None -> write_local_el (Name el_name) el_ctx
  ;;

  let write_local_adr adr =
    read
    >>= function
    | g, l, _, m -> write (g, l, adr, m)
  ;;

  let write_global n_g =
    read
    >>= function
    | _, l, adr, m -> write (n_g, l, adr, m)
  ;;

  let write_global_el el_name el_ctx =
    read_global >>= fun g -> write_global (MapName.add el_name el_ctx g)
  ;;

  let write_memory n_m =
    read
    >>= function
    | g, l, adr, _ -> write (g, l, adr, n_m)
  ;;

  let write_cur_adr new_adr =
    read_memory >>= fun (_, m) -> write_memory (new_adr, m) *> return new_adr
  ;;

  let write_memory_obj obj_adr obj_ctx =
    read_memory >>= fun (adr, m) -> write_memory (adr, MapAdr.add obj_adr obj_ctx m)
  ;;

  let find_local_el name =
    let rec find_memory_obj adr =
      read_memory_obj adr
      >>= fun obj ->
      MapName.find_opt name obj.mems
      |> function
      | Some (_, vl) -> return (Value (vl, None))
      | None ->
        (match obj.p_adr with
         | Some p_adr -> find_memory_obj p_adr
         | None -> fail (Interpret_error Mismatch))
    in
    let find_global_el adr =
      let f acc = function
        | CMethod (m, b) when equal_name m.m_name name ->
          return (Some (Code (Method (m, b))))
        | _ -> return acc
      in
      read_memory_obj adr
      >>= fun obj ->
      read_global_el obj.cl_name
      >>= function
      | Int_Class cl ->
        fold_left f None cl.cl_body
        >>= (function
         | Some vl -> return vl
         | None ->
           (match name with
            | Name n -> fail (Interpret_error (No_variable n))))
      | Int_Interface _ ->
        (match name with
         | Name n -> fail (Interpret_error (No_variable n)))
    in
    read_local_el name
    <|> (read_local_adr >>= fun adr -> find_memory_obj adr <|> find_global_el adr)
  ;;

  let get_new_idx = read_idx >>= fun (Idx i) -> write_idx (Idx (i + 1))
  let get_new_adr = read_cur_adr >>= fun (Adr i) -> write_cur_adr (Adr (i + 1))
  let fake_adr = Adr (-1) (* Memory access on the step leads to an error *)

  let downgrade_local l =
    let f k v acc =
      match v with
      | Code (Constructor (_, _)) -> MapName.add k v acc
      | _ -> acc
    in
    get_new_idx >>= fun idx -> write_local (idx, MapName.fold f l MapName.empty)
  ;;

  let new_local adr =
    read_local
    >>= fun (idx, old_l) ->
    downgrade_local old_l *> read_cur_adr
    >>= fun old_adr -> write_local_adr adr *> return (idx, old_l, old_adr)
  ;;

  let reset adr idx l = write_cur_adr adr *> write_local (idx, l)

  let create_obj constr i_expr =
    let get_fields n =
      let foo acc = function
        | CField (f, e) ->
          (match e with
           | Some e ->
             new_local fake_adr
             >>= fun (idx, old_l, old_adr) ->
             i_expr e
             >>= (function
              | Value (vl, _) ->
                reset old_adr idx old_l *> return (MapName.add f.f_name (f, vl) acc)
              | _ -> reset old_adr idx old_l *> fail (Interpret_error Mismatch))
           | None -> return (MapName.add f.f_name (f, Not_init) acc))
        | _ -> return acc
      in
      read_global_el n
      >>= function
      | Int_Class cl -> fold_left foo MapName.empty cl.cl_body
      | _ -> fail (Interpret_error (Impossible_result "Check during typecheck"))
    in
    let create_obj_ constr inh_adr =
      get_new_adr
      >>= fun adr ->
      get_fields constr.c_name
      >>= fun fields ->
      write_memory_obj
        adr
        { mems = fields; cl_name = constr.c_name; p_adr = None; inh_adr }
      *> return adr
    in
    let update_p_adr my_adr p_adr =
      read_memory_obj my_adr >>= fun obj -> write_memory_obj my_adr { obj with p_adr }
    in
    let rec create_p_obj constr adr =
      read_global_el constr.c_name
      >>= function
      | Int_Interface _ -> return None
      | Int_Class cl ->
        (match cl.cl_parent with
         | Some n ->
           find_local_el n
           >>= (function
            | Code (Constructor (sign, _)) ->
              create_obj_ sign adr
              >>= fun my_adr ->
              create_p_obj sign adr
              >>= fun p_adr -> update_p_adr my_adr p_adr *> return (Some my_adr)
            | _ -> fail (Interpret_error (Other "Can't find constructor")))
         | None -> return None)
    in
    create_obj_ constr None
    >>= fun my_adr ->
    create_p_obj constr (Some my_adr)
    >>= fun p_adr -> update_p_adr my_adr p_adr *> return my_adr
  ;;

  let write_args_to_local args (Params params) =
    let f ctx (Var_Declaration (_, n)) = write_new_local_el n ctx in
    iter2 f args params
  ;;

  let run_method args params adr ret_tp body_f =
    new_local adr
    >>= fun (idx, old_l, old_adr) ->
    write_args_to_local args params *> body_f
    |>>= fun sign ->
    match sign, ret_tp with
    | Pipe _, TVoid -> reset old_adr idx old_l *> return None
    | Return x, TReturn _ -> reset old_adr idx old_l *> return x
    | _ ->
      reset old_adr idx old_l
      *> fail (Interpret_error (Other "Error in the function return value"))
  ;;

  let run_constructor args i_expr i_statement adr =
    let rec eval args adr =
      let get_args args = map i_expr args in
      read_memory_obj adr
      >>= fun obj ->
      find_local_el obj.cl_name
      >>= function
      | Code (Constructor (c, b)) ->
        (match obj.p_adr with
         | Some p_adr ->
           (match c.c_base with
            | Some (Args p_args) ->
              new_local fake_adr
              >>= fun (idx, old_l, old_adr) ->
              write_args_to_local args c.c_params *> get_args p_args
              >>= fun p_args -> reset old_adr idx old_l *> eval p_args p_adr *> return ()
            | None -> fail (Interpret_error (Impossible_result "Check during typecheck")))
         | None -> return ())
        *> run_method args c.c_params adr TVoid (i_statement b)
      | _ -> fail (Interpret_error (Impossible_result "EEECheck during typecheck"))
    in
    eval args adr *> return adr
  ;;

  let allocate_object constr args i_expr i_statement =
    create_obj constr i_expr >>= run_constructor args i_expr i_statement
  ;;
end
