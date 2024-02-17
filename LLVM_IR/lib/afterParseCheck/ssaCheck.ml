(** Copyright 2023-2024, Efremov Alexey *)

(** SPDX-License-Identifier: CC0-1.0 *)

include Common.StateMonad

module MapString = struct
  include Map.Make (String)

  let pp pp_v ppf m =
    Format.fprintf ppf "@[[@[";
    iter (fun k v -> Format.fprintf ppf "@[\"%S\": %a@],@\n" k pp_v v) m;
    Format.fprintf ppf "@]]@]"
  ;;
end

type map_var = bool MapString.t [@@deriving show { with_path = false }]
type state = map_var * map_var

let check_var : Ast.variable -> (state, unit) t =
  let find_var : string -> map_var -> (state, unit) t =
    fun name map ->
    let value = MapString.find_opt name map in
    match value with
    | Some _ -> fail (Printf.sprintf "Variable %s already was assigned" name)
    | None -> return ()
  in
  fun variable ->
    let* local, glob = read in
    match variable with
    | Ast.GlobalVar name -> find_var name glob
    | Ast.LocalVar name -> find_var name local
;;

let write_var : Ast.variable -> state -> (state, unit) t =
  fun key (old_local, old_global) ->
  match key with
  | Ast.GlobalVar name -> write (old_local, MapString.add name true old_global)
  | Ast.LocalVar name -> write (MapString.add name true old_local, old_global)
;;

let try_add_var : Ast.variable -> (state, unit) t =
  fun var ->
  check_var var
  *>
  let* st = read in
  write_var var st
;;

let ssa_instruction : Ast.instruction -> (state, unit) t =
  let terminator = function
    | _ -> return ()
  and unary = function
    | Ast.Fneg (var, _, _) -> try_add_var var
  and binary = function
    | Ast.Add body
    | Ast.Fadd body
    | Ast.Mul body
    | Ast.Fmul body
    | Ast.Sub body
    | Ast.Fsub body
    | Ast.Udiv body
    | Ast.Sdiv body
    | Ast.Fdiv body
    | Ast.Urem body
    | Ast.Srem body
    | Ast.Frem body ->
      let var, _, _, _ = body in
      try_add_var var
  and bitwise = function
    | Ast.Shl body
    | Ast.Lshr body
    | Ast.Ashr body
    | Ast.And body
    | Ast.Or body
    | Ast.Xor body ->
      let var, _, _, _ = body in
      try_add_var var
  and vector = function
    | Ast.Extractelement (var, _, _, _, _)
    | Ast.Insertelement (var, _, _, _, _, _)
    | Ast.Shufflevector (var, _, _, _, _, _) -> try_add_var var
  and aggregate = function
    | Ast.Extractvalue (var, _, _, _) | Ast.Insertvalue (var, _, _, _, _, _) ->
      try_add_var var
  and memory = function
    | Ast.Alloca (var, _, _, _)
    | Ast.Load (var, _, _, _)
    | Ast.Getelementptr (var, _, _, _, _) -> try_add_var var
    | Ast.Store _ -> return ()
  and conversion = function
    | Ast.TruncTo body
    | Ast.ZextTo body
    | Ast.SextTo body
    | Ast.FptouiTo body
    | Ast.FptosiTo body
    | Ast.UitofpTo body
    | Ast.SitofpTo body
    | Ast.PrttointTo body
    | Ast.InttoprtTo body ->
      let var, _, _, _ = body in
      try_add_var var
  and other = function
    | Ast.Icmp (var, _, _, _, _)
    | Ast.Fcmp (var, _, _, _, _)
    | Ast.Phi (var, _, _)
    | Ast.Select (var, _, _, _, _, _)
    | Ast.Call (var, _, _, _) -> try_add_var var
  in
  function
  | Terminator inst -> terminator inst
  | Unary inst -> unary inst
  | Binary inst -> binary inst
  | BitwiseBinary inst -> bitwise inst
  | Vector inst -> vector inst
  | Aggregate inst -> aggregate inst
  | MemoryAddress inst -> memory inst
  | Conversion inst -> conversion inst
  | Other inst -> other inst
;;

let ssa_block_var : Ast.variable * Ast.const -> (state, unit) t =
  fun (var, block) ->
  try_add_var var
  *>
  match block with
  | Ast.CLabel inst_lst -> map_list ssa_instruction inst_lst *> return ()
  | _ -> fail "Impossible error: get not block in block const"
;;

let ssa_fnc : Ast.func -> (state, unit) t =
  fun fnc ->
  let* old_st = read in
  let _old_loc, old_glob = old_st in
  let new_st = MapString.empty, old_glob in
  let* _ = write new_st in
  let* _check_args = map_list try_add_var fnc.parameters in
  let* _check_blocks = map_list ssa_block_var fnc.basic_blocks in
  write old_st *> return ()
;;

let ssa_glob_list : Ast.glob_list -> (state, unit) t =
  fun glob_lst ->
  let ssa_glob_elem (_, var, value, _) =
    try_add_var var
    *>
    match value with
    | Ast.CFunc fnc -> ssa_fnc fnc
    | _ -> return ()
  in
  map_list ssa_glob_elem glob_lst *> return ()
;;

let run_ssa_glob_list glob_lst =
  let _, res = run (ssa_glob_list glob_lst) (MapString.empty, MapString.empty) in
  res
;;
