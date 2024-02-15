(** Copyright 2023-2024, Efremov Alexey *)

(** SPDX-License-Identifier: CC0-1.0 *)

open Ihelp
open State
open Instructions
open CommonInterpInstructions

let assign_globs : Ast.glob_list -> (state, unit) t =
  fun glob_lst ->
  let count_addr addr glob =
    let tp, _, _, align = glob in
    match tp with
    | Ast.TFunc (_, _) -> addr, addr
    | _ ->
      let na = Memory.align_addr addr align true in
      let res = na + Serialisation.raw_date_len tp in
      res, na
  in
  let _, addrs = List.fold_left_map count_addr glob_sect glob_lst in
  let addrs_and_globs = List.combine addrs glob_lst in
  let assign_glob : int * (Ast.tp * Ast.variable * Ast.const * int) -> (state, unit) t =
    fun (addr, glob) ->
    let tp, var, value, _ = glob in
    let cnst =
      match tp with
      | Ast.TFunc (_, _) -> value
      | _ -> Ast.CPointer (Ast.PointerInt addr)
    in
    write_var var cnst
  in
  map_list assign_glob addrs_and_globs *> return ()
;;

let allocate_globs : Ast.glob_list -> (state, unit) t =
  fun glob_lst ->
  let alloc_glob : Ast.tp * Ast.variable * Ast.const * int -> (state, unit) t =
    fun (_, var, cnst, _) ->
    let* ptr_cnst = read_var var in
    match ptr_cnst with
    | Ast.CPointer (Ast.PointerInt x) -> Memory.put_cnst_in_heap x cnst
    | _ -> return ()
  in
  map_list alloc_glob glob_lst *> return ()
;;

let init_state : Ast.glob_list -> (state, unit) t =
  fun glob_lst -> assign_globs glob_lst *> allocate_globs glob_lst
;;

let is_fnc_tp = function
  | Ast.TFunc (f_ret, f_args) -> return (f_ret, f_args)
  | _ -> fail "Impossible error: function have not function type"
;;

let check_fnc_tp (fnc : Ast.func) ret_tp args =
  let arg_tps = List.map Ast.const_to_tp args in
  let* bf = is_fnc_tp fnc.ftp in
  let f_ret, f_args = bf in
  if not (Ast.tp_equal f_ret ret_tp)
  then
    fail
      (Printf.sprintf
         "Try get %s from function with %s return"
         (Ast.show_tp ret_tp)
         (Ast.show_tp f_ret))
  else if not (List.equal Ast.tp_equal f_args arg_tps)
  then fail "Expected other function args"
  else return ()
;;

let rec icall var res_tp fnc_val params =
  let* cfnc = get_const_from_value fnc_val in
  let* args = map_list get_const_from_value params in
  match cfnc with
  | Ast.CPointer (Ast.PointerGlob x) ->
    let* cns = read_var x in
    (match cns with
     | Ast.CFunc fnc ->
       let* _ = check_fnc_tp fnc res_tp args in
       let* res = launch_function fnc args in
       write_var var res
     | c -> fail (Printf.sprintf "Expect function got %s" (Ast.show_const c)))
  | c -> fail (Printf.sprintf "Expect pointer to function got %s" (Ast.show_const c))

and launch_other_operation : Ast.other_operation -> (state, instr_launch_res) t =
  fun instr ->
  (match instr with
   | Ast.Icmp (var, com_mode, tp, v1, v2) -> Other.iicmp var com_mode tp v1 v2
   | Ast.Fcmp (var, com_mode, tp, v1, v2) -> Other.ifcmp var com_mode tp v1 v2
   | Ast.Phi (var, tp, lst) -> Other.iphi var tp lst
   | Ast.Select (var, cond_tp, cond, res_tp, v1, v2) ->
     Other.iselect var cond_tp cond res_tp v1 v2
   | Ast.Call (var, res_tp, fnc, params) -> icall var res_tp fnc params)
  *> return NoRes

and launch_instruction : Ast.instruction -> (state, instr_launch_res) t = function
  | Ast.Terminator inst -> Terminator.launch_terminator_instruction inst
  | Ast.Unary inst -> Unary.launch_unary_operation inst
  | Ast.Binary inst -> Binary.launch_binary_operation inst
  | Ast.BitwiseBinary inst -> Bitwise.launch_bitwise_operation inst
  | Ast.Vector inst -> Vector.launch_vector_instruction inst
  | Ast.Aggregate inst -> Aggregate.launch_aggregate_instruction inst
  | Ast.MemoryAddress inst -> MemoryAddress.launch_memory_address_operation inst
  | Ast.Conversion inst -> Conversion.launch_conversion_instruction inst
  | Ast.Other inst -> launch_other_operation inst

and launch_block : Ast.variable -> (state, Ast.const) t =
  fun bb_var ->
  (* let _ = Printf.printf "%s\n" (Ast.show_variable bb_var) in *)
  let* bb = read_var bb_var >>= is_block in
  let* instr_res = map_list launch_instruction bb in
  let last_instr = List.nth instr_res (List.length instr_res - 1) in
  match last_instr with
  | Jmp x -> write_last_block bb_var *> launch_block x
  | Ret x -> return x
  | NoRes -> fail "Impossible error: last instruction in block should have some result\n"

and launch_function : Ast.func -> Ast.const list -> (state, Ast.const) t =
  fun fnc params_val ->
  let* old_loc, old_glb, old_heap, old_stack, old_block = read in
  write (MapString.empty, old_glb, old_heap, old_stack, last_block_init)
  *>
  let init_var (param, cnst) = write_var param cnst in
  let params_cnst = List.combine fnc.parameters params_val in
  map_list init_var params_cnst
  *> map_list init_var fnc.basic_blocks
  *>
  let fb, _ = List.hd fnc.basic_blocks in
  let* res = launch_block fb in
  let* bf = is_fnc_tp fnc.ftp in
  let f_ret, _ = bf in
  let res_tp = Ast.const_to_tp res in
  (if Ast.tp_equal res_tp f_ret
   then return res
   else
     fail
       (Printf.sprintf
          "Function try return %s when excpected %s"
          (Ast.show_tp res_tp)
          (Ast.show_tp f_ret)))
  <* Memory.free_stack old_stack
  <* let* _, glb, heap, stack, _ = read in
     write (old_loc, glb, heap, stack, old_block)
;;

let init_sys_args int_sz =
  let argc = Common.IrInts.create (Int64.of_int (Array.length Sys.argv)) int_sz in
  let lst_of_bytes =
    Array.to_list
      (Array.map (fun s -> List.init (String.length s) (String.get s)) Sys.argv)
  in
  let f bytes =
    let bytes = List.append bytes [ Char.chr 0 ] in
    let len = List.length bytes in
    let* addr = Memory.alloc_stack_align len 1 in
    Memory.put_bytes_in_heap addr bytes *> return addr
  in
  let* rev_addr_lst = map_list f lst_of_bytes >>| List.rev in
  let f addr =
    let* ptr_addr =
      Memory.alloc_stack_align (Serialisation.raw_date_len Ast.TPointer) 1
    in
    Memory.put_cnst_in_heap ptr_addr (to_ptr addr) *> return ptr_addr
  in
  let* ptrs_addr = map_list f rev_addr_lst in
  let fin_addr = List.nth ptrs_addr (List.length ptrs_addr - 1) in
  let argv = to_ptr fin_addr in
  return [ argc; argv ]
;;

let launch_main =
  let* main = read_var (Ast.GlobalVar "main") in
  match main with
  (* TODO: add check for main arguments*)
  | Ast.CFunc x ->
    let* bf = is_fnc_tp x.ftp in
    let _, param = bf in
    (match param with
     | [] -> launch_function x []
     | [ Ast.TInteger sz; Ast.TPointer ] -> init_sys_args sz >>= launch_function x
     | _ -> fail "Can not run main with unknown signature")
  | _ -> fail "Error: main is not function\n"
;;

let interpretate_ast : Ast.glob_list -> (state, Ast.const) t =
  fun glb_lst -> init_state glb_lst *> launch_main
;;

let run_interpretate_on_ast ast = run (interpretate_ast ast) empty_state

let interp_test str =
  match Parser.Parsing.parse_program str with
  | Result.Ok x ->
    (match run_interpretate_on_ast x with
     | _, Result.Ok x -> Printf.printf "%s\n" (Ast.show_const x)
     | _, Result.Error s -> Printf.printf "Error: %s!\n" s)
  | _ -> Printf.printf "Parser error\n"
;;

let%expect_test _ =
  interp_test
    {|  
; Function Attrs: noinline nounwind optnone uwtable
define <3 x float> @main(){
  %1 = fneg <3 x float> < float 1.2,  float -3.4,  float 5.6>
  br  label %3
  3:
  ret <3 x float> %1
}
      |};
  [%expect {|
    (CVector [(CFloat -1.2); (CFloat 3.4); (CFloat -5.6)]) |}]
;;

let%expect_test _ =
  interp_test
    {|  
; Function Attrs: noinline nounwind optnone uwtable
define <3 x float> @main(){
  %1 = fneg <3 x float> < float 1.2,  float -3.4,  float 5.6>
  %3 = fadd <3 x float> %1, < float 10.0,  float 8.0,  float 7.0>
  ret <3 x float> %3
}
      |};
  [%expect {|
    (CVector [(CFloat 8.8); (CFloat 11.4); (CFloat 1.4)]) |}]
;;

let%expect_test _ =
  interp_test
    {|  
define i4 @main(){
  %a = add i4 8, 0
  %b = add i4 2, 0
  %c = sdiv i4 %a, %b
  ret i4 %c
}
      |};
  [%expect {| (CInteger (4, 12L)) |}]
;;

let%expect_test _ =
  interp_test
    {|  
define i4 @main(){
  %a = add i4 9, 0
  %b = add i4 2, 0
  %c = srem i4 %a, %b
  ret i4 %c
}
      |};
  [%expect {| (CInteger (4, 15L)) |}]
;;

let%expect_test _ =
  interp_test
    {|  
define i4 @main(){
  %a = add i4 9, 0
  %b = add i4 0, 0
  %c = srem i4 %a, %b
  ret i4 %c
}
      |};
  [%expect {| Error: Runtime error: Division by 0! |}]
;;

let%expect_test _ =
  interp_test
    {|  
define float @main(){
  %a = fadd float 0.0, 0.0
  %b = fadd float 2.0, 0.0
  %c = fdiv float %b, %a
  ret float %c
}
      |};
  [%expect {| (CFloat infinity) |}]
;;

let%expect_test _ =
  interp_test
    {|  
define <4 x i1> @main(){
  %a = xor <4 x i1> <i1 0, i1 0, i1 1, i1 1>, <i1 0, i1 1, i1 0, i1 1>
  ret <4 x i1> %a
}
      |};
  [%expect
    {|
      (CVector
         [(CInteger (1, 0L)); (CInteger (1, 1L)); (CInteger (1, 1L));
           (CInteger (1, 0L))]) |}]
;;

let%expect_test _ =
  interp_test
    {|  
define float @main(){
  %a = extractelement <3 x float> < float 1.2,  float -3.4,  float 5.6>, i32 1
  ret float %a
}
      |};
  [%expect {|
      (CFloat -3.4) |}]
;;

let%expect_test _ =
  interp_test
    {|  
define <3 x float> @main(){
  %a = insertelement <3 x float> < float 1.2,  float -3.4,  float 5.6>,  float 82.0, i32 1
  ret <3 x float> %a
}
      |};
  [%expect {|
      (CVector [(CFloat 1.2); (CFloat 82.); (CFloat 5.6)]) |}]
;;

let%expect_test _ =
  interp_test
    {|  
define <2 x float> @main(){
  %a = shufflevector
   <3 x float> < float 1.2,  float -3.4,  float 5.6>,
    <3 x float>  < float 10.0,  float 8.0,  float 7.0>,
     <2 x i32> <i32 1, i32 5>
  ret <2 x float> %a
}
      |};
  [%expect {|
      (CVector [(CFloat -3.4); (CFloat 7.)]) |}]
;;

let%expect_test _ =
  interp_test
    {|  
define i32 @main(){
  %a = extractvalue {[3 x i32], float} {[3 x i32][i32 21, i32 32, i32 43], float 50.0}, 0, 1
  ret i32 %a
}
      |};
  [%expect {|
      (CInteger (32, 32L)) |}]
;;

let%expect_test _ =
  interp_test
    {|  
define {[3 x i32], float} @main(){
  %a = insertvalue {[3 x i32], float} {[3 x i32][i32 21, i32 32, i32 43], float 50.0}, i32 4, 0, 1
  ret {[3 x i32], float} %a
}
      |};
  [%expect
    {|
      (CStruct
         [(CArr [(CInteger (32, 21L)); (CInteger (32, 4L)); (CInteger (32, 43L))]);
           (CFloat 50.)]) |}]
;;

let%expect_test _ =
  interp_test
    {|  
define {[3 x i32], float} @main(){
  %a = insertvalue {[3 x i32], float} {[3 x i32][i32 21, i32 32, i32 43], float 50.0}, i32 4, 1
  ret {[3 x i32], float} %a
}
      |};
  [%expect
    {|
      Error: Want to insert value with type (TInteger 32) instead of value with type TFloat! |}]
;;

let%expect_test _ =
  interp_test
    {|  
define {[3 x i32], float} @main(){
  %a = alloca {[3 x i32], float}, align 4
  store {[3 x i32], float} {[3 x i32][i32 21, i32 32, i32 43], float 50.}, ptr %a, align 4
  %b = load {[3 x i32], float}, ptr %a, align 4
  ret {[3 x i32], float} %b
}
      |};
  [%expect
    {|
      (CStruct
         [(CArr [(CInteger (32, 21L)); (CInteger (32, 32L)); (CInteger (32, 43L))]);
           (CFloat 50.)]) |}]
;;

let%expect_test _ =
  interp_test
    {|  
@dd = global i32 312312, align 4

define i32 @main(){
  %b = load i32, ptr @dd, align 4
  ret i32 %b
}
      |};
  [%expect {|
      (CInteger (32, 312312L)) |}]
;;

let%expect_test _ =
  interp_test
    {|  
@dd = global i32 312312, align 1000

define ptr @main(){
  %b = getelementptr { i32, [3 x  i32], i32 }, ptr @dd, i32 1, i32 0, i32 2
  ret ptr %b
}
      |};
  [%expect {|
      Error: Runtime error: invalid getelementptr indices! |}]
;;

let%expect_test _ =
  interp_test
    {|  
@dd = global i32 312312, align 1000

define ptr @main(){
  %b = getelementptr { i32, [3 x  i32], i32 }, ptr @dd, i32 1, i32 1, i32 2
  ret ptr %b
}
      |};
  [%expect {|
      (CPointer (PointerInt 2032)) |}]
;;

let%expect_test _ =
  interp_test
    {|  
@dd = global i32 312312, align 1000

define < 2 x ptr> @main(){
  %b = getelementptr { i32, [3 x  i32], i32 }, < 2 x ptr> <ptr @dd, ptr @dd>, 
                <2 x i32> < i32 2, i32 2>
  ret < 2 x ptr>  %b
}
      |};
  [%expect
    {|
      (CVector [(CPointer (PointerInt 2040)); (CPointer (PointerInt 2040))]) |}]
;;

let%expect_test _ =
  interp_test
    {|  
@dd = global i32 312312, align 1000

define < 2 x ptr> @main(){
  %b = getelementptr { i32, [3 x  i32], i32 }, < 2 x ptr> <ptr @dd, ptr @dd>, 
                <2 x i32> < i32 2, i32 2>, <2 x i32> < i32 2,i32 0>
  ret < 2 x ptr>  %b
}
      |};
  [%expect
    {|
      (CVector [(CPointer (PointerInt 2056)); (CPointer (PointerInt 2040))]) |}]
;;

let%expect_test _ =
  interp_test
    {|  
@dd = global i32 312312, align 1000

define < 2 x ptr> @main(){
  %b = getelementptr { i32, [3 x  i32], i32 }, < 2 x ptr> <ptr @dd, ptr @dd>, 
                <2 x i32> < i32 2, i32 2>, <1 x i32> < i32 2>
  ret < 2 x ptr>  %b
}
      |};
  [%expect {|
      Error: Vectors have different size in getlementptr! |}]
;;

let%expect_test _ =
  interp_test
    {|  
@dd = global i32 312312, align 1000

define ptr @main(){
  %a = alloca { [2 x i32], {i32, ptr}, i32 }
  store  { [2 x i32], {i32, ptr}, i32 }{ [2 x i32][i32 1, i32 2], {i32, ptr}{i32 3, ptr @dd}, i32 4}, ptr %a
  %b = getelementptr { [2 x i32], {i32, ptr}, i32 }, ptr %a, i32 0, i32 1, i32 1
  %c = load ptr, ptr %b
  ret ptr  %c
}
      |};
  [%expect {|
      (CPointer (PointerInt 2000)) |}]
;;

let%expect_test _ =
  interp_test
    {|  
@dd = global i32 312312, align 1000
@bb  = global i32 23
@cc = global i32 42

define  <3 x i32>  @main(){
  %a = ptrtoint <3 x ptr> <ptr @dd, ptr @bb, ptr @cc> to <3 x i32> 
  ret  <3 x i32>   %a
}
      |};
  [%expect
    {|
      (CVector
         [(CInteger (32, 2000L)); (CInteger (32, 2004L)); (CInteger (32, 2008L))]) |}]
;;

let%expect_test _ =
  interp_test
    {|  
@dd = global i32 312312, align 1000
@bb  = global i32 23
@cc = global i32 42

define  <3 x i32>  @main(){
  %a = ptrtoint <3 x ptr> <ptr @dd, ptr @bb, ptr @cc> to <2 x i32> 
  ret  <3 x i32>   %a
}
      |};
  [%expect {|
      Error: Source vector and destination have different size! |}]
;;

let%expect_test _ =
  interp_test
    {|  
define  float  @main(){
  %a = sitofp i32 -23 to float 
  ret float   %a
}
      |};
  [%expect {|
      (CFloat -23.) |}]
;;

let%expect_test _ =
  interp_test
    {|  
define  <3 x i1>  @main(){
  %a = icmp eq <3xi32> <i32 1, i32 2, i32 3>, <i32 2, i32 2, i32 5> 
  ret <3 x i1> %a 
}
      |};
  [%expect
    {|
      (CVector [(CInteger (1, 0L)); (CInteger (1, 1L)); (CInteger (1, 0L))]) |}]
;;

let%expect_test _ =
  interp_test {|  
define  i1  @main(){
  %a = icmp leq i32 23, 25
  ret i1 %a 
}
      |};
  [%expect {|
      Error: get unknown icmp predicate! |}]
;;

let%expect_test _ =
  interp_test {|  
define  i1  @main(){
  %a = icmp ule i32 23, 25
  ret i1 %a 
}
      |};
  [%expect {|
      (CInteger (1, 1L)) |}]
;;

let%expect_test _ =
  interp_test
    {|  
define i32 @main(){
one:
  br label %two
two:
  br label %three
three:
  %e = phi i32 [ 0, %one ], [1, %two]
  ret i32 %e
}      |};
  [%expect {|
      (CInteger (32, 1L)) |}]
;;

let%expect_test _ =
  interp_test
    {|  
define i32 @main(){
one:
  br label %three
two:
  br label %three
three:
  %e = phi i32 [ 0, %one ], [1, %two]
  ret i32 %e
}      |};
  [%expect {|
      (CInteger (32, 0L)) |}]
;;

let%expect_test _ =
  interp_test
    {|  
define i32 @main(){
one:
  br label %two
two:
  br label %three
three:
  %e = phi i32 [ 0, %one ]
  ret i32 %e
}      |};
  [%expect {|
      Error: LLVM do crash-crash (clang-17.0.3 )! |}]
;;

let%expect_test _ =
  interp_test
    {|  
define i32 @main(){
  %e = select i1 false, i32 17, i32 42     
  ret i32 %e
}      |};
  [%expect {|
      (CInteger (32, 42L)) |}]
;;

let%expect_test _ =
  interp_test
    {|  
define <2 x i32> @main(){
  %e = select <2 x i1><i1 true, i1 false>, <2 x i32> < i32 11, i32 12>, <2 x i32> < i32 21, i32 22>         ; yields i8:17
  ret <2 x i32> %e
}      |};
  [%expect {|
      (CVector [(CInteger (32, 11L)); (CInteger (32, 22L))]) |}]
;;

let%expect_test _ =
  interp_test
    {|  
define i32 @main(){
  br label %c9
c9:
  %c11 = add i32 0, 1
  br label %c14
c12:
  %c12 = add i32 0, 4
  br label %c14
c14:
  %c15 = phi i32 [ %c11, %c9 ], [ %c13, %c12 ]
  ret i32 %c15

}      |};
  [%expect {|
      (CInteger (32, 1L)) |}]
;;
