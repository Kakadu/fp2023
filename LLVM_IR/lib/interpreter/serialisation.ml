(** Copyright 2023-2024, Efremov Alexey *)

(** SPDX-License-Identifier: CC0-1.0 *)

open State

let div_up x y =
  let res = x / y in
  if y * res <> x then res + 1 else res
;;

let get_n_byte x n =
  Char.chr
    (Int64.to_int
       (Int64.shift_right_logical
          (Int64.logand x (Int64.shift_left 0xffL (8 * n)))
          (8 * n)))
;;

let rec raw_date_len : Ast.tp -> int = function
  | Ast.TVoid -> 0
  | Ast.TFloat -> 8
  | Ast.TInteger x -> div_up x 8
  | Ast.TPointer -> 8
  | Ast.TArr (n, tp) | Ast.TVector (n, tp) -> raw_date_len tp * n
  | Ast.TStruct tps -> List.fold_left (fun acc tp -> acc + raw_date_len tp) 0 tps
  | Ast.TLabel | Ast.TFunc (_, _) ->
    let _ = Printf.printf "Imposible error: func and label can't be serealized" in
    0
;;

let serialise_int sz value =
  let len = div_up sz 8 in
  List.init len (get_n_byte value)
;;

let serialise_flt flt =
  let iflt = Int64.bits_of_float flt in
  serialise_int 64 iflt
;;

let rec serialise_aggregate_tp cnsts =
  let f cnst =
    let* lst2 = serialise_with_state cnst in
    return lst2
  in
  let* lsts = map_list f cnsts in
  return (List.concat lsts)

and serialise_with_state : Ast.const -> (state, char list) t = function
  | Ast.CVoid -> return []
  | Ast.CInteger (sz, value) -> return (serialise_int sz value)
  | Ast.CFloat flt -> return (serialise_flt flt)
  | Ast.CPointer (Ast.PointerInt integer) ->
    return (serialise_int 63 (Int64.of_int integer))
  | Ast.CVector cnst | Ast.CArr cnst | Ast.CStruct cnst -> serialise_aggregate_tp cnst
  | Ast.CLabel _ | Ast.CFunc _ ->
    fail "Imposible error: func and label can't be serealized"
  | Ast.CPointer (Ast.PointerGlob x) -> read_var x >>= serialise_with_state
;;

let bytes_to_int lst =
  let f el acc = Int64.add (Int64.mul acc 0x100L) (Int64.of_int (Char.code el)) in
  List.fold_right f lst 0L
;;

let deserialise_int sz lst = Common.IrInts.create (bytes_to_int lst) sz

let deserialise_flt lst =
  let iflt = bytes_to_int lst in
  Ast.CFloat (Int64.float_of_bits iflt)
;;

let deserialise_ptr lst =
  let iflt = Int64.to_int (bytes_to_int lst) in
  Ast.CPointer (Ast.PointerInt iflt)
;;

let rec bytes_to_cnst_lst : char list -> Ast.tp list -> Ast.const list =
  let split_list ind mslt =
    let rec help x lst1 lst2 =
      if x < ind
      then (
        match lst2 with
        | h :: tl -> help (x + 1) (h :: lst1) tl
        | _ -> help (x + 1) lst1 lst2)
      else lst1, lst2
    in
    let lst1, lst2 = help 0 [] mslt in
    List.rev lst1, lst2
  in
  fun data tps ->
    let f (raw_data, acc_lst) tp =
      let len = raw_date_len tp in
      let lst1, lst2 = split_list len raw_data in
      lst2, deserialise tp lst1 :: acc_lst
    in
    let _, res = List.fold_left f (data, []) tps in
    List.rev res

and deserialise_vec n tp lst =
  let tps = List.init n (fun _ -> tp) in
  Ast.CVector (bytes_to_cnst_lst lst tps)

and deserialise_arr n tp lst =
  let tps = List.init n (fun _ -> tp) in
  Ast.CArr (bytes_to_cnst_lst lst tps)

and deserialise_struct tps lst = Ast.CStruct (bytes_to_cnst_lst lst tps)

and deserialise : Ast.tp -> char list -> Ast.const =
  fun tp lst ->
  match tp with
  | Ast.TVoid -> Ast.CVoid
  | Ast.TInteger sz -> deserialise_int sz lst
  | Ast.TFloat -> deserialise_flt lst
  | Ast.TPointer -> deserialise_ptr lst
  | Ast.TVector (n, tp) -> deserialise_vec n tp lst
  | Ast.TArr (n, tp) -> deserialise_arr n tp lst
  | Ast.TStruct tps -> deserialise_struct tps lst
  | Ast.TLabel | Ast.TFunc (_, _) ->
    let _ = Printf.printf "Imposible error: func and label can't be serealized" in
    Ast.CVoid
;;

let ser_deser target =
  let res = run (serialise_with_state target) empty_state in
  match res with
  | _, Result.Ok bts -> deserialise (Ast.const_to_tp target) bts
  | _, Result.Error _ -> CVoid
;;

let ser_test target print =
  let t1 = Ast.show_const (ser_deser target) in
  let t2 = Ast.show_const target in
  let _ =
    if print
    then
      Printf.printf
        {| ########################
         Exp: %s
         ----------------
         Get: %s
         ######################## 
         
         |}
        t2
        t1
  in
  String.equal t1 t2
;;

let%test _ = ser_test Ast.CVoid false
let%test _ = ser_test (Common.IrInts.create 0x23232323L 32) false
let%test _ = ser_test (Common.IrInts.create 0x111111111L 32) false
let%test _ = ser_test (Common.IrInts.create (-1L) 32) false
let%test _ = ser_test (Common.IrInts.create (-1L) 7) false
let%test _ = ser_test (Common.IrInts.create (-45L) 11) false
let%test _ = ser_test (CFloat 1.00) false
let%test _ = ser_test (CFloat (-21.00)) false
let%test _ = ser_test (CFloat 30.0002) false
let%test _ = ser_test (CFloat 30.000000000000002) false
let%test _ = ser_test (CFloat 123.45678901234567) false
let%test _ = ser_test (CPointer (PointerInt 0x11)) false
let%test _ = ser_test (CPointer (PointerInt 0x40000)) false

let ser_parse_test str print =
  match Parser.CommonParser.test_parse_res Parser.Values.parse_type_with_value str with
  | Result.Ok (_, Ast.Const cnst) -> ser_test cnst print
  | _ ->
    let _ = if print then Printf.printf "Error during parse '%s'\n" str in
    false
;;

let%test _ =
  ser_parse_test
    "<8 x i32> <i32 0, i32 134, i32 2, i32 3, i32 4, i32 5, i32 6, i32 7 > "
    false
;;

let%test _ =
  ser_parse_test
    "[2 x [2 x i32]] [[2 x i32] [i32 1, i32 3], [2 x i32] [i32 2, i32 4]] "
    false
;;

let%test _ = ser_parse_test "{i32, float} { i32 4, float 17.0} " false
