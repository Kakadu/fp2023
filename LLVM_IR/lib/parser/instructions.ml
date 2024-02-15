(** Copyright 2023-2024, Efremov Alexey *)

(** SPDX-License-Identifier: CC0-1.0 *)

open Angstrom
open CommonParser
open Values
open Types

let parse_instruction_result = parse_local_variable <* whitespaces <* char '='
let parse_align = comma *> word "align" *> whitespaces *> parse_integer <|> return 1

let parse_terminator_instruction =
  let iret =
    word "ret" *> whitespaces *> parse_type_with_value
    >>| function
    | tp, value -> Ast.Ret (tp, value)
  and ibr =
    word "br" *> whitespaces *> type_with_value Ast.TLabel >>| fun value -> Ast.Br value
  and ibr_cond =
    word "br"
    *> lift3
         (fun b l1 l2 -> Ast.BrCond (b, l1, l2))
         (type_with_value (Ast.TInteger 1) <* comma)
         (type_with_value Ast.TLabel <* comma)
         (type_with_value Ast.TLabel)
  and iswitch =
    word "switch"
    *> let* value_type, switch_value = parse_type_with_value <* comma in
       let* default_dest = type_with_value Ast.TLabel in
       let* switch_list =
         whitespaces
         *> char '['
         *> whitespaces
         *> many
              (lift2
                 (fun x y -> x, y)
                 (type_with_value value_type <* comma)
                 (type_with_value Ast.TLabel))
         <* whitespaces
         <* char ']'
       in
       return (Ast.Switch (value_type, switch_value, default_dest, switch_list))
  and iunreachable = word "unreachable" *> return Ast.Unreachable in
  whitespaces *> choice [ iret; ibr; ibr_cond; iswitch; iunreachable ]
;;

let parse_unary_operation =
  let ifneg =
    let* res_var = parse_instruction_result in
    let* tp, value = whitespaces *> word "fneg" *> whitespaces *> parse_type_with_value in
    return (Ast.Fneg (res_var, tp, value))
  in
  whitespaces *> choice [ ifneg ]
;;

let parse_binary_operation =
  let help (mnem : string) (bin_op : Ast.binary_operation_body -> Ast.binary_operation) =
    parse_instruction_result
    >>= fun var ->
    whitespaces
    *> word mnem
    *> whitespaces
    *> option [] (word "nuw" *> whitespaces)
    *> option [] (word "nsw" *> whitespaces)
    *> whitespaces
    *> parse_type_with_value2
    >>= function
    | tp, v1, v2 -> return (bin_op (var, tp, v1, v2))
  in
  whitespaces
  *> choice
       [ help "add" (fun x -> Ast.Add x)
       ; help "fadd" (fun x -> Ast.Fadd x)
       ; help "mul" (fun x -> Ast.Mul x)
       ; help "fmul" (fun x -> Ast.Fmul x)
       ; help "sub" (fun x -> Ast.Sub x)
       ; help "fsub" (fun x -> Ast.Fsub x)
       ; help "udiv" (fun x -> Ast.Udiv x)
       ; help "sdiv" (fun x -> Ast.Sdiv x)
       ; help "fdiv" (fun x -> Ast.Fdiv x)
       ; help "urem" (fun x -> Ast.Urem x)
       ; help "srem" (fun x -> Ast.Srem x)
       ; help "frem" (fun x -> Ast.Frem x)
       ]
;;

let parse_bitwise_binary_operation =
  let help
    (mnem : string)
    (bin_op : Ast.binary_operation_body -> Ast.bitwise_binary_operation)
    =
    parse_instruction_result
    >>= fun var ->
    whitespaces *> word mnem *> whitespaces *> parse_type_with_value2
    >>= function
    | tp, v1, v2 -> return (bin_op (var, tp, v1, v2))
  in
  whitespaces
  *> choice
       [ help "shl" (fun x -> Ast.Shl x)
       ; help "lshr" (fun x -> Ast.Lshr x)
       ; help "ashr" (fun x -> Ast.Ashr x)
       ; help "and" (fun x -> Ast.And x)
       ; help "or" (fun x -> Ast.Or x)
       ; help "xor" (fun x -> Ast.Xor x)
       ]
;;

let parse_vector_instruction =
  let iextractelement =
    let* res_var = parse_instruction_result in
    let* vec_tp, vec_val =
      whitespaces *> word "extractelement" *> whitespaces *> parse_type_with_value
      <* comma
    in
    let* int_tp, int_val = parse_type_with_value in
    return (Ast.Extractelement (res_var, vec_tp, vec_val, int_tp, int_val))
  and iinsertelement =
    let* res_var = parse_instruction_result in
    let* vec_tp, vec_val =
      whitespaces *> word "insertelement" *> whitespaces *> parse_type_with_value <* comma
    in
    let* elem_val =
      match vec_tp with
      | Ast.TVector (_, elem_tp) -> type_with_value elem_tp <* comma
      | _ -> fail "Parser error: insertelement's first value should be vector type"
    in
    let* int_tp, int_val = parse_type_with_value in
    return (Ast.Insertelement (res_var, vec_tp, vec_val, elem_val, int_tp, int_val))
  and ishufflevector =
    let* res_var = parse_instruction_result in
    let* vec_tp, vec_v1 =
      whitespaces *> word "shufflevector" *> whitespaces *> parse_type_with_value <* comma
    in
    let* vec_v2 = type_with_value vec_tp <* comma in
    let* mask_tp, mask_val = parse_type_with_value in
    match mask_tp, mask_val with
    | Ast.TVector (mask_size, Ast.TInteger 32), Ast.Const mask_const ->
      return (Ast.Shufflevector (res_var, vec_tp, vec_v1, vec_v2, mask_size, mask_const))
    | _ -> fail "Parser error: couldn't parse mask for shufflevector instruction"
  in
  whitespaces *> choice [ iextractelement; iinsertelement; ishufflevector ]
;;

let parse_aggregate_instruction =
  let iextractvalue =
    let* res_var = parse_instruction_result in
    let* agg_tp, agg_val =
      whitespaces *> word "extractvalue" *> whitespaces *> parse_type_with_value <* comma
    in
    let* indexes = sep_by1 comma parse_integer in
    return (Ast.Extractvalue (res_var, agg_tp, agg_val, indexes))
  and iinsertvalue =
    let* res_var = parse_instruction_result in
    let* agg_tp, agg_val =
      whitespaces *> word "insertvalue" *> whitespaces *> parse_type_with_value <* comma
    in
    let* value_tp, value_val = parse_type_with_value <* comma in
    let* indexes = sep_by1 comma parse_integer in
    return (Ast.Insertvalue (res_var, agg_tp, agg_val, value_tp, value_val, indexes))
  in
  whitespaces *> choice [ iextractvalue; iinsertvalue ]
;;

let parse_memory_instruction =
  let ialloca =
    lift4
      (fun var tp value align -> Ast.Alloca (var, tp, value, align))
      parse_instruction_result
      (whitespaces *> word "alloca" *> parse_main_type)
      (comma *> parse_type_with_value
       >>= (function
              | Ast.TInteger _, value -> return value
              | _ -> fail "Parser error: excepted integer type")
       <|> return (Ast.Const (Ast.CInteger (1, 1L))))
      (whitespaces *> parse_align)
  and istore =
    lift3
      (fun (tp, value) vptr align -> Ast.Store (tp, value, vptr, align))
      (whitespaces *> word "store" *> whitespaces *> parse_type_with_value)
      (comma *> type_with_value Ast.TPointer)
      parse_align
  and iload =
    lift4
      (fun res tp vptr align -> Ast.Load (res, tp, vptr, align))
      parse_instruction_result
      (whitespaces *> word "load" *> parse_main_type)
      (comma *> type_with_value Ast.TPointer)
      parse_align
  and igetelementptr =
    let* res_var = parse_instruction_result in
    let* res_tp = whitespaces *> word "getelementptr" *> parse_additional_type <* comma in
    let* ptr_tp, ptr_val = parse_type_with_value in
    let* index_lst = choice [ comma *> sep_by1 comma parse_type_with_value; return [] ] in
    return (Ast.Getelementptr (res_var, res_tp, ptr_tp, ptr_val, index_lst))
  in
  whitespaces *> choice [ ialloca; istore; iload; igetelementptr ]
;;

let parse_conversion_instruction =
  let help
    (mnem : string)
    (conv_inst : Ast.conversion_instruction_body -> Ast.conversion_instruction)
    =
    let* res_var = parse_instruction_result in
    let* value_tp_src, value_val =
      whitespaces *> word mnem *> whitespaces *> parse_type_with_value
    in
    let* value_tp_dst =
      whitespaces *> word "to" *> whitespaces *> parse_additional_type
    in
    return (conv_inst (res_var, value_tp_src, value_val, value_tp_dst))
  in
  whitespaces
  *> choice
       [ help "trunc" (fun x -> Ast.TruncTo x)
       ; help "zext" (fun x -> Ast.ZextTo x)
       ; help "sext" (fun x -> Ast.SextTo x)
       ; help "fptoui" (fun x -> Ast.FptouiTo x)
       ; help "fptosi" (fun x -> Ast.FptosiTo x)
       ; help "uitofp" (fun x -> Ast.UitofpTo x)
       ; help "sitofp" (fun x -> Ast.SitofpTo x)
       ; help "ptrtoint" (fun x -> Ast.PrttointTo x)
       ; help "inttoptr" (fun x -> Ast.InttoprtTo x)
       ]
;;

let parse_other_operation =
  let iicmp =
    lift3
      (fun var cond (tp, v1, v2) -> Ast.Icmp (var, cond, tp, v1, v2))
      parse_instruction_result
      (whitespaces *> word "icmp" *> whitespaces *> parse_word)
      (whitespaces *> parse_type_with_value2)
  and ifcmp =
    lift3
      (fun var cond (tp, v1, v2) -> Ast.Fcmp (var, cond, tp, v1, v2))
      parse_instruction_result
      (whitespaces *> word "fcmp" *> whitespaces *> parse_word)
      (whitespaces *> parse_type_with_value2)
  and iphi =
    let* res_var = parse_instruction_result in
    let* res_tp = whitespaces *> word "phi" *> whitespaces *> parse_additional_type in
    let* choose_lst =
      sep_by1
        comma
        (whitespaces *> char '[' *> parse_value res_tp
         >>= (fun value ->
               comma *> parse_value Ast.TLabel >>= fun label -> return (value, label))
         <* whitespaces
         <* char ']')
    in
    return (Ast.Phi (res_var, res_tp, choose_lst))
  and iselect =
    let* res_var = parse_instruction_result in
    let* cond_tp, cond_val =
      whitespaces *> word "select" *> whitespaces *> parse_type_with_value <* comma
    in
    let* value_tp, value_v1 = parse_type_with_value <* comma in
    let* value_v2 = type_with_value value_tp in
    return (Ast.Select (res_var, cond_tp, cond_val, value_tp, value_v1, value_v2))
  and icall =
    lift4
      (fun var ret_tp vptr arg_lst -> Ast.Call (var, ret_tp, vptr, arg_lst))
      parse_instruction_result
      (whitespaces *> word "call" *> whitespaces *> parse_additional_type)
      (whitespaces *> parse_value Ast.TPointer)
      (whitespaces
       *> char '('
       *> sep_by
            comma
            (whitespaces *> parse_type_with_value
             >>= function
             | _, value -> return value)
       <* whitespaces
       <* char ')')
  in
  whitespaces *> choice [ iicmp; ifcmp; iphi; iselect; icall ]
;;

let parse_instruction : Ast.instruction t =
  choice
    [ (parse_terminator_instruction >>| fun ins -> Ast.Terminator ins)
    ; (parse_unary_operation >>| fun ins -> Ast.Unary ins)
    ; (parse_binary_operation >>| fun ins -> Ast.Binary ins)
    ; (parse_bitwise_binary_operation >>| fun ins -> Ast.BitwiseBinary ins)
    ; (parse_vector_instruction >>| fun ins -> Ast.Vector ins)
    ; (parse_aggregate_instruction >>| fun ins -> Ast.Aggregate ins)
    ; (parse_other_operation >>| fun ins -> Ast.Other ins)
    ; (parse_memory_instruction >>| fun ins -> Ast.MemoryAddress ins)
    ; (parse_conversion_instruction >>| fun ins -> Ast.Conversion ins)
    ]
;;

(* ##########################################################*)
(* ################ TERMINATOR ##############################*)
(* ##########################################################*)

let%expect_test _ =
  test_parse parse_instruction Ast.show_instruction "ret i32 %14";
  [%expect
    {|
    (Terminator
       (Ret ((TInteger 32), (FromVariable ((LocalVar "14"), (TInteger 32)))))) |}]
;;

let%expect_test _ =
  test_parse parse_instruction Ast.show_instruction "br label %13";
  [%expect {|
    (Terminator (Br (FromVariable ((LocalVar "13"), TLabel)))) |}]
;;

let%expect_test _ =
  test_parse parse_instruction Ast.show_instruction " br i1 %5, label %6, label %7";
  [%expect
    {|
    (Terminator
       (BrCond ((FromVariable ((LocalVar "5"), (TInteger 1))),
          (FromVariable ((LocalVar "6"), TLabel)),
          (FromVariable ((LocalVar "7"), TLabel))))) |}]
;;

let%expect_test _ =
  test_parse
    parse_instruction
    Ast.show_instruction
    {| switch i32 %val, label %otherwise [ i32 0, label %onzero
  i32 1, label %onone
  i32 2, label %ontwo ] |};
  [%expect
    {|
    (Terminator
       (Switch ((TInteger 32), (FromVariable ((LocalVar "val"), (TInteger 32))),
          (FromVariable ((LocalVar "otherwise"), TLabel)),
          [((Const (CInteger (32, 0L))),
            (FromVariable ((LocalVar "onzero"), TLabel)));
            ((Const (CInteger (32, 1L))),
             (FromVariable ((LocalVar "onone"), TLabel)));
            ((Const (CInteger (32, 2L))),
             (FromVariable ((LocalVar "ontwo"), TLabel)))
            ]
          ))) |}]
;;

let%expect_test _ =
  test_parse parse_instruction Ast.show_instruction {| unreachable |};
  [%expect {|
    (Terminator Unreachable) |}]
;;

(* ##########################################################*)
(* ##################### UNARY ##############################*)
(* ##########################################################*)

let%expect_test _ =
  test_parse parse_instruction Ast.show_instruction {| %23 = fneg float %val   |};
  [%expect
    {|
    (Unary
       (Fneg ((LocalVar "23"), TFloat, (FromVariable ((LocalVar "val"), TFloat))
          ))) |}]
;;

(* ##########################################################*)
(* #################### BINARY ##############################*)
(* ##########################################################*)

let%expect_test _ =
  test_parse parse_instruction Ast.show_instruction "  %9 = sub i32 %8, 1";
  [%expect
    {|
    (Binary
       (Sub
          ((LocalVar "9"), (TInteger 32),
           (FromVariable ((LocalVar "8"), (TInteger 32))),
           (Const (CInteger (32, 1L)))))) |}]
;;

let%expect_test _ =
  test_parse parse_instruction Ast.show_instruction "   %12 = mul i32 %10, %11";
  [%expect
    {|
    (Binary
       (Mul
          ((LocalVar "12"), (TInteger 32),
           (FromVariable ((LocalVar "10"), (TInteger 32))),
           (FromVariable ((LocalVar "11"), (TInteger 32)))))) |}]
;;

(* ##########################################################*)
(* #################### BITWISE #############################*)
(* ########################################################## *)

let%expect_test _ =
  test_parse parse_instruction Ast.show_instruction "  %res =  xor i32 0x1A, %var ";
  [%expect
    {|
      (BitwiseBinary
         (Xor
            ((LocalVar "res"), (TInteger 32), (Const (CInteger (32, 26L))),
             (FromVariable ((LocalVar "var"), (TInteger 32)))))) |}]
;;

let%expect_test _ =
  test_parse parse_instruction Ast.show_instruction "  %res = ashr i8 -4, -0x23 ";
  [%expect
    {|
      (BitwiseBinary
         (Ashr
            ((LocalVar "res"), (TInteger 8), (Const (CInteger (8, 252L))),
             (Const (CInteger (8, 221L)))))) |}]
;;

(* ##########################################################*)
(* ##################### VECTOR #############################*)
(* ##########################################################*)

let%expect_test _ =
  test_parse
    parse_instruction
    Ast.show_instruction
    "%result = extractelement <4 x i32> %vec, i32 0 ";
  [%expect
    {|
      (Vector
         (Extractelement ((LocalVar "result"), (TVector (4, (TInteger 32))),
            (FromVariable ((LocalVar "vec"), (TVector (4, (TInteger 32))))),
            (TInteger 32), (Const (CInteger (32, 0L)))))) |}]
;;

let%expect_test _ =
  test_parse
    parse_instruction
    Ast.show_instruction
    "%result = insertelement <4 x i32> %vec, i32 1, i32 0    ; yields <4 x i32> ";
  [%expect
    {|
      (Vector
         (Insertelement ((LocalVar "result"), (TVector (4, (TInteger 32))),
            (FromVariable ((LocalVar "vec"), (TVector (4, (TInteger 32))))),
            (Const (CInteger (32, 1L))), (TInteger 32), (Const (CInteger (32, 0L)))
            ))) |}]
;;

let%expect_test _ =
  test_parse
    parse_instruction
    Ast.show_instruction
    {| %result = shufflevector <4 x i32> %v1, <4 x i32> %v2,
        <8 x i32> <i32 0, i32 1, i32 2, i32 3, i32 4, i32 5, i32 6, i32 7 > |};
  [%expect
    {|
      (Vector
         (Shufflevector ((LocalVar "result"), (TVector (4, (TInteger 32))),
            (FromVariable ((LocalVar "v1"), (TVector (4, (TInteger 32))))),
            (FromVariable ((LocalVar "v2"), (TVector (4, (TInteger 32))))), 8,
            (CVector
               [(CInteger (32, 0L)); (CInteger (32, 1L)); (CInteger (32, 2L));
                 (CInteger (32, 3L)); (CInteger (32, 4L)); (CInteger (32, 5L));
                 (CInteger (32, 6L)); (CInteger (32, 7L))])
            ))) |}]
;;

(* #############################################################*)
(* ##################### Aggregate #############################*)
(* #############################################################*)

let%expect_test _ =
  test_parse
    parse_instruction
    Ast.show_instruction
    "%result = extractvalue {i32, float} %agg, 0    ; yields i32";
  [%expect
    {|
      (Aggregate
         (Extractvalue ((LocalVar "result"), (TStruct [(TInteger 32); TFloat]),
            (FromVariable ((LocalVar "agg"), (TStruct [(TInteger 32); TFloat]))),
            [0]))) |}]
;;

let%expect_test _ =
  test_parse
    parse_instruction
    Ast.show_instruction
    "%agg3 = insertvalue {i32, {float}} %agg1, float %val, 1, 0 ";
  [%expect
    {|
      (Aggregate
         (Insertvalue ((LocalVar "agg3"),
            (TStruct [(TInteger 32); (TStruct [TFloat])]),
            (FromVariable ((LocalVar "agg1"),
               (TStruct [(TInteger 32); (TStruct [TFloat])]))),
            TFloat, (FromVariable ((LocalVar "val"), TFloat)), [1; 0]))) |}]
;;

(* ##########################################################*)
(* ################ MEMORY ADDRESS ##########################*)
(* ##########################################################*)

let%expect_test _ =
  test_parse parse_instruction Ast.show_instruction "  %2 = alloca i32, align 4";
  [%expect
    {|
      (MemoryAddress
         (Alloca ((LocalVar "2"), (TInteger 32), (Const (CInteger (1, 1L))), 4))) |}]
;;

let%expect_test _ =
  test_parse parse_instruction Ast.show_instruction "  %2 = alloca i32";
  [%expect
    {|
      (MemoryAddress
         (Alloca ((LocalVar "2"), (TInteger 32), (Const (CInteger (1, 1L))), 1))) |}]
;;

let%expect_test _ =
  test_parse parse_instruction Ast.show_instruction "   %2 = alloca i32, i32 4, align 4";
  [%expect
    {|
      (MemoryAddress
         (Alloca ((LocalVar "2"), (TInteger 32), (Const (CInteger (32, 4L))), 4))) |}]
;;

let%expect_test _ =
  test_parse parse_instruction Ast.show_instruction "   %2 = alloca i32, i32 %kakadu";
  [%expect
    {|
      (MemoryAddress
         (Alloca ((LocalVar "2"), (TInteger 32),
            (FromVariable ((LocalVar "kakadu"), (TInteger 32))), 1))) |}]
;;

let%expect_test _ =
  test_parse parse_instruction Ast.show_instruction "%11 = load i32, ptr %3, align 4";
  [%expect
    {|
      (MemoryAddress
         (Load ((LocalVar "11"), (TInteger 32),
            (FromVariable ((LocalVar "3"), TPointer)), 4))) |}]
;;

let%expect_test _ =
  test_parse parse_instruction Ast.show_instruction "  store i32 %12, ptr %2, align 4";
  [%expect
    {|
      (MemoryAddress
         (Store ((TInteger 32), (FromVariable ((LocalVar "12"), (TInteger 32))),
            (FromVariable ((LocalVar "2"), TPointer)), 4))) |}]
;;

let%expect_test _ =
  test_parse
    parse_instruction
    Ast.show_instruction
    "%vptr = getelementptr {i32, <2 x i8>}, ptr %svptr, i64 0, i32 1, i32 1";
  [%expect
    {|
    (MemoryAddress
       (Getelementptr ((LocalVar "vptr"),
          (TStruct [(TInteger 32); (TVector (2, (TInteger 8)))]), TPointer,
          (FromVariable ((LocalVar "svptr"), TPointer)),
          [((TInteger 64), (Const (CInteger (64, 0L))));
            ((TInteger 32), (Const (CInteger (32, 1L))));
            ((TInteger 32), (Const (CInteger (32, 1L))))]
          ))) |}]
;;

let%expect_test _ =
  test_parse
    parse_instruction
    Ast.show_instruction
    {| %vptr = getelementptr  i32, <4 x ptr> %vs, <4 x i64> %vind1,
        <4 x i32> <i32 2, i32 2, i32 2, i32 2>,
        <4 x i32> <i32 1, i32 1, i32 1, i32 1>,
        <4 x i32> %vind4,
        <4 x i64> <i64 13, i64 13, i64 13, i64 13> |};
  [%expect
    {|
    (MemoryAddress
       (Getelementptr ((LocalVar "vptr"), (TInteger 32), (TVector (4, TPointer)),
          (FromVariable ((LocalVar "vs"), (TVector (4, TPointer)))),
          [((TVector (4, (TInteger 64))),
            (FromVariable ((LocalVar "vind1"), (TVector (4, (TInteger 64))))));
            ((TVector (4, (TInteger 32))),
             (Const
                (CVector
                   [(CInteger (32, 2L)); (CInteger (32, 2L));
                     (CInteger (32, 2L)); (CInteger (32, 2L))])));
            ((TVector (4, (TInteger 32))),
             (Const
                (CVector
                   [(CInteger (32, 1L)); (CInteger (32, 1L));
                     (CInteger (32, 1L)); (CInteger (32, 1L))])));
            ((TVector (4, (TInteger 32))),
             (FromVariable ((LocalVar "vind4"), (TVector (4, (TInteger 32))))));
            ((TVector (4, (TInteger 64))),
             (Const
                (CVector
                   [(CInteger (64, 13L)); (CInteger (64, 13L));
                     (CInteger (64, 13L)); (CInteger (64, 13L))])))
            ]
          ))) |}]
;;

(* ##########################################################*)
(* #################### CONVERSION ##########################*)
(* ##########################################################*)

let%expect_test _ =
  test_parse parse_instruction Ast.show_instruction " %Y = inttoptr i32 255 to ptr";
  [%expect
    {|
      (Conversion
         (InttoprtTo
            ((LocalVar "Y"), (TInteger 32), (Const (CInteger (32, 255L))), TPointer))) |}]
;;

let%expect_test _ =
  test_parse parse_instruction Ast.show_instruction "%Y = fptoui float 1.0 to i1";
  [%expect
    {|
      (Conversion
         (FptouiTo ((LocalVar "Y"), TFloat, (Const (CFloat 1.)), (TInteger 1)))) |}]
;;

let%expect_test _ =
  test_parse
    parse_instruction
    Ast.show_instruction
    "%Z = zext <2 x i16> <i16 8, i16 7> to <2 x i32>";
  [%expect
    {|
      (Conversion
         (ZextTo
            ((LocalVar "Z"), (TVector (2, (TInteger 16))),
             (Const (CVector [(CInteger (16, 8L)); (CInteger (16, 7L))])),
             (TVector (2, (TInteger 32)))))) |}]
;;

(* ##########################################################*)
(* ##################### OTHER ##############################*)
(* ##########################################################*)

let%expect_test _ =
  test_parse parse_instruction Ast.show_instruction " %5 = icmp slt i32 %4, 1";
  [%expect
    {|
    (Other
       (Icmp ((LocalVar "5"), "slt", (TInteger 32),
          (FromVariable ((LocalVar "4"), (TInteger 32))),
          (Const (CInteger (32, 1L)))))) |}]
;;

let%expect_test _ =
  test_parse parse_instruction Ast.show_instruction "   %10 = call i32 @fac(i32 %9)  ";
  [%expect
    {|
    (Other
       (Call ((LocalVar "10"), (TInteger 32),
          (Const (CPointer (PointerGlob (GlobalVar "fac")))),
          [(FromVariable ((LocalVar "9"), (TInteger 32)))]))) |}]
;;

let%expect_test _ =
  test_parse parse_instruction Ast.show_instruction "  %res = fcmp one float 4.0, 5.0";
  [%expect
    {|
    (Other
       (Fcmp ((LocalVar "res"), "one", TFloat, (Const (CFloat 4.)),
          (Const (CFloat 5.))))) |}]
;;

let%expect_test _ =
  test_parse
    parse_instruction
    Ast.show_instruction
    " %indvar = phi i32 [ 0, %LoopHeader ], [ %nextindvar, %Loop ]";
  [%expect
    {|
    (Other
       (Phi ((LocalVar "indvar"), (TInteger 32),
          [((Const (CInteger (32, 0L))),
            (FromVariable ((LocalVar "LoopHeader"), TLabel)));
            ((FromVariable ((LocalVar "nextindvar"), (TInteger 32))),
             (FromVariable ((LocalVar "Loop"), TLabel)))
            ]
          ))) |}]
;;

let%expect_test _ =
  test_parse parse_instruction Ast.show_instruction "%X = select i1 true, i8 17, i8 42";
  [%expect
    {|
      (Other
         (Select ((LocalVar "X"), (TInteger 1), (Const (CInteger (1, 1L))),
            (TInteger 8), (Const (CInteger (8, 17L))), (Const (CInteger (8, 42L)))
            ))) |}]
;;
