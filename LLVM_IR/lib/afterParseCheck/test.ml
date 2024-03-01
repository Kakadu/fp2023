(** Copyright 2023-2024, Efremov Alexey *)

(** SPDX-License-Identifier: CC0-1.0 *)

open SsaCheck

let test_ssa str =
  match Parser.Parsing.parse_program str with
  | Result.Error s -> Printf.printf "Parser error: \n %s" s
  | Result.Ok glob_lst ->
    (match run_ssa_glob_list glob_lst with
     | Result.Ok _ -> Printf.printf "Pass"
     | Result.Error s -> Printf.printf "SSA check failed: \n %s" s)
;;

let%expect_test _ =
  test_ssa "  %res = fcmp one float 4.0, 5.0";
  [%expect {|
    Parser error:
     : end_of_input |}]
;;

let%expect_test _ =
  test_ssa
    {| @dd = global i32 0, align 4
     @bb = global i32 32, align 4
     @cc = global i32 32, align 4
     @aa = global i32 32, align 4 |};
  [%expect {|
    Pass |}]
;;

let%expect_test _ =
  test_ssa {| @bb = global i32 0, align 4
  @bb = global i32 32, align 4 |};
  [%expect {|
    SSA check failed:
     Variable bb already was assigned |}]
;;

let%expect_test _ =
  test_ssa
    {| @bb = global i32 0, align 4
    define i32 @bb(){
      %1 = call i32 @ds()
      %2 = call i32 @ds()
      ret i32 0
     } |};
  [%expect {|
    SSA check failed:
     Variable bb already was assigned |}]
;;

let%expect_test _ =
  test_ssa "@dd = global i32 0, align 4\ndefine i32 @bb(i32 %0, i32 %1){ ret i32 0 }";
  [%expect {|
    Pass |}]
;;

let%expect_test _ =
  test_ssa
    "@dd = global i32 0, align 4\ndefine i32 @bb(i32 %0, i32 %1, i35 %0){ret i32 0}";
  [%expect {|
    SSA check failed:
     Variable 0 already was assigned |}]
;;

let%expect_test _ =
  test_ssa
    {| define i32 @ds() {
          %1 = alloca i32, align 4
          store i32 5, ptr %1, align 4
          %2 = load i32, ptr %1, align 4
          %3 = load i32, ptr @bb, align 4
          %4 = add nsw i32 %3, %2
          store i32 %4, ptr @bb, align 4
          %5 = load i32, ptr @dd, align 4
          store i32 %5, ptr %1, align 4
          %6 = load i32, ptr %1, align 4
          ret i32 %6
        } |};
  [%expect {|
    Pass |}]
;;

let%expect_test _ =
  test_ssa
    {| define i32 @ds(i32 %6) {
          %1 = alloca i32, align 4
          store i32 5, ptr %1, align 4
          %2 = load i32, ptr %1, align 4
          %3 = load i32, ptr @bb, align 4
          %4 = add nsw i32 %3, %2
          store i32 %4, ptr @bb, align 4
          %5 = load i32, ptr @dd, align 4
          store i32 %5, ptr %1, align 4
          %6 = load i32, ptr %1, align 4
          ret i32 %6
        } |};
  [%expect {|
    SSA check failed:
     Variable 6 already was assigned |}]
;;

let%expect_test _ =
  test_ssa
    {| define i32 @d1(i32 %0) {
          %1 = alloca i32, align 4
          ret i32 %6
        }
        
        define i32 @d2(i32 %0) {
          %1 = alloca i32, align 4
          ret i32 %6
        } |};
  [%expect {|
    Pass |}]
;;
