(** Copyright 2023-2024 Vadim Yakshigulov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Angstrom

let regi8 = [ "ah"; "al"; "bh"; "bl"; "ch"; "cl"; "dh"; "dl" ]
let regi16 = [ "ax"; "bx"; "cx"; "dx" ]
let regi32 = [ "eax"; "ebx"; "ecx"; "edx"; "esi"; "edi"; "esp"; "ebp" ]
let regi64 = [ "rax"; "rbx"; "rcx"; "rdx"; "rsi"; "rdi"; "rsp"; "rbp" ]
let regs = regi8 @ regi16 @ regi32 @ regi64

let string_of_reg s =
  match String.lowercase_ascii s with
  | "ah" -> i8 ah
  | "al" -> i8 al
  | "bh" -> i8 bh
  | "bl" -> i8 bl
  | "ch" -> i8 ch
  | "cl" -> i8 cl
  | "dh" -> i8 dh
  | "dl" -> i8 dl
  | "ax" -> i16 ax
  | "bx" -> i16 bx
  | "cx" -> i16 cx
  | "dx" -> i16 dx
  | "eax" -> i32 eax
  | "ebx" -> i32 ebx
  | "ecx" -> i32 ecx
  | "edx" -> i32 edx
  | "esi" -> i32 esi
  | "edi" -> i32 edi
  | "esp" -> i32 esp
  | "ebp" -> i32 ebp
  | "rax" -> i64 rax
  | "rbx" -> i64 rbx
  | "rcx" -> i64 rcx
  | "rdx" -> i64 rdx
  | "rsi" -> i64 rsi
  | "rdi" -> i64 rdi
  | "rsp" -> i64 rsp
  | "rbp" -> i64 rbp
  | s ->
    failwith (String.concat " " [ "Register name"; s; "is not matched with constructor" ])
;;

let mnemonics =
  [ "mov"
  ; "push"
  ; "call"
  ; "jmp"
  ; "ret"
  ; "syscall"
  ; "add"
  ; "xor"
  ; "jle"
  ; "dec"
  ; "imul"
  ; "pop"
  ; "sub"
  ; "cmp"
  ; "je"
  ]
;;

let string_of_mnemonic s =
  match String.lowercase_ascii s with
  | "mov" -> mov
  | "push" -> push
  | "call" -> call
  | "jmp" -> jmp
  | "ret" -> ret
  | "syscall" -> syscall
  | "add" -> add
  | "xor" -> xor
  | "jle" -> jle
  | "dec" -> dec
  | "imul" -> imul
  | "pop" -> pop
  | "sub" -> sub
  | "cmp" -> cmp
  | "je" -> je
  | s ->
    failwith (String.concat " " [ "Mnemonic name"; s; "is not matched with constructor" ])
;;

let is_reseved name = List.mem name (regs @ mnemonics)
let sections = [ ".data"; ".text" ]

let is_space = function
  | ' ' | '\t' -> true
  | _ -> false
;;

let space_p = satisfy is_space *> return () <?> "space_p"

(* Get rid of some number of zero or more spaces *)
let spaces_p = skip_many space_p <?> "spaces_p"

(* Get rid of some number of one or more spaces *)
let trim_p p = spaces_p *> p <* spaces_p
let comma_p = trim_p (char ',') <?> "comma_p"

(* Parse a comment *)
let comment_p = char ';' *> skip_while (fun c -> not (Char.equal c '\n')) <?> "comment_p"
let trim_p x = spaces_p *> x <* spaces_p
let comma_p = trim_p (char ',') <?> "comma_p"
let between t1 t2 e1 = t1 *> (trim_p @@ e1) <* t2

let skip_empty_p =
  skip_many (space_p <|> end_of_line <|> comment_p) <?> "skip_empty_lines_p"
;;

let is_decimal_digit = function
  | '0' .. '9' -> true
  | _ -> false
;;

let is_hex_digit = function
  | '0' .. '9' | 'a' .. 'f' | 'A' .. 'Z' -> true
  | _ -> false
;;

let is_letter = function
  | 'a' .. 'z' | 'A' .. 'Z' -> true
  | _ -> false
;;

let is_label_char = function
  | '_' | '$' | '#' | '@' | '~' | '.' | '?' -> true
  | c -> is_decimal_digit c || is_letter c
;;

let label_str_p =
  let is_valid c = is_letter c || Char.equal '_' c in
  let* s =
    lift2 (fun c s -> Char.escaped c ^ s) (satisfy is_valid) (take_while is_label_char)
  in
  if is_reseved s then fail "This lable name is reserved" else return s
;;

(* Regs parser *)
let reg_p =
  (let parser = string in
   choice (List.map parser regs) >>| string_of_reg)
  <?> "reg_p"
;;

(* Mnemonic parser *)
let mnemonic_p =
  let parser = string in
  choice (List.map parser mnemonics) >>| string_of_mnemonic <?> "mnemonic_p"
;;

(*
   [ "mov"
  ; "push"
  ; "call"
  ; "jmp"
  ; "ret"
  ; "syscall"
  ; "add"
  ; "xor"
  ; "jle"
  ; "dec"
  ; "imul"
  ; "pop"
  ]*)

(* NumVal parsers *)
let decimal_int_p =
  let sign_p = string "+" <|> string "-" in
  let const_str_p = lift2 ( ^ ) (option "" sign_p) (take_while1 is_decimal_digit) in
  const_str_p >>| int_of_string <?> "decimal_int_p"
;;

let hex_int_p =
  let hex_of_string s = int_of_string ("0x" ^ s) in
  let start_with_0x =
    string "0x" *> take_while1 is_hex_digit
    >>| hex_of_string
    <?> "hex_int_p:start_with_0x"
  in
  let stop_with_h =
    take_while1 is_hex_digit <* char 'h' >>| hex_of_string <?> "hex_int_p:stop_with_h"
  in
  choice [ start_with_0x; stop_with_h ] <?> "hex_int_p"
;;

let num_val_p = hex_int_p <|> decimal_int_p >>| numval <?> "num_val_p"

(* StrVal parser *)
let str_val_p =
  let between_quote quote =
    char quote *> take_till (fun c -> Char.equal c quote) <* char quote
  in
  between_quote '\'' <|> between_quote '"' >>| strval <?> "str_val_p"
;;

(* Immidiate parser *)
let imm_p = choice [ num_val_p; str_val_p ] >>| imm <?> "imm_p"

(* Ref parser *)
let reg_ref_p = between (char '[') (char ']') reg_p >>| ref <?> "reg_ref_p"

(* RegRef parser *)
let op_reg_ref_p = reg_ref_p >>| regref <?> "op_reg_ref_p"
let op_reg_p = reg_p >>| reg <?> "op_reg_p"

(* LableRef parser *)
let label_ref_p = label_str_p >>| lableref

(* InstrOperand0 parser*)
let instroperand0_p = trim_p mnemonic_p >>| instroperand0 <?> "instroperand0_p"

(* InstrOperand1 parser*)
let instroperand1_p =
  let parser = choice [ op_reg_p; op_reg_ref_p; imm_p; label_ref_p ] in
  let* mnemonic = trim_p mnemonic_p in
  trim_p parser >>| instroperand1 mnemonic <?> "instroperand1_p"
;;

let instroperand2_p =
  let parser = choice [ op_reg_p; op_reg_ref_p; imm_p; label_ref_p ] in
  let* mnemonic = trim_p mnemonic_p in
  let* op1 = trim_p parser in
  comma_p *> trim_p parser >>| instroperand2 mnemonic op1 <?> "instroperand2_p"
;;

let instruction_p =
  choice [ instroperand2_p; instroperand1_p; instroperand0_p ] <?> "instruction_p"
;;

let section_p =
  let parser = choice (List.map string sections) in
  string_ci "section" *> trim_p parser >>| section
;;

let global_p =
  let parser = label_str_p in
  string_ci "global" *> trim_p parser >>| global
;;

let derictive_p = trim_p (choice [ section_p; global_p ]) >>| directive <?> "derictive_p"
let label_p = trim_p (label_str_p <* char ':') >>| label

let statement_p =
  choice [ instruction_p >>| instruction; derictive_p; label_p ] <?> "statement_p"
;;

let ast_p =
  skip_empty_p
  *> sep_by1 (end_of_line *> skip_empty_p) (trim_p statement_p <* option () comment_p)
  <* skip_empty_p
;;

(* Parsing utils*)
let parse_string p s = Angstrom.parse_string ~consume:Consume.All p s

let parse p show str =
  match parse_string p str with
  | Result.Error e -> Format.printf "Error: %s" e
  | Result.Ok ast -> Format.printf "%s" (show ast)
;;

let%expect_test _ =
  parse reg_p show_reg "eax";
  [%expect {| (I32 Eax) |}]
;;

let%expect_test _ =
  parse reg_ref_p show_reg_ref "[    eax ]";
  [%expect {| (Ref (I32 Eax)) |}]
;;

let%expect_test _ =
  parse imm_p show_operand1 "0x10";
  [%expect {| (Imm (NumVal 16)) |}]
;;

let%expect_test _ =
  parse num_val_p show_value "231";
  [%expect {| (NumVal 231) |}]
;;

let%expect_test _ =
  parse num_val_p show_value "10h";
  [%expect {| (NumVal 16) |}]
;;

let%expect_test _ =
  parse num_val_p show_value "0x10";
  [%expect {| (NumVal 16) |}]
;;

let%expect_test _ =
  parse label_ref_p show_operand1 "l~.a@#ble";
  [%expect {| (LableRef "l~.a@#ble") |}]
;;

let%expect_test _ =
  parse label_ref_p show_operand1 "mov";
  [%expect {| Error: : This lable name is reserved |}]
;;

let%expect_test _ =
  parse instroperand1_p show_instruction "jmp l~.a@#ble";
  [%expect {| (InstrOperand1 (Jmp, (LableRef "l~.a@#ble"))) |}]
;;

let%expect_test _ =
  parse instroperand0_p show_instruction "ret   ";
  [%expect {| (InstrOperand0 Ret) |}]
;;

let%expect_test _ =
  parse instroperand1_p show_instruction "push eax";
  [%expect {| (InstrOperand1 (Push, (Reg (I32 Eax)))) |}]
;;

let%expect_test _ =
  parse instroperand1_p show_instruction "push [eax ]";
  [%expect {| (InstrOperand1 (Push, (RegRef (Ref (I32 Eax))))) |}]
;;

let%expect_test _ =
  parse instroperand1_p show_instruction "jmp \" Hello! \"";
  [%expect {| (InstrOperand1 (Jmp, (Imm (StrVal " Hello! ")))) |}]
;;

let%expect_test _ =
  parse instroperand1_p show_instruction " jmp \'!olleH  \'";
  [%expect {| (InstrOperand1 (Jmp, (Imm (StrVal "!olleH  ")))) |}]
;;

let%expect_test _ =
  parse instroperand2_p show_instruction "mov eax, eax ";
  [%expect {| (InstrOperand2 (Mov, (Reg (I32 Eax)), (Reg (I32 Eax)))) |}]
;;

let%expect_test _ =
  parse instruction_p show_instruction "mov eax, [ ebx ] ";
  [%expect {| (InstrOperand2 (Mov, (Reg (I32 Eax)), (RegRef (Ref (I32 Ebx))))) |}]
;;

let%expect_test _ =
  parse instroperand1_p show_instruction {|mov [ esp ]|};
  [%expect {| (InstrOperand1 (Mov, (RegRef (Ref (I32 Esp))))) |}]
;;
