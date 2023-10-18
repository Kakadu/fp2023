open! Base
open Angstrom
open Ast
open Common

let parse_pat_var = parse_name >>| fun name -> Pat_var name

let parse_pattern = ws *> choice [parse_pat_var]
