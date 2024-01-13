(** Copyright 2021-2023, Georgy Sichkar *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

let pars_class str =
  match Parser.parse_ast str with
  | Result.Ok (Ast (x :: _)) -> Some x
  | _ -> None
;;

let exception_name = Ast.Id "Exception"

let exception_decl =
  let info = {|
        class Exception{
          Exception(){}
        } 
    |} in
  pars_class info
;;

let pars_ast str =
  match Parser.parse_ast str with
  | Result.Ok x -> Some x
  | _ -> None
;;

let base_lib_decl =
  let info =
    {|
      class Division_by_zero : Exception {}

      class FileInfo{
        string path;
        bool Exists = false;
        bool Opened = false;

        FileInfo(string path_)
        {
          path = path_;
          Opened = true;
        }

        void CloseFile() {}

        string ReadAllText () {}

        void AppendAllText(string info) {}

        } 
    |}
  in
  pars_ast info
;;
