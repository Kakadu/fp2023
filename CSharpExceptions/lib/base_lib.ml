(** Copyright 2021-2023, Georgy Sichkar *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

let pars str =
  match Parser.parse_ast str with
  | Result.Ok (Ast (x :: _)) -> Some x
  | _ -> None
;;

let fileInfo_name = Ast.Id "FileInfo"

let fileInfo_decl =
  let info =
    {|
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
  pars info
;;

let exception_name = Ast.Id "Exception"

let exception_decl =
  let info = {|
        class Exception{} 
    |} in
  pars info
;;
