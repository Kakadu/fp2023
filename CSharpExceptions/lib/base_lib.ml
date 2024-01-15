(** Copyright 2021-2023, Georgy Sichkar *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Errors
open Common_types
open Monads.Eval_Monad

let pars_class str =
  match Parser.parse_ast str with
  | Result.Ok (Ast (x :: _)) -> Some x
  | _ -> None
;;

let pars_ast str =
  match Parser.parse_ast str with
  | Result.Ok x -> Some x
  | _ -> None
;;

module File_Info = struct
  let cl_name = Code_ident (Id "FileInfo")

  (** Open file *)
  let constr_name = Id "FileInfo"

  let open_out_FileInfo path = return_n @@ open_out path

  (** Add in file *)
  let append_name = Id "AppendAllText"

  let appendAllText oc msg = Printf.fprintf oc "%s" msg |> return_n

  (** Close file *)
  let close_name = Id "CloseFile"

  let closeFile = close_out |> return_n

  (* -_- *)
  let get_base_lib_method id cl_decl = find_cl_decl_meth id cl_decl

  let read_fl_descriptors =
    read_smem
    >>= fun info ->
    match Sys_Map.find_opt Fl_descriptors info with
    | Some (Files (id, info)) -> return_n (id, info)
    | _ -> fail (System_error "The file was not opened for writing")
  ;;

  let save_fl_descriptors new_discrs =
    read_smem >>| fun info -> Sys_Map.add Fl_descriptors new_discrs info |> fun _ -> ()
  ;;

  let read_out_channel i_ad =
    read_fl_descriptors
    >>= fun (_, info) ->
    match Intern_Mem.find_opt i_ad info with
    | Some (W_File x) -> return_n x
    | _ -> fail (Runtime_error "File descriptors are not initialized")
  ;;

  let append_out_channel oc =
    read_fl_descriptors
    >>= fun (i_ad, info) ->
    save_fl_descriptors (Files (i_incr_ i_ad, Intern_Mem.add i_ad (W_File oc) info))
  ;;
end

let exception_name = Id "Exception"

let exception_decl =
  let info =
    {|
        class Exception : Exception{
          Exception(){}
        } 
    |}
  in
  pars_class info
;;

let base_lib_decl =
  let info =
    {|
      class FileInfo{
        public int UID;

        public string path;
        public bool Exists = false;
        public bool Opened = false;

        FileInfo(string path_){}

        void CloseFile() {}

        void AppendAllText(string info) {}

        } 
    |}
  in
  pars_ast info
;;
