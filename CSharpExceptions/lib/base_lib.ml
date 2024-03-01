(** Copyright 2021-2023, Georgy Sichkar *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Errors
open Common_types
open Interpret_converters
open Monads.Eval

let main_method = Id "Main"

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

module type UNIT_METHOD = sig
  val tp : meth_type
  val name : ident
  val run : (unit, 'a) t
end

module File_Info = struct
  let name = Code_ident (Id "FileInfo")

  module System_mem_internal_functions = struct
    let read_fl_descriptors =
      read_smem
      >>= fun info ->
      match Sys_Map.find_opt Fl_descriptors info with
      | Some (Files (id, info)) -> return_n (id, info)
      | _ -> fail (Interpret_error (System_error "The file was not opened for writing!!"))
    ;;

    let save_fl_descriptors new_discrs =
      read_smem
      >>= fun info -> return_n (Sys_Map.add Fl_descriptors new_discrs info) >>= save_smem
    ;;

    let read_out_channel i_ad =
      read_fl_descriptors
      >>= fun (_, info) ->
      match Intern_Mem.find_opt i_ad info with
      | Some (W_File x) -> return_n x
      | _ -> fail (Interpret_error (Runtime_error "File descriptors are not initialized"))
    ;;

    let append_out_channel oc =
      read_fl_descriptors
      >>= fun (i_ad, info) ->
      save_fl_descriptors (Files (i_incr_ i_ad, Intern_Mem.add i_ad (W_File oc) info))
      *> return_n i_ad
    ;;

    let read_out_channel_from_self intern_fild_name =
      read_self_ad
      >>= read_inst_el intern_fild_name
      >>= get_int
      >>| (fun x -> ILink x)
      >>= read_out_channel
    ;;
  end

  module Fields = struct
    let internal_address_name = Id "UID"
    let path = Id "path"
    let state = Id "Opened"
  end

  module Methods = struct
    open Fields
    open System_mem_internal_functions

    module Open_file = struct
      (** Run at the same time with constructor *)
      let name = Id "FileInfo"

      let run path = return_n @@ open_out path
    end

    module Write_in_file : UNIT_METHOD = struct
      let tp = Void
      let name = Id "AppendAllText"
      let string_name = Id "info"

      let run =
        let oc_t = read_out_channel_from_self internal_address_name in
        let msg_t = read_local_el string_name >>= get_string in
        oc_t
        >>= fun oc ->
        msg_t
        >>= fun msg ->
        try Printf.fprintf oc "%s" msg |> fun _ -> return_n () with
        | Sys_error msg -> fail (Interpret_error (System_error msg))
      ;;
    end

    module Close_file : UNIT_METHOD = struct
      let tp = Void
      let name = Id "CloseFile"
      let run = read_out_channel_from_self internal_address_name >>| fun x -> close_out x
    end

    module After_constructor_handler = struct
      let run ad =
        read_inst_el path ad
        >>= get_string
        >>= Open_file.run
        >>= append_out_channel
        >>= fun (ILink x) -> ret_int x >>= update_instance_el internal_address_name ad
      ;;
    end
  end

  let run_method id =
    let open Methods in
    match id with
    | x when equal_ident x Write_in_file.name -> Write_in_file.run
    | x when equal_ident x Close_file.name -> Close_file.run
    | _ ->
      let (Id str) = id in
      fail
        (Interpret_error (Runtime_error ("FileInfo does not contain " ^ str ^ " method")))
  ;;

  module Declaration = struct
    let decl_ast =
      let info =
        {|
          class FileInfo{
            public int UID;
            
            public string path;
            public bool Opened = false;
            
            FileInfo(string path_){
              path = path_;
              Opened = true;
            }
            
            public void CloseFile() {}
            
            public void AppendAllText(string info) {}
            } 
            |}
      in
      pars_class info
    ;;
  end
end

module Exception = struct
  let name = Id "Exception"

  module Declaration = struct
    let decl_ast =
      let info =
        {|
        class Exception : Exception {
          Exception(){}
        } 
        |}
      in
      pars_class info
    ;;
  end
end

let classes_with_system_classes = [ File_Info.name ]

let get_system_method_opt cl_id meth_id =
  List.find_opt (fun x -> equal_code_ident x cl_id) classes_with_system_classes
  |> function
  | Some cl_id ->
    (match cl_id with
     | id when equal_code_ident id File_Info.name -> Some (File_Info.run_method meth_id)
     | _ -> None)
  | None -> None
;;

let get_system_constr_opt cl_id =
  List.find_opt (fun x -> equal_code_ident x cl_id) classes_with_system_classes
  |> function
  | Some cl_id ->
    (match cl_id with
     | id when equal_code_ident id File_Info.name ->
       Some File_Info.Methods.After_constructor_handler.run
     | _ -> None)
  | None -> None
;;

let run_sys_constructor_or_normal f cl_id =
  f
  >>= fun ad ->
  (get_system_constr_opt cl_id
   |> function
   | Some f -> f ad
   | None -> return_n ())
  *> return_n ad
;;

let base_lib_decl =
  let base_lib_decl_ =
    [ File_Info.Declaration.decl_ast; Exception.Declaration.decl_ast ]
  in
  let f acc = function
    | Some x -> x :: acc
    | None -> acc
  in
  match List.fold_left f [] base_lib_decl_ with
  | [] -> None
  | x -> Some (Ast x)
;;
