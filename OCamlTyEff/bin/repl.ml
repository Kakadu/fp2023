(** Copyright 2023-2024, Danil S, Andrei *)

(** SPDX-License-Identifier: MIT *)

open! Base
open Stdio
open Lwt
open Misc

open LTerm_text
open LTerm_style

let ( let* ) x f = bind x f

let history_filepath = ".ocamltyeff_history"

module Command = struct
  type t =
    | Help  (** Show help message *)
    | Quit  (** Quit REPL *)
    | Break  (** Ctrl-c *)
    | Rectypes  (** Switch recursive types *)
    | Load of string  (** Evaluate source code from file *)
    | Eval of string  (** Evaluate the given string *)

  let parse = function
    | str when String.equal str ":help" ->
        Some Help
    | str when String.equal str ":q" ->
        Some Quit
    | str when String.equal str ":rectypes" ->
        Some Rectypes
    | str when String.is_prefix str ~prefix:":load" ->
        let filepath =
          String.lstrip @@ String.chop_prefix_exn str ~prefix:":load"
        in
        Some (Load filepath)
    | str when Zed_utf8.contains str ";;" ->
        Some (Eval str)
    | _ ->
        None
end

let help_message =
  eval
    [ B_fg lcyan
    ; S "<expression>"
    ; E_fg
    ; S " - evaluate the given expression\n"
    ; B_fg lcyan
    ; S ":load <filepath>"
    ; E_fg
    ; S " - load source code from file\n"
    ; B_fg lcyan
    ; S ":rectypes"
    ; E_fg
    ; S " - (experimental) enable / disable recursive types\n"
    ; B_fg lcyan
    ; S ":q"
    ; E_fg
    ; S " - quit" ]

let eval_command ~term ~rec_types env = function
  | Command.Eval str ->
      let* env = Interpret.interpret ~term ~rec_types env str in
      return @@ Some (rec_types, env)
  | Load filepath ->
      let* env =
        try
          let code = In_channel.read_all filepath in
          Interpret.interpret ~term ~rec_types env code
        with Sys_error err ->
          let* () =
            LTerm.fprintls term @@ eval [B_fg lcyan; S "Error. "; S err; E_fg]
          in
          return env
      in
      return @@ Some (rec_types, env)
  | Rectypes ->
      let rec_types = not rec_types in
      let msg =
        Format.sprintf "Recursive types %s" (if rec_types then "on" else "off")
      in
      let* () = LTerm.fprintl term msg in
      return @@ Some (rec_types, env)
  | Help ->
      let* () = LTerm.fprintls term help_message in
      return @@ Some (rec_types, env)
  | Break ->
      let* () =
        LTerm.fprintls term @@ eval [B_fg lcyan; S "Interrupted"; E_fg]
      in
      return @@ Some (rec_types, env)
  | Quit ->
      return None

let prompt = eval [B_fg lgreen; S "\n# "; E_fg]

class read_line ~term ~history ~completion_ids =
  object (self)
    inherit
      [Command.t] LTerm_read_line.engine
        ~history:(LTerm_history.contents history)
        ()
    inherit [Command.t] LTerm_read_line.term term as super_term

    initializer self#set_prompt (React.S.const prompt)

    method eval = Break

    method! completion =
      let input = Zed_string.to_utf8 @@ Zed_rope.to_string self#input_prev in
      let cur_word =
        List.last_exn @@ String.split_on_chars input ~on:[' '; '\n']
      in
      let cur_word_index =
        List.last_exn
        @@ String.substr_index_all input ~may_overlap:false ~pattern:cur_word
      in

      let completion_ids =
        List.filter completion_ids ~f:(fun id ->
            String.is_prefix ~prefix:cur_word id )
      in
      self#set_completion cur_word_index
      @@ List.map completion_ids ~f:(fun id ->
             (Zed_string.of_utf8 id, Zed_string.empty ()) )

    method! exec ?keys =
      function
      | Accept :: actions -> (
          Zed_macro.add self#macro Accept ;

          let input = Zed_rope.to_string @@ Zed_edit.text self#edit in
          match Command.parse (Zed_string.to_utf8 input) with
          | Some cmd ->
              LTerm_history.add history input ;
              return @@ LTerm_read_line.Result cmd
          | None ->
              (* continue input *)
              self#insert (Uchar.of_char '\n') ;
              self#exec ?keys actions )
      | Break :: _ ->
          Zed_macro.add self#macro Break ;
          return @@ LTerm_read_line.Result Command.Break
      | actions ->
          super_term#exec ?keys actions
  end

let rec repl_loop ~term ~history ~rec_types ((ty_env, _) as env) =
  let completion_ids =
    Types.Env.idents ty_env |> List.map ~f:(fun (Ident.Ident name) -> name)
  in
  let rl = new read_line ~term ~history ~completion_ids in
  rl#run
  >>= eval_command ~term ~rec_types env
  >>= function
  | None ->
      return ()
  | Some (new_rec, new_env) ->
      repl_loop ~term ~history ~rec_types:new_rec new_env

let eval_str ~term ~rec_types env str =
  let* (_ : bool * Interpret.env * string) =
    Lwt_list.fold_left_s
      (fun (rec_types, env, str_buf) line ->
        match Command.parse (str_buf ^ line) with
        | None ->
            return (rec_types, env, str_buf ^ line)
        | Some cmd ->
            let* res = eval_command ~term ~rec_types env cmd in
            let* () = LTerm.fprint term "\n" in
            let rec_types, env =
              Option.value_map res ~default:(rec_types, env) ~f:Fun.id
            in
            return (rec_types, env, "") )
      (rec_types, env, "") (String.split_lines str)
  in
  return ()

let greeting =
  eval [B_fg lcyan; S "Welcome to OcamlTyEff. Type :help for help\n"; E_fg]

let run_repl ~term =
  let history = LTerm_history.create ~max_entries:1024 [] in
  let* () = LTerm_history.load history history_filepath in
  let* () = LTerm_inputrc.load () in

  let* () = LTerm.fprints term greeting in
  let* () =
    Lwt.catch
      (fun () -> repl_loop ~term ~history ~rec_types:false Interpret.std_env)
      (function LTerm_read_line.Interrupt -> return () | exn -> fail exn)
  in

  LTerm_history.save history history_filepath

let eval_stdin ~term =
  let code = In_channel.input_all stdin in
  eval_str ~term ~rec_types:false Interpret.std_env code

let main =
  let* term = force LTerm.stdout in
  if LTerm.is_a_tty term then run_repl ~term else eval_stdin ~term

let () = Lwt_main.run main
