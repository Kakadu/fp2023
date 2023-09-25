#use "topfind";;
#require "unix";;
#require "str";;

let red = "\027[0;31m"
let no_color = "\027[0m"

let log fmt = Format.kasprintf (fun s -> if false then print_endline s ) fmt
let get_output fmt =
  Format.kasprintf (fun cmd ->
      log "Running: %s%s%s" red cmd no_color;
      let ch = Unix.open_process_in cmd in
      let s = In_channel.input_all ch in
      let () = In_channel.close ch in
      s)
    fmt
;;

open Printf

(* let () = print_endline "hello world" *)

let () =
  let s = get_output "git remote | grep upstream" in
  log "%S" s;
  if not (List.mem "upstream" @@ Str.split (Str.regexp "\n") s) then (
    let _ = Sys.command "git remote add upstream https://github.com/Kakadu/fp2023.git" in
    let _ = Sys.command "git fetch upstream master" in
    Format.eprintf "Upstream added\n%!";
    ()
  )


let user_branch =
  if Array.length Sys.argv < 2
  then "master" else Sys.argv.(1)

let merge_base =
  let s = get_output "git merge-base upstream/master %s" (* user_branch *) "HEAD" in
  match Str.split (Str.regexp "\n") s |> List.filter ((<>)"") with
  | [h] -> h
  | xs ->
      Format.eprintf "After merge-base got a list of length %d\n%!" (List.length xs);
      Format.eprintf "[ %s ]\n%!" (String.concat "; " xs);
      exit 1

let () = log "%S " merge_base

let calculate_common_subdir files =
  let module SS = Set.Make(String) in
  let get_top_dir name =
    let prefix_len =
      match (String.index name '/') with
      | exception Not_found -> String.length name
      | n  -> n
    in
    String.sub name 0 prefix_len
  in
  let set = List.fold_right (fun x acc -> SS.add (get_top_dir x) acc) files SS.empty in
  SS.to_seq set |> List.of_seq

let pp_str_list ppf xs =
  Format.fprintf ppf "[ %s ]" (String.concat ", " xs)
(*  *)
let () =
  let s =
    get_output "git diff-tree %s..%s  | rev | cut -f 1 | rev" merge_base user_branch in
  log "%S " s;
  let changed_files =
    String.split_on_char '\n' s |> List.filter (function
      | "" -> false
      | s when not (Sys.is_directory s) && Filename.dirname s = Filename.current_dir_name -> false
      | _ -> true
      )
    in
  log "%s " (String.concat ", " changed_files);
  match changed_files with
  | [] -> Format.eprintf "No changed files.\n%!"; exit 1
  | xs ->
      match calculate_common_subdir xs with
      | [] -> assert false
      | [h] -> Format.printf "latest=%s\n" h; exit 0
      | ds -> Format.eprintf "Too many directories has been changed: %a\n%!" pp_str_list ds; exit 1
