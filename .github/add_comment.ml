#use "topfind";;
#require "curly";;
#require "yojson";;
#require "str";;

type filename = string
type action =
  | Put_comment of filename
  | Remove_comment of string
  | Delete_comment_by_id of int
  | No_action
type config =
  { mutable action: action
  ; mutable token: string
  ; mutable issue: int
  ; mutable user: string
  ; mutable repo: string
  }
let config =
  let c = { action=No_action; token=""; issue = 0; user="Kakadu"; repo="comp23hw" } in
  Arg.parse
    [ ("-file", Arg.String (fun s -> c.action <- Put_comment s), " input file ")
    ; ("-remove", Arg.String (fun s -> c.action <- Remove_comment s), " remove comments with text")
    ; ("-delete-comment", Arg.Int(fun n -> c.action <- Delete_comment_by_id n), " delete comment by id")
    ; ("-token", Arg.String (fun s -> c.token <- s ), " access token")
    ; ("-issue", Arg.Int (fun s -> c.issue <- s ), " issue/PR number")
    ; ("-user", Arg.String (fun s -> c.user <- s ), " username")
    ; ("-repo", Arg.String (fun s -> c.repo <- s ), " repo")
    ]
    (fun _ -> assert false)
    "";
  c

let log fmt =
  Format.kasprintf (print_endline) fmt

let headers =
  [ "Authorization", Printf.sprintf "Bearer %s" config.token
  ; "Accept", "application/vnd.github+json"
  ; "X-GitHub-Api-Version", "2022-11-28"
  ]

let add_comment ~filename =
  let url =
    Printf.sprintf
      "https://api.github.com/repos/%s/%s/issues/%d/comments"
      config.user
      config.repo
      config.issue
  in
  let data = In_channel.with_open_text filename (fun ch ->
    let s = In_channel.input_all ch in
    s
    (* |> Str.global_replace (Str.regexp "\n") "\\n" *)
    (* |> Str.global_replace (Str.regexp "'") "'\\''" *)
    )
  in
  let body =
    `Assoc [ "body", `String data ]
    |> Yojson.Safe.pretty_to_string
  in
  (match Curly.(run (Request.make ~body ~headers ~url ~meth:`POST ())) with
    | Ok ({ Curly.Response.code = 401; _ } as x) -> (* Bad credentials *)
      Format.printf "status: %d\n" x.Curly.Response.code;
      Format.printf "body: %s\n" x.Curly.Response.body;
      exit 1
    | Ok x ->
      Format.printf "status: %d\n" x.Curly.Response.code;
      Format.printf "body: %s\n" x.Curly.Response.body;
      true
    | Error e ->
      Format.eprintf "Failed: %a" Curly.Error.pp e;
      false)

let make_comment_info id ~user body =
  (id, user, body)
let assoc_string name j =
  match j with
  | `Assoc xs -> (match List.assoc name xs with `String s -> s | _ -> assert false)
  | _ -> assert false

let assoc_int name j =
  match (j: Yojson.Safe.t) with
  | `Assoc xs -> (match List.assoc name xs with `Int s -> s | _ -> assert false)
  | _ -> assert false

let find_comments () =
  let url =
    Printf.sprintf
      "https://api.github.com/repos/%s/%s/issues/%d/comments"
      config.user
      config.repo
      config.issue
  in
  (match Curly.(get ~headers url) with
    | Error e ->
      Format.eprintf "Failed: %a" Curly.Error.pp e;
      Result.error()
    | Ok x ->
      Format.printf "status: %d\n" x.Curly.Response.code;
      Format.printf "body: %s\n" x.Curly.Response.body;
      let j = Yojson.Safe.from_string x.Curly.Response.body in
      let js = match j with
        | `List js -> js
        | _ -> assert false in
      Result.Ok (
        js |> List.map (function
          | (`Assoc kvs as j) ->
            make_comment_info
              (assoc_int "id" j)
              (assoc_string "body" j)
              ~user:(match List.assoc "user" kvs with
              | (`Assoc kvs) as j-> assoc_string "login" j
              | _ -> assert false)
          | _ -> assert false
          )
      )
    )


let delete_comment comment_id =
  let url =
    Printf.sprintf
      "https://api.github.com/repos/%s/%s/issues/comments/%d"
      config.user
      config.repo
      comment_id
  in
  log "Delete using url %S." url;
  (match Curly.(delete ~headers url) with
    | Error e ->
      Format.eprintf "Failed: %a" Curly.Error.pp e;
      false
    | Ok x ->
      Format.printf "status: %d\n" x.Curly.Response.code;
      true)

let () =
  let (let*) = Result.bind in
  ignore (match config.action with
  | No_action -> Printf.eprintf "No action specified\n"; exit 1
  | Put_comment filename ->
      assert (add_comment ~filename);
      Result.ok ()
  | Delete_comment_by_id n ->
      assert (delete_comment n);
      Result.ok ()
  | Remove_comment text ->
    if (String.length text = 0)
    then failwith "text for search can't be empty";
    let* all_comments = find_comments () in
    let __ () = List.iter (fun (id, user, body) ->
      Printf.printf "%d\n%s\n%s\n\n" id user body
      ) all_comments
    in
    let ids_to_remove =
      List.filter_map (fun (id, _, body) ->
          match Str.search_forward (Str.regexp_string text) body 0 with
          | _ -> Some id
          | exception Not_found -> None
        ) all_comments in
    List.iter (fun n ->
      Printf.printf "removing %d...\n" n;
      assert(delete_comment n);
    ) ids_to_remove;
    Result.ok ())
