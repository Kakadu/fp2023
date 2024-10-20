(* open Base
open Lambda_lib

let run_repl _ =
  Stdlib.Format.eprintf "OCaml-style toplevel (ocamlc, utop) is not implemented"
;;

let run_single eval =
  let open Lambda_lib in
  let text = Stdio.In_channel.(input_all stdin) |> String.rstrip in
  let ast = Parser.parse text in
  match ast with
  | Error e -> Stdlib.Format.printf "Error: %a\n%!" Parser.pp_error e
  | Result.Ok ast ->
    Stdlib.Format.printf "Parsed result: %a\n%!" Printast.pp_named ast;
    (match eval ast with
     | rez -> Stdlib.Format.printf "Evaluated result: %a\n%!" Printast.pp_named rez)
;;

type strategy =
  | CBN
  | CBV
  | NO
  | AO

type opts =
  { mutable batch : bool
  ; mutable stra : strategy
  }

let () =
  let opts = { batch = false; stra = CBN } in
  let open Stdlib.Arg in
  parse
    [ ( "-"
      , Unit (fun () -> opts.batch <- true)
      , "Read from stdin single program, instead of running full REPL" )
    ; "-cbv", Unit (fun () -> opts.stra <- CBV), "Call-by-value strategy"
    ; "-cbn", Unit (fun () -> opts.stra <- CBN), "Call-by-name strategy"
    ; "-no", Unit (fun () -> opts.stra <- NO), "Normal Order strategy"
    ; "-ao", Unit (fun () -> opts.stra <- NO), "Applicative Order strategy"
    ]
    (fun _ ->
      Stdlib.Format.eprintf "Positioned arguments are not supported\n";
      Stdlib.exit 1)
    "Read-Eval-Print-Loop for Utyped Lambda Calculus";
  let eval =
    Lambda.apply_strat
      (match opts.stra with
       | NO -> Lambda.nor_strat
       | CBV -> Lambda.cbv_strat
       | AO -> Lambda.ao_strat
       | CBN -> Lambda.cbn_strat)
  in
  (if opts.batch then run_single else run_repl) eval
;;
*)
let _ = Python_Lib.Interpreter.Result.( >>= )
let _ = Python_Lib.Interpreter.Result.error
let _ = Python_Lib.Interpreter.Result.return
let _ = Python_Lib.Parser.pyParser
