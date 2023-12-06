open! Base
open Types
open Constraints
open Ast
open GenMonad.Gen
open GenMonad.Let_syntax
open GenMonad.Let
module As = Assumptions

let type_of_constant = function
  | Const_integer _ ->
      Ty.int
  | Const_char _ ->
      Ty.char
  | Const_string _ ->
      Ty.string

let rec gen_expr =
  let ( ! ) tv = Ty.Ty_var tv in
  let ( @> ) ty_arg ty_res = Ty.Ty_arr (ty_arg, ty_res) in
  let ( == ) t1 t2 = Constr.EqConstr (t1, t2) in
  let ( ++ ) = As.merge in
  let ( -- ) asm = List.fold ~init:asm ~f:As.remove in
  function
  | Exp_ident id ->
      let* var = fresh_var in
      return (As.singleton id var, !var)
  | Exp_constant const ->
      return (As.empty, type_of_constant const)
  | Exp_fun (args, e) ->
      (* for now convert args to ident list *)
      let args =
        List.map args ~f:(function
          | Pat_var x ->
              (* XXX: why do we create Ident here, not in parser? *)
              Ident.Ident x
          | _ ->
              failwith "not implemented" )
      in
      let* args_vars = GenMonad.List.map args ~f:(fun _ -> fresh_var) in
      let* as_e, ty_e = extend_varset args_vars (gen_expr e) in
      let ty_args = List.map args_vars ~f:( ! ) in
      (* for each arg: tv from assumptions == fresh tv created above *)
      let* () =
        add_constrs
          ( List.map2_exn args ty_args ~f:(fun arg ty_arg ->
                As.lookup as_e arg |> List.map ~f:(fun var -> ty_arg == !var) )
          |> List.concat_no_order )
      in
      let ty_res = List.fold_right ty_args ~init:ty_e ~f:( @> ) in
      return (as_e -- args, ty_res)
  | Exp_apply (e_fun, e_arg) ->
      let* as_fun, ty_fun = gen_expr e_fun in
      let* as_arg, ty_arg = gen_expr e_arg in
      let* ty_res = fresh_var >>| ( ! ) in
      let* () = add_constrs [ty_fun == ty_arg @> ty_res] in
      return (as_fun ++ as_arg, ty_res)
  | Exp_let (Nonrecursive, bindings, e2) ->
      let name, e1 =
        match bindings with
        | [{pat= Pat_var name; expr= e1}] ->
            (Ident.Ident name, e1)
        | _ ->
            failwith "not implemented"
      in
      let* as1, ty1 = gen_expr e1 in
      let* as2, ty2 = gen_expr e2 in
      let* mset = varset in
      let* () =
        add_constrs
          ( As.lookup as2 name
          |> List.map ~f:(fun var -> Constr.ImplInstConstr (!var, mset, ty1)) )
      in
      return (as1 ++ (as2 -- [name]), ty2)
  | Exp_ifthenelse (e_cond, e_th, e_el) ->
      let* as_cond, ty_cond = gen_expr e_cond in
      let* as_th, ty_th = gen_expr e_th in
      let* as_el, ty_el =
        match e_el with
        | None ->
            return (As.empty, Ty.unit)
        | Some e ->
            gen_expr e
      in
      let* () = add_constrs [ty_cond == Ty.bool; ty_th == ty_el] in
      return (as_cond ++ as_th ++ as_el, ty_th)
  | _ ->
      failwith "not implemented"
