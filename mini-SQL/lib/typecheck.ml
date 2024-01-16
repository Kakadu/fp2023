module Exec (M : Utils.MONAD_FAIL) = struct
  open M
  open Types
  open Utils

  (* ----- ARITHMETIC OPERATIONS ------ *)

  (* Bool -> Numeric -> Real -> String *)

  let ( ^^ ) x y = String.concat "" [ x; y ]
  let int_of_bool x = if x then 1 else 0
  let float_of_bool x = if x then 1. else 0.

  let ( #+ ) (x : item) (y : item) =
    match x with
    (* INT + ... *)
    | Numeric x ->
      (match y with
       | Numeric y -> return (Numeric (x + y))
       | Real y -> return (Real (float_of_int x +. y))
       | String y -> return (String (string_of_int x ^^ y))
       | Bool y -> return (Numeric (x + int_of_bool y)))
    (* FLOAT + ... *)
    | Real x ->
      (match y with
       | Numeric y -> return (Real (x +. float_of_int y))
       | Real y -> return (Real (x +. y))
       | String y -> return (String (string_of_float x ^^ y))
       | Bool y -> return (Real (x +. float_of_bool y)))
    (* STRING + ... *)
    | String x ->
      (match y with
       | Numeric y -> return (String (x ^^ string_of_int y))
       | Real y -> return (String (x ^^ string_of_float y))
       | String y -> return (String (x ^^ y))
       | Bool y -> return (String (x ^^ string_of_bool y)))
    (* BOOL + ... *)
    | Bool x ->
      (match y with
       | Numeric y -> return (Numeric (int_of_bool x + y))
       | Real y -> return (Real (float_of_bool x +. y))
       | String y -> return (String (string_of_bool x ^^ y))
       | Bool y -> return (Numeric (int_of_bool x + int_of_bool y)))
  ;;

  let ( #- ) (a : item) (b : item) =
    match a with
    (* INT - ... *)
    | Numeric x ->
      (match b with
       | Numeric y -> return (Numeric (x - y))
       | Real y -> return (Real (float_of_int x -. y))
       | String _ -> fail (TypesMismatch (a, "-", b))
       | Bool y -> return (Numeric (x - int_of_bool y)))
    (* FLOAT - ... *)
    | Real x ->
      (match b with
       | Numeric y -> return (Real (x -. float_of_int y))
       | Real y -> return (Real (x -. y))
       | String _ -> fail (TypesMismatch (a, "-", b))
       | Bool y -> return (Real (x -. float_of_bool y)))
    (* STRING - ... *)
    | String _ -> fail (TypesMismatch (a, "-", b))
    (* BOOL - ... *)
    | Bool x ->
      (match b with
       | Numeric y -> return (Numeric (int_of_bool x - y))
       | Real y -> return (Real (float_of_bool x -. y))
       | String _ -> fail (TypesMismatch (a, "-", b))
       | Bool y -> return (Numeric (int_of_bool x - int_of_bool y)))
  ;;

  let ( #* ) (a : item) (b : item) =
    match a with
    (* INT * ... *)
    | Numeric x ->
      (match b with
       | Numeric y -> return (Numeric (x * y))
       | Real y -> return (Real (float_of_int x *. y))
       | String _ -> fail (TypesMismatch (a, "*", b))
       | Bool y -> return (Numeric (x * int_of_bool y)))
    (* FLOAT * ... *)
    | Real x ->
      (match b with
       | Numeric y -> return (Real (x *. float_of_int y))
       | Real y -> return (Real (x *. y))
       | String _ -> fail (TypesMismatch (a, "*", b))
       | Bool y -> return (Real (x *. float_of_bool y)))
    (* STRING * ... *)
    | String _ -> fail (TypesMismatch (a, "*", b))
    (* BOOL * ... *)
    | Bool x ->
      (match b with
       | Numeric y -> return (Numeric (int_of_bool x * y))
       | Real y -> return (Real (float_of_bool x *. y))
       | String _ -> fail (TypesMismatch (a, "*", b))
       | Bool y -> return (Numeric (int_of_bool x * int_of_bool y)))
  ;;

  let is_zero_int (x : int) = if x = 0 then fail DivisionByZero else return x
  let is_zero_float (x : float) = if x = 0. then fail DivisionByZero else return x

  let ( #/ ) (a : item) (b : item) =
    match a with
    (* INT * ... *)
    | Numeric x ->
      (match b with
       | Numeric y -> is_zero_int y >>= fun y -> return (Numeric (x / y))
       | Real y -> is_zero_float y >>= fun y -> return (Real (float_of_int x /. y))
       | String _ -> fail (TypesMismatch (a, "/", b))
       | Bool _ -> fail (TypesMismatch (a, "/", b)))
    (* FLOAT * ... *)
    | Real x ->
      (match b with
       | Numeric y -> is_zero_int y >>= fun y -> return (Real (x /. float_of_int y))
       | Real y -> is_zero_float y >>= fun y -> return (Real (x /. y))
       | String _ -> fail (TypesMismatch (a, "/", b))
       | Bool _ -> fail (TypesMismatch (a, "/", b)))
    (* STRING * ... *)
    | String _ -> fail (TypesMismatch (a, "/", b))
    (* BOOL * ... *)
    | Bool _ -> fail (TypesMismatch (a, "/", b))
  ;;

  let ( #% ) (a : item) (b : item) =
    match a with
    (* INT * ... *)
    | Numeric x ->
      (match b with
       | Numeric y -> is_zero_int y >>= fun y -> return (Numeric (x mod y))
       | Real y ->
         is_zero_float y >>= fun y -> return (Real (mod_float (float_of_int x) y))
       | String _ -> fail (TypesMismatch (a, "%", b))
       | Bool _ -> fail (TypesMismatch (a, "%", b)))
    (* FLOAT * ... *)
    | Real x ->
      (match b with
       | Numeric y ->
         is_zero_int y >>= fun y -> return (Real (mod_float x (float_of_int y)))
       | Real y -> is_zero_float y >>= fun y -> return (Real (mod_float x y))
       | String _ -> fail (TypesMismatch (a, "%", b))
       | Bool _ -> fail (TypesMismatch (a, "%", b)))
    (* STRING * ... *)
    | String _ -> fail (TypesMismatch (a, "%", b))
    (* BOOL * ... *)
    | Bool _ -> fail (TypesMismatch (a, "%", b))
  ;;

  let ( #= ) (x : item) (y : item) =
    match x with
    (* INT == ... *)
    | Numeric x ->
      (match y with
       | Numeric y -> return (x = y)
       | Real y -> return (float_of_int x = y)
       | String y -> return (string_of_int x = y)
       | Bool y -> return (x = int_of_bool y))
    (* FLOAT == ... *)
    | Real x ->
      (match y with
       | Numeric y -> return (x = float_of_int y)
       | Real y -> return (x = y)
       | String y -> return (string_of_float x = y)
       | Bool y -> return (x = float_of_bool y))
    (* STRING == ... *)
    | String x ->
      (match y with
       | Numeric y -> return (x = string_of_int y)
       | Real y -> return (x = string_of_float y)
       | String y -> return (x = y)
       | Bool y -> return (x = string_of_bool y))
    (* BOOL == ... *)
    | Bool x ->
      (match y with
       | Numeric y -> return (int_of_bool x = y)
       | Real y -> return (float_of_bool x = y)
       | String y -> return (string_of_bool x = y)
       | Bool y -> return (int_of_bool x = int_of_bool y))
  ;;

  let ( #> ) (x : item) (y : item) =
    match x with
    (* INT == ... *)
    | Numeric x ->
      (match y with
       | Numeric y -> return (x > y)
       | Real y -> return (float_of_int x > y)
       | String y -> return (string_of_int x > y)
       | Bool y -> return (x > int_of_bool y))
    (* FLOAT == ... *)
    | Real x ->
      (match y with
       | Numeric y -> return (x > float_of_int y)
       | Real y -> return (x > y)
       | String y -> return (string_of_float x > y)
       | Bool y -> return (x > float_of_bool y))
    (* STRING == ... *)
    | String x ->
      (match y with
       | Numeric y -> return (x > string_of_int y)
       | Real y -> return (x > string_of_float y)
       | String y -> return (x > y)
       | Bool y -> return (x > string_of_bool y))
    (* BOOL == ... *)
    | Bool x ->
      (match y with
       | Numeric y -> return (int_of_bool x > y)
       | Real y -> return (float_of_bool x > y)
       | String y -> return (string_of_bool x > y)
       | Bool y -> return (int_of_bool x > int_of_bool y))
  ;;

  let ( #< ) (x : item) (y : item) =
    match x with
    (* INT == ... *)
    | Numeric x ->
      (match y with
       | Numeric y -> return (x < y)
       | Real y -> return (float_of_int x < y)
       | String y -> return (string_of_int x < y)
       | Bool y -> return (x < int_of_bool y))
    (* FLOAT == ... *)
    | Real x ->
      (match y with
       | Numeric y -> return (x < float_of_int y)
       | Real y -> return (x < y)
       | String y -> return (string_of_float x < y)
       | Bool y -> return (x < float_of_bool y))
    (* STRING == ... *)
    | String x ->
      (match y with
       | Numeric y -> return (x < string_of_int y)
       | Real y -> return (x < string_of_float y)
       | String y -> return (x < y)
       | Bool y -> return (x < string_of_bool y))
    (* BOOL == ... *)
    | Bool x ->
      (match y with
       | Numeric y -> return (int_of_bool x < y)
       | Real y -> return (float_of_bool x < y)
       | String y -> return (string_of_bool x < y)
       | Bool y -> return (int_of_bool x < int_of_bool y))
  ;;

  let ( #!= ) (x : item) (y : item) = x #= y >>= fun res -> return (not res)
  let ( #>= ) (x : item) (y : item) = x #< y >>= fun res -> return (not res)
  let ( #<= ) (x : item) (y : item) = x #> y >>= fun res -> return (not res)

  let bool_of_item a =
    match a with
    | Bool x -> return x
    | Numeric x -> if x > 0 then return true else return false
    | Real x -> if x > 0. then return true else return false
    | String x ->
      (match bool_of_string_opt x with
       | Some x -> return x
       | None -> fail (TypeConversionFail (a, "Bool")))
  ;;

  (** Get current item from sheet *)
end
