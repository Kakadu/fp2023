open Base

module type STATE = sig
  type t
end

module STATE_MONAD (State : STATE) : sig
  (* include MONAD *)
  include Base.Monad.Infix

  val return : 'a -> 'a t
  val run : 'a t -> init:State.t -> State.t * 'a
  val put : State.t -> unit t
  val get : State.t t
  val many : 'a list -> f:('a -> 'b t) -> 'b list t
end = struct
  type 'a t = State.t -> State.t * 'a

  let run t ~init = t init

  let ( >>= ) m f s =
    let s, t = m s in
    let res = f t in
    res s
  ;;

  let return s a = a, s
  let put s _ = s, ()
  let get s = s, s

  let rec many lst ~f =
    match lst with
    | [] -> return []
    | hd :: tl -> f hd >>= fun h -> many tl ~f >>= fun rest -> return (h :: rest)
  ;;

  let ( >>| ) m f = m >>= fun x -> return (f x)
end
