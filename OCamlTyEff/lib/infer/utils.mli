open! Base
open Monads.Std

(** Makes Reader-Writer-State monad *)
module MakeRWSMonad : functor
  (ReaderT : T)
  (WriterT : Monoid.S)
  (StateT : T)
  -> sig
  include Monad.S

  val run : 'a t -> ReaderT.t -> StateT.t -> 'a * WriterT.t * StateT.t

  module Reader : sig
    val read : ReaderT.t t

    val local : (ReaderT.t -> ReaderT.t) -> 'a t -> 'a t
    (** Executes a computation in a modified environment *)
  end

  module Writer : sig
    val write : WriterT.t -> unit t
  end

  module State : sig
    val get : StateT.t t

    val put : StateT.t -> unit t
  end
end
