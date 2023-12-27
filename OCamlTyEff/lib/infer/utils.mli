(** Copyright 2023, Danil S, Andrei *)

(** SPDX-License-Identifier: MIT *)

open! Base
open Monads.Std

(** Makes Reader-Writer-State-Error monad *)
module MakeRWSEMonad : functor
  (ReaderT : T)
  (WriterT : Monoid.S)
  (StateT : T)
  (ErrorT : T)
  -> sig
  include Monad.S

  val run :
       'a t
    -> ReaderT.t
    -> StateT.t
    -> ('a * WriterT.t * StateT.t, ErrorT.t) result

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

  module Error : sig
    val fail : ErrorT.t -> 'a t
  end
end

(** Makes State-Error monad *)
module MakeSEMonad : functor (StateT : T) (ErrorT : T) -> sig
  include Monad.S

  val run : 'a t -> StateT.t -> ('a * StateT.t, ErrorT.t) result

  module State : sig
    val get : StateT.t t
    val put : StateT.t -> unit t
  end

  module Error : sig
    val fail : ErrorT.t -> 'a t
  end
end
