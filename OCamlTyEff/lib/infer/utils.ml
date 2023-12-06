open! Base
open Monads.Std

module MakeRWSMonad (ReaderT : T) (WriterT : Monoid.S) (StateT : T) = struct
  module StateM = struct
    include Monad.State.T1 (StateT) (Monad.Ident)
    include Monad.State.Make (StateT) (Monad.Ident)
  end

  module WriterM = struct
    include Monad.Writer.T1 (WriterT) (StateM)
    include Monad.Writer.Make (WriterT) (StateM)
  end

  include Monad.Reader.T1 (ReaderT) (WriterM)
  include Monad.Reader.Make (ReaderT) (WriterM)

  module Reader = struct
    let read = read ()

    let local f m =
      let* cur = read in
      lift @@ run m (f cur)
  end

  module Writer = struct
    let write x = lift @@ WriterM.write x
  end

  module State = struct
    let get = lift @@ WriterM.lift @@ StateM.get ()

    let put x = lift @@ WriterM.lift @@ StateM.put x
  end

  let run m reader_env init_state =
    let r = run m reader_env in
    let r = WriterM.run r in
    let (ret, writer_state), state = StateM.run r init_state in
    (ret, writer_state, state)
end

module MakeSEMonad (StateT : T) (ErrorT : T) = struct
  module ErrorM = struct
    include Monad.Result.T1 (ErrorT) (Monad.Ident)
    include Monad.Result.Make (ErrorT) (Monad.Ident)
  end

  include Monad.State.T1 (StateT) (ErrorM)
  include Monad.State.Make (StateT) (ErrorM)

  module State = struct
    let get = get ()

    let put = put
  end

  module Error = struct
    let fail err = lift @@ ErrorM.fail err
  end

  let run m init_state =
    let r = run m init_state in
    ErrorM.run r
end
