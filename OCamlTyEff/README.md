# OCamlTyEff

Interpreter for a subset of `OCaml` with support for typed effects.

`OCamlTyEff` tracks the (side) effects of every function and represents them in the function’s type.

![demo](https://github.com/therain7/OCamlTyEff/assets/15161335/8ed130f9-d230-418d-852d-1697067ec14d)

## Effects

`OCamlTyEff` has 3 basic built-in effects: `console`, `exn`, `ref`. Intepreter automatically infers these effects if they occur in a function:

```ocaml
# fun () -> print_endline "Hello, World!";;
- : unit -[console]-> unit = <fun>

# fun x -> if x = 0 then raise Invalid_argument else x * 2;;
- : int -[exn _Invalid_argument]-> int = <fun>

# fun x -> global := x; 8 / x;;
- : int -[ref, exn _Division_by_zero]-> int = <fun>
```

`exn` effect can be lifted using `try ... with`:

```ocaml
# ( / );;
- : int -> int -[exn _Division_by_zero]-> int = <fun>

# let safe_div x y = try x / y with Division_by_zero -> 0;;
safe_div : int -> int -> int = <fun>
```

<br />

Many functions are polymorphic in their effect.

For example, `list_map` applies provided function to each element of a list. As such, the effect of `list_map` depends on the effect of provided function:

```ocaml
# list_map;;
- : ('a -'e-> 'b) -> 'a list -'e-> 'b list = <fun>

# list_map id;;
- : 'a list -> 'a list = <fun>

# list_map print_endline;;
- : string list -[console]-> unit list = <fun>
```

<br />

The type system for effects is based on [Koka](https://koka-lang.github.io/koka/doc/index.html) programming language. Please refer to its [documentation](https://koka-lang.github.io/koka/doc/book.html#sec-effect-types) for more information on effect types.

## Features

Take a look at provided [examples](examples.t) to get a grasp of what's supported.

## Run locally

First install the dependencies using `opam`:

```bash
opam install --deps-only -t -y .
```

Then build and run REPL using `dune`:

```bash
dune exec repl
```

<br />

Tests can be run by executing:

```bash
dune runtest
```

## License

Distributed under the MIT License. See [LICENSE](LICENSE) for more information.
