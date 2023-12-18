# OCamlTyEff

Interpreter for a subset of OCaml language with support for typed effects

## WIP

The project is very much in a WIP state.

To learn more about implementation details and to see some features in action please refer to [parser](lib/parse) & [type inference](lib/infer) READMEs.

## Build & run tests

First install depedencies using opam:

```bash
opam install -y --deps-only .
```

Then build the project and run tests:

```bash
dune build
dune runtest
```

## License

Distributed under the MIT License. See [LICENSE](LICENSE) for more information.
