# OCamlTyEff

Interpreter for a subset of OCaml language with support for typed effects

## Typed effects

WIP

## Features

### Parser

-   Structure items (top level statements)

    -   [x] `let` value bindings
        -   [x] `rec`
        -   [x] `and`
        -   [x] `let f x = ..`
    -   [x] expressions

-   Constants

    -   [x] integer, char constants
    -   [x] string constants `"helo world"`
    -   [x] string constants `{|hello world|}`
    -   [x] `()` constant
    -   [x] boolean constants

-   Expressions

    -   [x] function application `f x y`
    -   [x] constants
    -   [x] `let .. in` value bindings
    -   [x] `if .. then .. else ..`
    -   [x] anonymous functions
        -   [x] `fun x y -> ..`
        -   [x] `function`
    -   [x] lists
        -   [x] `[a; b; c]`
        -   [x] `::`, `[]` constructors
    -   [x] `match .. with ..`
    -   [x] constructors' application `None`, `Some x`, `Cons (hd, tl)`, etc.
    -   [x] tuples `a, b, c`
    -   [x] sequences `a; b; c`
    -   [x] prefix and infix operators
        -   `let (+++) a b = a - b in 5 +++ 4`
        -   `let (?!) _ = failwith "?!" in ?!0`
        -   `a + b`, `(+) (-a) b`

-   Patterns
    -   [x] any `_`
    -   [x] variables `a`
    -   [x] constants
    -   [x] tuples `a, b`
    -   [x] or `a | b | c`
    -   [x] constructors' application `None`, `Some x`, `Cons (hd, tl)`, etc.
    -   [x] lists
        -   [x] `::` constructor
        -   [x] `[a; b; c]`

### Type checker

WIP

### Interpreter

WIP

## Build & Run tests

```bash
dune build
dune runtest
```

## License

Distributed under the MIT License. See [LICENSE](LICENSE) for more information.
