# Type inference (Hindley–Milner)

Algorithm W is the best known algorithm for implementing Hindley-Milner type inference.
Algorithm W is simple but has the property of intermingling two separate processes: constraint generation and solving.

Our implementation is based on Heeren B. J., Hage J., Swierstra S. D. (2002) [Generalizing Hindley-Milner type inference algorithms](https://www.researchgate.net/profile/Jurriaan-Hage/publication/2528716_Generalizing_Hindley-Milner_Type_Inference_Algorithms/links/09e415108dfe6e7cbe000000/Generalizing-Hindley-Milner-Type-Inference-Algorithms.pdf).
In this approach constraints on types are first collected by bottom-up traversal ([constraints generation](infer/gen)), and then solved independently ([constraints solving](infer/solve.ml)).

The type system for effects is based on Leijen D. (2014) [Koka: Programming with row polymorphic effect types](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/paper-20.pdf).

# Parsing

Parser is implemented using a parser-combinator library [Angstrom](https://github.com/inhabitedtype/angstrom).

The algorithm for parsing infix and prefix operators is based on Alex Kladov's [Simple but Powerful Pratt Parsing](https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html).

# Standard library

Standard library is implemented in [builtin.ml](builtin/builtin.ml) and [prelude.ml](builtin/prelude.ml).
