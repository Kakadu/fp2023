# Parsing

Parser is implemented using a parser-combinator library [Angstrom](https://github.com/inhabitedtype/angstrom).

The algorithm for parsing infix and prefix operators is based on Alex Kladov's [Simple but Powerful Pratt Parsing](https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html).

To see parser in action please check [test.ml](test/test.ml) & [unit tests](unitTests)
