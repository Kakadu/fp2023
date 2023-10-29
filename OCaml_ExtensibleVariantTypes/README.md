### An implementaion of Lambda mini-language

This is a homework for functional programming course.

License: LGPL for implementation code + WTFPL for test examles in miniLanguage

Author: Vasy Pupkin, vasya@pupkin.com

Features done (append only):

- Parser  (for example)
- interpreter of non-recursive functions (for example)
- ...

Features in progress (and TODOs):

- Interpreter of recursive functions is not yet ready  (for example)
- TODO: make pretty-printing less memory consuming (for example)
- ...


##### Замечания по стилю кодирования

- Если merge request не проходит CI -- проверяться не будет
- Замечания должны быть откомментированы, иначе проверяться не будет.
  - Если исправлены, должны быть поменчены как "исправлены"
  - Если непонятны/некорректны, то это должно быть откомментировано соответствующим образом.

  Такие суровые ограничения вводятся, чтобы замечания не игнорировались.

- Иимена типов и функций -- snake_case
- Имена типов модулей и модулей -- CamelCase
- Ворнинги должны быть пофикшены
- Не стесняйтесь писать `if ... then ... else` вместо `match ... with true -> .. | false -> ...`
- Не стесняйтесь писать гварды в мэтчинге, например
```ocaml
match ... with
| x when f x -> ...
| x          -> ...
| ...
```
вместо
```ocaml
match ... with
| x -> if f x then ... else ...
| ...
```
- Вместо `fun x y -> match y with` лучше писать короче: `fun x -> function`
- Используйте quoted string literals в тестах, чтобы не экранировать руками
```
─( 11:21:01 )─< command 1 >────────────────────────────
utop # {|
  int main () {
    return 0;
  }
  |};;
- : string = "\n  int main () {\n    return 0;\n  }\n  "
```
- Не надо писать
```ocaml
match ... with
| x ->
    Hashtbl.replace tbl key value |> fun () -> ...
```
Лучше
```ocaml
match ... with
| x ->
    let () = Hashtbl.replace tbl key value in
    ...
```
или
```ocaml
match ... with
| x -> (
    Hashtbl.replace tbl key value;
    ...
  )
```
или даже
```ocaml
match ... with
| x -> begin
    Hashtbl.replace tbl key value;
    ...
  end
```
- Не надо писать
```ocaml
let x = if long_expression then true else false in ...
```
лучше
```ocaml
let x = long_expression in ...
```

- 1
