### An Implementation of SQL language

Author: Zaytsev Dmitriy

Features:

- AST
- Parser
- Interpreter
- Environment (Working with different databases)
- Unit and cram tests

Supported commands:

- SELECT [columns]
- FROM (+ INNER/FULL/RIGHT/LEFT JOIN) [tables]
- WHERE [expression]

Example:

> SELECT table0.id, table0.email, table2.id, table1.id, table1.firstname FROM ((table1 INNER JOIN table2 ON table1.email = table2.email) INNER JOIN table0 ON table0.id = table2.id) WHERE table0.id > 110

