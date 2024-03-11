  $ ./demoParse.exe << EOF
  > CREATE
  > (charlie:Person {name: 'Charlie Sheen'}),
  > (martin:Person {name: 'Martin Sheen'}),
  > (michael:Person {name: 'Michael Douglas'}),
  > (oliver:Person {name: 'Oliver Stone'}),
  > (rob:Person {name: 'Rob Reiner'}),
  > (wallStreet:Movie {title: 'Wall Street'}),
  > (charlie)-[:ACTED_IN {role: 'Bud Fox'}]->(wallStreet),
  > (martin)-[:ACTED_IN {role: 'Carl Fox'}]->(wallStreet),
  > (michael)-[:ACTED_IN {role: 'Gordon Gekko'}]->(wallStreet),
  > (oliver)-[:DIRECTED]->(wallStreet),
  > (thePresident:Movie {title: 'The American President'}),
  > (martin)-[:ACTED_IN {role: 'A.J. MacInerney'}]->(thePresident),
  > (michael)-[:ACTED_IN {role: 'President Andrew Shepherd'}]->(thePresident),
  > (rob)-[:DIRECTED]->(thePresident),
  > (martin)-[:FATHER_OF]->(charlie)
  > EOF
  (Create (
     [{ start_node_pt =
        ((Some "charlie"),
         (["Person"], [("name", (Const (String "Charlie Sheen")))]));
        rel_node_pts = [] };
       { start_node_pt =
         ((Some "martin"),
          (["Person"], [("name", (Const (String "Martin Sheen")))]));
         rel_node_pts = [] };
       { start_node_pt =
         ((Some "michael"),
          (["Person"], [("name", (Const (String "Michael Douglas")))]));
         rel_node_pts = [] };
       { start_node_pt =
         ((Some "oliver"),
          (["Person"], [("name", (Const (String "Oliver Stone")))]));
         rel_node_pts = [] };
       { start_node_pt =
         ((Some "rob"), (["Person"], [("name", (Const (String "Rob Reiner")))]));
         rel_node_pts = [] };
       { start_node_pt =
         ((Some "wallStreet"),
          (["Movie"], [("title", (Const (String "Wall Street")))]));
         rel_node_pts = [] };
       { start_node_pt = ((Some "charlie"), ([], []));
         rel_node_pts =
         [(((None, (["ACTED_IN"], [("role", (Const (String "Bud Fox")))])),
            Right),
           ((Some "wallStreet"), ([], [])))]
         };
       { start_node_pt = ((Some "martin"), ([], []));
         rel_node_pts =
         [(((None, (["ACTED_IN"], [("role", (Const (String "Carl Fox")))])),
            Right),
           ((Some "wallStreet"), ([], [])))]
         };
       { start_node_pt = ((Some "michael"), ([], []));
         rel_node_pts =
         [(((None, (["ACTED_IN"], [("role", (Const (String "Gordon Gekko")))])),
            Right),
           ((Some "wallStreet"), ([], [])))]
         };
       { start_node_pt = ((Some "oliver"), ([], []));
         rel_node_pts =
         [(((None, (["DIRECTED"], [])), Right), ((Some "wallStreet"), ([], [])))
           ]
         };
       { start_node_pt =
         ((Some "thePresident"),
          (["Movie"], [("title", (Const (String "The American President")))]));
         rel_node_pts = [] };
       { start_node_pt = ((Some "martin"), ([], []));
         rel_node_pts =
         [(((None,
             (["ACTED_IN"], [("role", (Const (String "A.J. MacInerney")))])),
            Right),
           ((Some "thePresident"), ([], [])))]
         };
       { start_node_pt = ((Some "michael"), ([], []));
         rel_node_pts =
         [(((None,
             (["ACTED_IN"],
              [("role", (Const (String "President Andrew Shepherd")))])),
            Right),
           ((Some "thePresident"), ([], [])))]
         };
       { start_node_pt = ((Some "rob"), ([], []));
         rel_node_pts =
         [(((None, (["DIRECTED"], [])), Right),
           ((Some "thePresident"), ([], [])))]
         };
       { start_node_pt = ((Some "martin"), ([], []));
         rel_node_pts =
         [(((None, (["FATHER_OF"], [])), Right), ((Some "charlie"), ([], [])))]
         }
       ],
     None))
  $ ./demoParse.exe << EOF
  > MATCH (n)
  > RETURN n
  > EOF
  (Match ([{ start_node_pt = ((Some "n"), ([], [])); rel_node_pts = [] }], 
     [], (Return (None, [((Var "n"), None)], []))))
  $ ./demoParse.exe << EOF
  > MATCH (movie:Movie)
  > RETURN movie.title
  > EOF
  (Match (
     [{ start_node_pt = ((Some "movie"), (["Movie"], [])); rel_node_pts = [] }],
     [], (Return (None, [((Property ("movie", "title")), None)], []))))
  $ ./demoParse.exe << EOF
  > MATCH (director {name: 'Oliver Stone'})--(movie)
  > RETURN movie.title
  > EOF
  (Match (
     [{ start_node_pt =
        ((Some "director"), ([], [("name", (Const (String "Oliver Stone")))]));
        rel_node_pts = [(((None, ([], [])), No), ((Some "movie"), ([], [])))] }
       ],
     [], (Return (None, [((Property ("movie", "title")), None)], []))))
  $ ./demoParse.exe << EOF
  > MATCH (:Person {name: 'Oliver Stone'})--(movie:Movie)
  > RETURN movie.title
  > EOF
  (Match (
     [{ start_node_pt =
        (None, (["Person"], [("name", (Const (String "Oliver Stone")))]));
        rel_node_pts =
        [(((None, ([], [])), No), ((Some "movie"), (["Movie"], [])))] }
       ],
     [], (Return (None, [((Property ("movie", "title")), None)], []))))
  $ ./demoParse.exe << EOF
  > MATCH (n)
  > RETURN n.name AS name, n.title AS title
  > EOF
  (Match ([{ start_node_pt = ((Some "n"), ([], [])); rel_node_pts = [] }], 
     [],
     (Return (None,
        [((Property ("n", "name")), (Some "name"));
          ((Property ("n", "title")), (Some "title"))],
        []))
     ))
  $ ./demoParse.exe << EOF
  > MATCH (:Person {name: 'Oliver Stone'})-->(movie)
  > RETURN movie.title
  > EOF
  (Match (
     [{ start_node_pt =
        (None, (["Person"], [("name", (Const (String "Oliver Stone")))]));
        rel_node_pts =
        [(((None, ([], [])), Right), ((Some "movie"), ([], [])))] }
       ],
     [], (Return (None, [((Property ("movie", "title")), None)], []))))
  $ ./demoParse.exe << EOF
  > MATCH (:Person {name: 'Oliver Stone'})-[r]->(movie)
  > RETURN r
  > EOF
  (Match (
     [{ start_node_pt =
        (None, (["Person"], [("name", (Const (String "Oliver Stone")))]));
        rel_node_pts =
        [((((Some "r"), ([], [])), Right), ((Some "movie"), ([], [])))] }
       ],
     [], (Return (None, [((Var "r"), None)], []))))
  $ ./demoParse.exe << EOF
  > MATCH (a)-[:ACTED_IN {role: 'Bud Fox'}]-(b)
  > RETURN a, b
  > EOF
  (Match (
     [{ start_node_pt = ((Some "a"), ([], []));
        rel_node_pts =
        [(((None, (["ACTED_IN"], [("role", (Const (String "Bud Fox")))])), No),
          ((Some "b"), ([], [])))]
        }
       ],
     [], (Return (None, [((Var "a"), None); ((Var "b"), None)], []))))
  $ ./demoParse.exe << EOF
  > MATCH (wallstreet:Movie {title: 'Wall Street'})<-[:ACTED_IN]-(actor)
  > RETURN actor.name
  > EOF
  (Match (
     [{ start_node_pt =
        ((Some "wallstreet"),
         (["Movie"], [("title", (Const (String "Wall Street")))]));
        rel_node_pts =
        [(((None, (["ACTED_IN"], [])), Left), ((Some "actor"), ([], [])))] }
       ],
     [], (Return (None, [((Property ("actor", "name")), None)], []))))
  $ ./demoParse.exe << EOF
  > MATCH (wallstreet {title: 'Wall Street'})<-[]-(person)
  > RETURN person.name
  > EOF
  (Match (
     [{ start_node_pt =
        ((Some "wallstreet"), ([], [("title", (Const (String "Wall Street")))]));
        rel_node_pts =
        [(((None, ([], [])), Left), ((Some "person"), ([], [])))] }
       ],
     [], (Return (None, [((Property ("person", "name")), None)], []))))
  $ ./demoParse.exe << EOF
  > MATCH (wallstreet {title: 'Wall Street'})<-[r:ACTED_IN]-(actor)
  > RETURN r.role
  > EOF
  (Match (
     [{ start_node_pt =
        ((Some "wallstreet"), ([], [("title", (Const (String "Wall Street")))]));
        rel_node_pts =
        [((((Some "r"), (["ACTED_IN"], [])), Left), ((Some "actor"), ([], [])))
          ]
        }
       ],
     [], (Return (None, [((Property ("r", "role")), None)], []))))
  $ ./demoParse.exe << EOF
  > MATCH
  >   (martin:Person {name: 'Martin Sheen'}),
  >   (rob:Person {name: 'Rob Reiner'})
  > CREATE (rob)-[:\`OLD FRIENDS\`]->(martin)
  > EOF
  (Match (
     [{ start_node_pt =
        ((Some "martin"),
         (["Person"], [("name", (Const (String "Martin Sheen")))]));
        rel_node_pts = [] };
       { start_node_pt =
         ((Some "rob"), (["Person"], [("name", (Const (String "Rob Reiner")))]));
         rel_node_pts = [] }
       ],
     [],
     (Create (
        [{ start_node_pt = ((Some "rob"), ([], []));
           rel_node_pts =
           [(((None, (["`OLD FRIENDS`"], [])), Right),
             ((Some "martin"), ([], [])))]
           }
          ],
        None))
     ))
  $ ./demoParse.exe << EOF
  > MATCH (n {name: 'Rob Reiner'})-[r:\`OLD FRIENDS\`]->()
  > RETURN r
  > EOF
  (Match (
     [{ start_node_pt =
        ((Some "n"), ([], [("name", (Const (String "Rob Reiner")))]));
        rel_node_pts =
        [((((Some "r"), (["`OLD FRIENDS`"], [])), Right), (None, ([], [])))] }
       ],
     [], (Return (None, [((Var "r"), None)], []))))
  $ ./demoParse.exe << EOF
  > MATCH (charlie {name: 'Charlie Sheen'})-[:ACTED_IN]->(movie)<-[:DIRECTED]-(director)
  > RETURN movie.title, director.name
  > EOF
  (Match (
     [{ start_node_pt =
        ((Some "charlie"), ([], [("name", (Const (String "Charlie Sheen")))]));
        rel_node_pts =
        [(((None, (["ACTED_IN"], [])), Right), ((Some "movie"), ([], [])));
          (((None, (["DIRECTED"], [])), Left), ((Some "director"), ([], [])))]
        }
       ],
     [],
     (Return (None,
        [((Property ("movie", "title")), None);
          ((Property ("director", "name")), None)],
        []))
     ))
  $ ./demoParse.exe << EOF
  > CREATE
  > (andy:Swedish:Person {name: 'Andy', age: 36, belt: 'white'}),
  > (timothy:Person {name: 'Timothy', age: 25}),
  > (peter:Person {name: 'Peter', age: 35, email: 'peter_n@example.com'}),
  > (andy)-[:KNOWS {since: 2012}]->(timothy),
  > (andy)-[:KNOWS {since: 1999}]->(peter)
  > EOF
  (Create (
     [{ start_node_pt =
        ((Some "andy"),
         (["Swedish"; "Person"],
          [("name", (Const (String "Andy"))); ("age", (Const (Int64 36L)));
            ("belt", (Const (String "white")))]));
        rel_node_pts = [] };
       { start_node_pt =
         ((Some "timothy"),
          (["Person"],
           [("name", (Const (String "Timothy"))); ("age", (Const (Int64 25L)))]));
         rel_node_pts = [] };
       { start_node_pt =
         ((Some "peter"),
          (["Person"],
           [("name", (Const (String "Peter"))); ("age", (Const (Int64 35L)));
             ("email", (Const (String "peter_n@example.com")))]));
         rel_node_pts = [] };
       { start_node_pt = ((Some "andy"), ([], []));
         rel_node_pts =
         [(((None, (["KNOWS"], [("since", (Const (Int64 2012L)))])), Right),
           ((Some "timothy"), ([], [])))]
         };
       { start_node_pt = ((Some "andy"), ([], []));
         rel_node_pts =
         [(((None, (["KNOWS"], [("since", (Const (Int64 1999L)))])), Right),
           ((Some "peter"), ([], [])))]
         }
       ],
     None))
  $ ./demoParse.exe << EOF
  > WITH 30 AS minAge
  > MATCH (a:Person)-[:KNOWS]->(b:Person)
  > WHERE a.name = 'Andy', b.age > minAge
  > RETURN b.name
  > EOF
  (With (None, [((Const (Int64 30L)), "minAge")], [], [],
     (Match (
        [{ start_node_pt = ((Some "a"), (["Person"], []));
           rel_node_pts =
           [(((None, (["KNOWS"], [])), Right), ((Some "b"), (["Person"], [])))]
           }
          ],
        [(List_op ((Property ("a", "name")), [(Eq, (Const (String "Andy")))]));
          (List_op ((Property ("b", "age")), [(Greater, (Var "minAge"))]))],
        (Return (None, [((Property ("b", "name")), None)], []))))
     ))
  $ ./demoParse.exe << EOF
  > MATCH (a:Person {name: 'Andy'})-->(b:Person)
  > RETURN b.name AS friend
  > EOF
  (Match (
     [{ start_node_pt =
        ((Some "a"), (["Person"], [("name", (Const (String "Andy")))]));
        rel_node_pts =
        [(((None, ([], [])), Right), ((Some "b"), (["Person"], [])))] }
       ],
     [], (Return (None, [((Property ("b", "name")), (Some "friend"))], []))))
  $ ./demoParse.exe << EOF
  > MATCH (n:Person)
  > WHERE n.name = 'Peter' XOR (n.age < 30 AND n.name = 'Timothy') OR NOT (n.name = 'Timothy' OR n.name = 'Peter')
  > RETURN
  >   n.name AS name,
  >   n.age AS age
  > ORDER BY name
  > EOF
  (Match (
     [{ start_node_pt = ((Some "n"), (["Person"], [])); rel_node_pts = [] }],
     [(Bin_op (OR,
         (Bin_op (XOR,
            (List_op ((Property ("n", "name")),
               [(Eq, (Const (String "Peter")))])),
            (Bin_op (AND,
               (List_op ((Property ("n", "age")), [(Less, (Const (Int64 30L)))]
                  )),
               (List_op ((Property ("n", "name")),
                  [(Eq, (Const (String "Timothy")))]))
               ))
            )),
         (Un_op (NOT,
            (Bin_op (OR,
               (List_op ((Property ("n", "name")),
                  [(Eq, (Const (String "Timothy")))])),
               (List_op ((Property ("n", "name")),
                  [(Eq, (Const (String "Peter")))]))
               ))
            ))
         ))
       ],
     (Return (None,
        [((Property ("n", "name")), (Some "name"));
          ((Property ("n", "age")), (Some "age"))],
        [([(Var "name")], Asc)]))
     ))
  $ ./demoParse.exe << EOF
  > MATCH (n:Swedish)
  > RETURN n.name, n.age
  > EOF
  (Match (
     [{ start_node_pt = ((Some "n"), (["Swedish"], [])); rel_node_pts = [] }],
     [],
     (Return (None,
        [((Property ("n", "name")), None); ((Property ("n", "age")), None)], 
        []))
     ))
  $ ./demoParse.exe << EOF
  > MATCH (n:Person)
  > WHERE n.age < 30
  > RETURN n.name, n.age
  > EOF
  (Match (
     [{ start_node_pt = ((Some "n"), (["Person"], [])); rel_node_pts = [] }],
     [(List_op ((Property ("n", "age")), [(Less, (Const (Int64 30L)))]))],
     (Return (None,
        [((Property ("n", "name")), None); ((Property ("n", "age")), None)], 
        []))
     ))
  $ ./demoParse.exe << EOF
  > MATCH (n:Person)-[k:KNOWS]->(f)
  > WHERE k.since < 2000
  > RETURN f.name, f.age, f.email
  > EOF
  (Match (
     [{ start_node_pt = ((Some "n"), (["Person"], []));
        rel_node_pts =
        [((((Some "k"), (["KNOWS"], [])), Right), ((Some "f"), ([], [])))] }
       ],
     [(List_op ((Property ("k", "since")), [(Less, (Const (Int64 2000L)))]))],
     (Return (None,
        [((Property ("f", "name")), None); ((Property ("f", "age")), None);
          ((Property ("f", "email")), None)],
        []))
     ))
  $ ./demoParse.exe << EOF
  > MATCH (n:Person)
  > WHERE n.age < 30
  > RETURN n.name, n.age
  > EOF
  (Match (
     [{ start_node_pt = ((Some "n"), (["Person"], [])); rel_node_pts = [] }],
     [(List_op ((Property ("n", "age")), [(Less, (Const (Int64 30L)))]))],
     (Return (None,
        [((Property ("n", "name")), None); ((Property ("n", "age")), None)], 
        []))
     ))
  $ ./demoParse.exe << EOF
  > MATCH (n:Person)
  > WHERE n.belt IS NOT NULL
  > RETURN n.name, n.belt
  > EOF
  (Match (
     [{ start_node_pt = ((Some "n"), (["Person"], [])); rel_node_pts = [] }],
     [(Un_op (IS_NOT_NULL, (Property ("n", "belt"))))],
     (Return (None,
        [((Property ("n", "name")), None); ((Property ("n", "belt")), None)],
        []))
     ))
  $ ./demoParse.exe << EOF
  > MATCH (n:Person)
  > WITH n.name as name
  > WHERE n.age = 25
  > RETURN name
  > EOF
  (Match (
     [{ start_node_pt = ((Some "n"), (["Person"], [])); rel_node_pts = [] }],
     [],
     (With (None, [((Property ("n", "name")), "name")], [],
        [(List_op ((Property ("n", "age")), [(Eq, (Const (Int64 25L)))]))],
        (Return (None, [((Var "name"), None)], []))))
     ))
  $ ./demoParse.exe << EOF
  > MATCH (n:Person)
  > WHERE n.name STARTS WITH 'Pet'
  > RETURN n.name, n.age
  > EOF
  (Match (
     [{ start_node_pt = ((Some "n"), (["Person"], [])); rel_node_pts = [] }],
     [(Bin_op (STARTS_WITH, (Property ("n", "name")), (Const (String "Pet"))))],
     (Return (None,
        [((Property ("n", "name")), None); ((Property ("n", "age")), None)], 
        []))
     ))
  $ ./demoParse.exe << EOF
  > MATCH (n:Person)
  > WHERE n.name ENDS WITH 'ter'
  > RETURN n.name, n.age
  > EOF
  (Match (
     [{ start_node_pt = ((Some "n"), (["Person"], [])); rel_node_pts = [] }],
     [(Bin_op (ENDS_WITH, (Property ("n", "name")), (Const (String "ter"))))],
     (Return (None,
        [((Property ("n", "name")), None); ((Property ("n", "age")), None)], 
        []))
     ))
  $ ./demoParse.exe << EOF
  > MATCH (n:Person)
  > WHERE n.name CONTAINS 'ete'
  > RETURN n.name, n.age
  > EOF
  (Match (
     [{ start_node_pt = ((Some "n"), (["Person"], [])); rel_node_pts = [] }],
     [(Bin_op (CONTAINS, (Property ("n", "name")), (Const (String "ete"))))],
     (Return (None,
        [((Property ("n", "name")), None); ((Property ("n", "age")), None)], 
        []))
     ))
  $ ./demoParse.exe << EOF
  > MATCH (n:Person)
  > WHERE NOT n.name ENDS WITH 'y'
  > RETURN n.name, n.age
  > EOF
  (Match (
     [{ start_node_pt = ((Some "n"), (["Person"], [])); rel_node_pts = [] }],
     [(Un_op (NOT,
         (Bin_op (ENDS_WITH, (Property ("n", "name")), (Const (String "y"))))))
       ],
     (Return (None,
        [((Property ("n", "name")), None); ((Property ("n", "age")), None)], 
        []))
     ))
  $ ./demoParse.exe << EOF
  > MATCH
  >   (timothy:Person {name: 'Timothy'})<--(other:Person)
  > RETURN other.name, other.age
  > EOF
  (Match (
     [{ start_node_pt =
        ((Some "timothy"), (["Person"], [("name", (Const (String "Timothy")))]));
        rel_node_pts =
        [(((None, ([], [])), Left), ((Some "other"), (["Person"], [])))] }
       ],
     [],
     (Return (None,
        [((Property ("other", "name")), None);
          ((Property ("other", "age")), None)],
        []))
     ))
  $ ./demoParse.exe << EOF
  > MATCH
  >   (peter:Person {name: 'Peter'}),
  >   (other:Person)-->(notpeter)
  > WHERE NOT (notpeter) = (peter)
  > RETURN other.name, other.age
  > EOF
  (Match (
     [{ start_node_pt =
        ((Some "peter"), (["Person"], [("name", (Const (String "Peter")))]));
        rel_node_pts = [] };
       { start_node_pt = ((Some "other"), (["Person"], []));
         rel_node_pts =
         [(((None, ([], [])), Right), ((Some "notpeter"), ([], [])))] }
       ],
     [(Un_op (NOT, (List_op ((Var "notpeter"), [(Eq, (Var "peter"))]))))],
     (Return (None,
        [((Property ("other", "name")), None);
          ((Property ("other", "age")), None)],
        []))
     ))
  $ ./demoParse.exe << EOF
  > MATCH (other:Person)-[:KNOWS]-({name: 'Timothy'})
  > RETURN other.name, other.age
  > EOF
  (Match (
     [{ start_node_pt = ((Some "other"), (["Person"], []));
        rel_node_pts =
        [(((None, (["KNOWS"], [])), No),
          (None, ([], [("name", (Const (String "Timothy")))])))]
        }
       ],
     [],
     (Return (None,
        [((Property ("other", "name")), None);
          ((Property ("other", "age")), None)],
        []))
     ))
  $ ./demoParse.exe << EOF
  > MATCH (a:Person)
  > WHERE a.name = 'Peter' OR a.name = 'Timothy'
  > RETURN a.name, a.age
  > EOF
  (Match (
     [{ start_node_pt = ((Some "a"), (["Person"], [])); rel_node_pts = [] }],
     [(Bin_op (OR,
         (List_op ((Property ("a", "name")), [(Eq, (Const (String "Peter")))])),
         (List_op ((Property ("a", "name")), [(Eq, (Const (String "Timothy")))]
            ))
         ))
       ],
     (Return (None,
        [((Property ("a", "name")), None); ((Property ("a", "age")), None)], 
        []))
     ))
  $ ./demoParse.exe << EOF
  > MATCH (n:Person)
  > WHERE n.belt = 'white'
  > RETURN n.name, n.age, n.belt
  > EOF
  (Match (
     [{ start_node_pt = ((Some "n"), (["Person"], [])); rel_node_pts = [] }],
     [(List_op ((Property ("n", "belt")), [(Eq, (Const (String "white")))]))],
     (Return (None,
        [((Property ("n", "name")), None); ((Property ("n", "age")), None);
          ((Property ("n", "belt")), None)],
        []))
     ))
  $ ./demoParse.exe << EOF
  > MATCH (n:Person)
  > WHERE n.belt = 'white' OR n.belt IS NULL
  > RETURN n.name, n.age, n.belt
  > ORDER BY n.name
  > EOF
  (Match (
     [{ start_node_pt = ((Some "n"), (["Person"], [])); rel_node_pts = [] }],
     [(Bin_op (OR,
         (List_op ((Property ("n", "belt")), [(Eq, (Const (String "white")))])),
         (Un_op (IS_NULL, (Property ("n", "belt"))))))
       ],
     (Return (None,
        [((Property ("n", "name")), None); ((Property ("n", "age")), None);
          ((Property ("n", "belt")), None)],
        [([(Property ("n", "name"))], Asc)]))
     ))
  $ ./demoParse.exe << EOF
  > MATCH (person:Person)
  > WHERE person.name = 'Peter' AND person.belt IS NULL
  > RETURN person.name, person.age, person.belt
  > EOF
  (Match (
     [{ start_node_pt = ((Some "person"), (["Person"], [])); rel_node_pts = []
        }
       ],
     [(Bin_op (AND,
         (List_op ((Property ("person", "name")),
            [(Eq, (Const (String "Peter")))])),
         (Un_op (IS_NULL, (Property ("person", "belt"))))))
       ],
     (Return (None,
        [((Property ("person", "name")), None);
          ((Property ("person", "age")), None);
          ((Property ("person", "belt")), None)],
        []))
     ))
  $ ./demoParse.exe << EOF
  > MATCH (a:Person)
  > WHERE a.name >= 'Peter'
  > RETURN a.name, a.age
  > EOF
  (Match (
     [{ start_node_pt = ((Some "a"), (["Person"], [])); rel_node_pts = [] }],
     [(List_op ((Property ("a", "name")), [(GEq, (Const (String "Peter")))]))],
     (Return (None,
        [((Property ("a", "name")), None); ((Property ("a", "age")), None)], 
        []))
     ))
  $ ./demoParse.exe << EOF
  > MATCH (a:Person)
  > WHERE a.name > 'Andy' AND a.name < 'Timothy'
  > RETURN a.name, a.age
  > EOF
  (Match (
     [{ start_node_pt = ((Some "a"), (["Person"], [])); rel_node_pts = [] }],
     [(Bin_op (AND,
         (List_op ((Property ("a", "name")),
            [(Greater, (Const (String "Andy")))])),
         (List_op ((Property ("a", "name")),
            [(Less, (Const (String "Timothy")))]))
         ))
       ],
     (Return (None,
        [((Property ("a", "name")), None); ((Property ("a", "age")), None)], 
        []))
     ))
  $ ./demoParse.exe << EOF
  > WITH 2000 AS minYear
  > MATCH (a:Person)-[r:KNOWS]->(b:Person)
  > WHERE r.since < minYear
  > RETURN r.since
  > EOF
  (With (None, [((Const (Int64 2000L)), "minYear")], [], [],
     (Match (
        [{ start_node_pt = ((Some "a"), (["Person"], []));
           rel_node_pts =
           [((((Some "r"), (["KNOWS"], [])), Right),
             ((Some "b"), (["Person"], [])))]
           }
          ],
        [(List_op ((Property ("r", "since")), [(Less, (Var "minYear"))]))],
        (Return (None, [((Property ("r", "since")), None)], []))))
     ))
  $ ./demoParse.exe << EOF
  > WITH 2000 AS minYear
  > MATCH (a:Person {name: 'Andy'})-[r:KNOWS]->(b:Person)
  > WHERE r.since < minYear
  > RETURN r.since AS years
  > EOF
  (With (None, [((Const (Int64 2000L)), "minYear")], [], [],
     (Match (
        [{ start_node_pt =
           ((Some "a"), (["Person"], [("name", (Const (String "Andy")))]));
           rel_node_pts =
           [((((Some "r"), (["KNOWS"], [])), Right),
             ((Some "b"), (["Person"], [])))]
           }
          ],
        [(List_op ((Property ("r", "since")), [(Less, (Var "minYear"))]))],
        (Return (None, [((Property ("r", "since")), (Some "years"))], []))))
     ))
