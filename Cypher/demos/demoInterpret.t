  $ ./demoInterpret.exe << EOF
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
  > (martin)-[:FATHER_OF]->(charlie);
  > MATCH (n)
  > RETURN n
  > EOF
  ((7, 8), (0, 0), [])
  ((0, 0), (0, 0),
                        [[("n",
                           (OutNode (1, ["Person"],
                              [("name", (String "Charlie Sheen"))])))
                           ];
                          [("n",
                            (OutNode (2, ["Person"],
                               [("name", (String "Martin Sheen"))])))
                            ];
                          [("n",
                            (OutNode (3, ["Person"],
                               [("name", (String "Michael Douglas"))])))
                            ];
                          [("n",
                            (OutNode (4, ["Person"],
                               [("name", (String "Oliver Stone"))])))
                            ];
                          [("n",
                            (OutNode (5, ["Person"],
                               [("name", (String "Rob Reiner"))])))
                            ];
                          [("n",
                            (OutNode (6, ["Movie"],
                               [("title", (String "Wall Street"))])))
                            ];
                          [("n",
                            (OutNode (7, ["Movie"],
                               [("title", (String "The American President"))])))
                            ]
                          ])
  $ ./demoInterpret.exe << EOF
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
  > (martin)-[:FATHER_OF]->(charlie);
  > MATCH (movie:Movie)
  > RETURN movie.title
  > EOF
  ((7, 8), (0, 0), [])
  ((0, 0), (0, 0),
                        [[("movie.title", (OutConstant (String "Wall Street")))
                           ];
                          [("movie.title",
                            (OutConstant (String "The American President")))]
                          ])
  $ ./demoInterpret.exe << EOF
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
  > (martin)-[:FATHER_OF]->(charlie);
  > MATCH (director {name: 'Oliver Stone'})--(movie)
  > RETURN movie.title
  > EOF
  ((7, 8), (0, 0), [])
  ((0, 0), (0, 0),
                        [[("movie.title", (OutConstant (String "Wall Street")))
                           ]
                          ])
  $ ./demoInterpret.exe << EOF
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
  > (martin)-[:FATHER_OF]->(charlie);
  > MATCH (:Person {name: 'Oliver Stone'})--(movie:Movie)
  > RETURN movie.title
  > EOF
  ((7, 8), (0, 0), [])
  ((0, 0), (0, 0),
                        [[("movie.title", (OutConstant (String "Wall Street")))
                           ]
                          ])
  $ ./demoInterpret.exe << EOF
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
  > (martin)-[:FATHER_OF]->(charlie);
  > MATCH (n)
  > RETURN n.name AS name, n.title AS title
  > EOF
  ((7, 8), (0, 0), [])
  ((0, 0), (0, 0),
                        [[("name", (OutConstant (String "Charlie Sheen")));
                           ("title", (OutConstant Null))];
                          [("name", (OutConstant (String "Martin Sheen")));
                            ("title", (OutConstant Null))];
                          [("name", (OutConstant (String "Michael Douglas")));
                            ("title", (OutConstant Null))];
                          [("name", (OutConstant (String "Oliver Stone")));
                            ("title", (OutConstant Null))];
                          [("name", (OutConstant (String "Rob Reiner")));
                            ("title", (OutConstant Null))];
                          [("name", (OutConstant Null));
                            ("title", (OutConstant (String "Wall Street")))];
                          [("name", (OutConstant Null));
                            ("title",
                             (OutConstant (String "The American President")))
                            ]
                          ])
  $ ./demoInterpret.exe << EOF
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
  > (martin)-[:FATHER_OF]->(charlie);
  > MATCH (:Person {name: 'Oliver Stone'})-->(movie)
  > RETURN movie.title
  > EOF
  ((7, 8), (0, 0), [])
  ((0, 0), (0, 0),
                        [[("movie.title", (OutConstant (String "Wall Street")))
                           ]
                          ])
  $ ./demoInterpret.exe << EOF
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
  > (martin)-[:FATHER_OF]->(charlie);
  > MATCH (:Person {name: 'Oliver Stone'})-[r]->(movie)
  > RETURN r
  > EOF
  ((7, 8), (0, 0), [])
  ((0, 0), (0, 0),
                        [[("r", (OutRel (4, (4, ["DIRECTED"], []), 6)))]])
  $ ./demoInterpret.exe << EOF
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
  > (martin)-[:FATHER_OF]->(charlie);
  > MATCH (a)-[:ACTED_IN {role: 'Bud Fox'}]-(b)
  > RETURN a, b
  > EOF
  ((7, 8), (0, 0), [])
  ((0, 0), (0, 0),
                        [[("a",
                           (OutNode (6, ["Movie"],
                              [("title", (String "Wall Street"))])));
                           ("b",
                            (OutNode (1, ["Person"],
                               [("name", (String "Charlie Sheen"))])))
                           ];
                          [("a",
                            (OutNode (1, ["Person"],
                               [("name", (String "Charlie Sheen"))])));
                            ("b",
                             (OutNode (6, ["Movie"],
                                [("title", (String "Wall Street"))])))
                            ]
                          ])
  $ ./demoInterpret.exe << EOF
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
  > (martin)-[:FATHER_OF]->(charlie);
  > MATCH (wallstreet:Movie {title: 'Wall Street'})<-[:ACTED_IN]-(actor)
  > RETURN actor.name
  > EOF
  ((7, 8), (0, 0), [])
  ((0, 0), (0, 0),
                        [[("actor.name", (OutConstant (String "Charlie Sheen")))
                           ];
                          [("actor.name", (OutConstant (String "Martin Sheen")))
                            ];
                          [("actor.name",
                            (OutConstant (String "Michael Douglas")))]
                          ])
  $ ./demoInterpret.exe << EOF
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
  > (martin)-[:FATHER_OF]->(charlie);
  > MATCH (wallstreet {title: 'Wall Street'})<-[]-(person)
  > RETURN person.name
  > EOF
  ((7, 8), (0, 0), [])
  ((0, 0), (0, 0),
                        [[("person.name",
                           (OutConstant (String "Charlie Sheen")))];
                          [("person.name",
                            (OutConstant (String "Martin Sheen")))];
                          [("person.name",
                            (OutConstant (String "Michael Douglas")))];
                          [("person.name",
                            (OutConstant (String "Oliver Stone")))]
                          ])
  $ ./demoInterpret.exe << EOF
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
  > (martin)-[:FATHER_OF]->(charlie);
  > MATCH (wallstreet {title: 'Wall Street'})<-[r:ACTED_IN]-(actor)
  > RETURN r.role
  > EOF
  ((7, 8), (0, 0), [])
  ((0, 0), (0, 0),
                        [[("r.role", (OutConstant (String "Bud Fox")))];
                          [("r.role", (OutConstant (String "Carl Fox")))];
                          [("r.role", (OutConstant (String "Gordon Gekko")))]])
  $ ./demoInterpret.exe << EOF
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
  > (martin)-[:FATHER_OF]->(charlie);
  > MATCH
  >   (martin:Person {name: 'Martin Sheen'}),
  >   (rob:Person {name: 'Rob Reiner'})
  > CREATE (rob)-[:\`OLD FRIENDS\`]->(martin);
  > MATCH (n {name: 'Rob Reiner'})-[r:\`OLD FRIENDS\`]->()
  > RETURN r
  > EOF
  ((7, 8), (0, 0), [])
  ((0, 1), (0, 0), [])
  ((0, 0), (0, 0),
                                             [[("r",
                                                (OutRel (5,
                                                   (9, ["`OLD FRIENDS`"], []),
                                                   2)))
                                                ]
                                               ])
  $ ./demoInterpret.exe << EOF
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
  > (martin)-[:FATHER_OF]->(charlie);
  > MATCH
  >   (martin:Person {name: 'Martin Sheen'}),
  >   (rob:Person {name: 'Rob Reiner'})
  > CREATE (rob)-[:\`OLD FRIENDS\`]->(martin);
  > MATCH (charlie {name: 'Charlie Sheen'})-[:ACTED_IN]->(movie)<-[:DIRECTED]-(director)
  > RETURN movie.title, director.name
  > EOF
  ((7, 8), (0, 0), [])
  ((0, 1), (0, 0), [])
  ((0, 0), (0, 0),
                                             [[("movie.title",
                                                (OutConstant
                                                   (String "Wall Street")));
                                                ("director.name",
                                                 (OutConstant
                                                    (String "Oliver Stone")))
                                                ]
                                               ])
  $ ./demoInterpret.exe << EOF
  > CREATE
  > (andy:Swedish:Person {name: 'Andy', age: 36, belt: 'white'}),
  > (timothy:Person {name: 'Timothy', age: 25}),
  > (peter:Person {name: 'Peter', age: 35, email: 'peter_n@example.com'}),
  > (andy)-[:KNOWS {since: 2012}]->(timothy),
  > (andy)-[:KNOWS {since: 1999}]->(peter);
  > WITH 30 AS minAge
  > MATCH (a:Person)-[:KNOWS]->(b:Person)
  > WHERE a.name = 'Andy', b.age > minAge
  > RETURN b.name
  > EOF
  ((3, 2), (0, 0), [])
  ((0, 0), (0, 0),
                        [[("b.name", (OutConstant (String "Peter")))]])
  $ ./demoInterpret.exe << EOF
  > CREATE
  > (andy:Swedish:Person {name: 'Andy', age: 36, belt: 'white'}),
  > (timothy:Person {name: 'Timothy', age: 25}),
  > (peter:Person {name: 'Peter', age: 35, email: 'peter_n@example.com'}),
  > (andy)-[:KNOWS {since: 2012}]->(timothy),
  > (andy)-[:KNOWS {since: 1999}]->(peter);
  > MATCH (a:Person {name: 'Andy'})-->(b:Person)
  > RETURN b.name AS friend
  > EOF
  ((3, 2), (0, 0), [])
  ((0, 0), (0, 0),
                        [[("friend", (OutConstant (String "Timothy")))];
                          [("friend", (OutConstant (String "Peter")))]])
  $ ./demoInterpret.exe << EOF
  > CREATE
  > (andy:Swedish:Person {name: 'Andy', age: 36, belt: 'white'}),
  > (timothy:Person {name: 'Timothy', age: 25}),
  > (peter:Person {name: 'Peter', age: 35, email: 'peter_n@example.com'}),
  > (andy)-[:KNOWS {since: 2012}]->(timothy),
  > (andy)-[:KNOWS {since: 1999}]->(peter);
  > MATCH (n:Person)
  > WHERE n.name = 'Peter' XOR (n.age < 30 AND n.name = 'Timothy') OR NOT (n.name = 'Timothy' OR n.name = 'Peter')
  > RETURN
  >   n.name AS name,
  >   n.age AS age
  > ORDER BY name
  > EOF
  ((3, 2), (0, 0), [])
  ((0, 0), (0, 0),
                        [[("name", (OutConstant (String "Andy")));
                           ("age", (OutConstant (Int64 36L)))];
                          [("name", (OutConstant (String "Peter")));
                            ("age", (OutConstant (Int64 35L)))];
                          [("name", (OutConstant (String "Timothy")));
                            ("age", (OutConstant (Int64 25L)))]
                          ])
  $ ./demoInterpret.exe << EOF
  > CREATE
  > (andy:Swedish:Person {name: 'Andy', age: 36, belt: 'white'}),
  > (timothy:Person {name: 'Timothy', age: 25}),
  > (peter:Person {name: 'Peter', age: 35, email: 'peter_n@example.com'}),
  > (andy)-[:KNOWS {since: 2012}]->(timothy),
  > (andy)-[:KNOWS {since: 1999}]->(peter);
  > MATCH (n:Swedish)
  > RETURN n.name, n.age
  > EOF
  ((3, 2), (0, 0), [])
  ((0, 0), (0, 0),
                        [[("n.name", (OutConstant (String "Andy")));
                           ("n.age", (OutConstant (Int64 36L)))]
                          ])
  $ ./demoInterpret.exe << EOF
  > CREATE
  > (andy:Swedish:Person {name: 'Andy', age: 36, belt: 'white'}),
  > (timothy:Person {name: 'Timothy', age: 25}),
  > (peter:Person {name: 'Peter', age: 35, email: 'peter_n@example.com'}),
  > (andy)-[:KNOWS {since: 2012}]->(timothy),
  > (andy)-[:KNOWS {since: 1999}]->(peter);
  > MATCH (n:Person)
  > WHERE n.age < 30
  > RETURN n.name, n.age
  > EOF
  ((3, 2), (0, 0), [])
  ((0, 0), (0, 0),
                        [[("n.name", (OutConstant (String "Timothy")));
                           ("n.age", (OutConstant (Int64 25L)))]
                          ])
  $ ./demoInterpret.exe << EOF
  > CREATE
  > (andy:Swedish:Person {name: 'Andy', age: 36, belt: 'white'}),
  > (timothy:Person {name: 'Timothy', age: 25}),
  > (peter:Person {name: 'Peter', age: 35, email: 'peter_n@example.com'}),
  > (andy)-[:KNOWS {since: 2012}]->(timothy),
  > (andy)-[:KNOWS {since: 1999}]->(peter);
  > MATCH (n:Person)-[k:KNOWS]->(f)
  > WHERE k.since < 2000
  > RETURN f.name, f.age, f.email
  > EOF
  ((3, 2), (0, 0), [])
  ((0, 0), (0, 0),
                        [[("f.name", (OutConstant (String "Peter")));
                           ("f.age", (OutConstant (Int64 35L)));
                           ("f.email",
                            (OutConstant (String "peter_n@example.com")))
                           ]
                          ])
  $ ./demoInterpret.exe << EOF
  > CREATE
  > (andy:Swedish:Person {name: 'Andy', age: 36, belt: 'white'}),
  > (timothy:Person {name: 'Timothy', age: 25}),
  > (peter:Person {name: 'Peter', age: 35, email: 'peter_n@example.com'}),
  > (andy)-[:KNOWS {since: 2012}]->(timothy),
  > (andy)-[:KNOWS {since: 1999}]->(peter);
  > MATCH (n:Person)
  > WHERE n.age < 30
  > RETURN n.name, n.age
  > EOF
  ((3, 2), (0, 0), [])
  ((0, 0), (0, 0),
                        [[("n.name", (OutConstant (String "Timothy")));
                           ("n.age", (OutConstant (Int64 25L)))]
                          ])
  $ ./demoInterpret.exe << EOF
  > CREATE
  > (andy:Swedish:Person {name: 'Andy', age: 36, belt: 'white'}),
  > (timothy:Person {name: 'Timothy', age: 25}),
  > (peter:Person {name: 'Peter', age: 35, email: 'peter_n@example.com'}),
  > (andy)-[:KNOWS {since: 2012}]->(timothy),
  > (andy)-[:KNOWS {since: 1999}]->(peter);
  > MATCH (n:Person)
  > WHERE n.belt IS NOT NULL
  > RETURN n.name, n.belt
  > EOF
  ((3, 2), (0, 0), [])
  ((0, 0), (0, 0),
                        [[("n.name", (OutConstant (String "Andy")));
                           ("n.belt", (OutConstant (String "white")))]
                          ])
  $ ./demoInterpret.exe << EOF
  > CREATE
  > (andy:Swedish:Person {name: 'Andy', age: 36, belt: 'white'}),
  > (timothy:Person {name: 'Timothy', age: 25}),
  > (peter:Person {name: 'Peter', age: 35, email: 'peter_n@example.com'}),
  > (andy)-[:KNOWS {since: 2012}]->(timothy),
  > (andy)-[:KNOWS {since: 1999}]->(peter);
  > MATCH (n:Person)
  > WITH n.name as name
  > WHERE n.age = 25
  > RETURN name
  > EOF
  ((3, 2), (0, 0), [])
  ((0, 0), (0, 0),
                        [[("name", (OutConstant (String "Timothy")))]])
  $ ./demoInterpret.exe << EOF
  > CREATE
  > (andy:Swedish:Person {name: 'Andy', age: 36, belt: 'white'}),
  > (timothy:Person {name: 'Timothy', age: 25}),
  > (peter:Person {name: 'Peter', age: 35, email: 'peter_n@example.com'}),
  > (andy)-[:KNOWS {since: 2012}]->(timothy),
  > (andy)-[:KNOWS {since: 1999}]->(peter);
  > MATCH (n:Person)
  > WHERE n.name STARTS WITH 'Pet'
  > RETURN n.name, n.age
  > EOF
  ((3, 2), (0, 0), [])
  ((0, 0), (0, 0),
                        [[("n.name", (OutConstant (String "Peter")));
                           ("n.age", (OutConstant (Int64 35L)))]
                          ])
  $ ./demoInterpret.exe << EOF
  > CREATE
  > (andy:Swedish:Person {name: 'Andy', age: 36, belt: 'white'}),
  > (timothy:Person {name: 'Timothy', age: 25}),
  > (peter:Person {name: 'Peter', age: 35, email: 'peter_n@example.com'}),
  > (andy)-[:KNOWS {since: 2012}]->(timothy),
  > (andy)-[:KNOWS {since: 1999}]->(peter);
  > MATCH (n:Person)
  > WHERE n.name ENDS WITH 'ter'
  > RETURN n.name, n.age
  > EOF
  ((3, 2), (0, 0), [])
  ((0, 0), (0, 0),
                        [[("n.name", (OutConstant (String "Peter")));
                           ("n.age", (OutConstant (Int64 35L)))]
                          ])
  $ ./demoInterpret.exe << EOF
  > CREATE
  > (andy:Swedish:Person {name: 'Andy', age: 36, belt: 'white'}),
  > (timothy:Person {name: 'Timothy', age: 25}),
  > (peter:Person {name: 'Peter', age: 35, email: 'peter_n@example.com'}),
  > (andy)-[:KNOWS {since: 2012}]->(timothy),
  > (andy)-[:KNOWS {since: 1999}]->(peter);
  > MATCH (n:Person)
  > WHERE n.name CONTAINS 'ete'
  > RETURN n.name, n.age
  > EOF
  ((3, 2), (0, 0), [])
  ((0, 0), (0, 0),
                        [[("n.name", (OutConstant (String "Peter")));
                           ("n.age", (OutConstant (Int64 35L)))]
                          ])
  $ ./demoInterpret.exe << EOF
  > CREATE
  > (andy:Swedish:Person {name: 'Andy', age: 36, belt: 'white'}),
  > (timothy:Person {name: 'Timothy', age: 25}),
  > (peter:Person {name: 'Peter', age: 35, email: 'peter_n@example.com'}),
  > (andy)-[:KNOWS {since: 2012}]->(timothy),
  > (andy)-[:KNOWS {since: 1999}]->(peter);
  > MATCH (n:Person)
  > WHERE NOT n.name ENDS WITH 'y'
  > RETURN n.name, n.age
  > EOF
  ((3, 2), (0, 0), [])
  ((0, 0), (0, 0),
                        [[("n.name", (OutConstant (String "Peter")));
                           ("n.age", (OutConstant (Int64 35L)))]
                          ])
  $ ./demoInterpret.exe << EOF
  > CREATE
  > (andy:Swedish:Person {name: 'Andy', age: 36, belt: 'white'}),
  > (timothy:Person {name: 'Timothy', age: 25}),
  > (peter:Person {name: 'Peter', age: 35, email: 'peter_n@example.com'}),
  > (andy)-[:KNOWS {since: 2012}]->(timothy),
  > (andy)-[:KNOWS {since: 1999}]->(peter);
  > MATCH
  >   (timothy:Person {name: 'Timothy'})<--(other:Person)
  > RETURN other.name, other.age
  > EOF
  ((3, 2), (0, 0), [])
  ((0, 0), (0, 0),
                        [[("other.name", (OutConstant (String "Andy")));
                           ("other.age", (OutConstant (Int64 36L)))]
                          ])
  $ ./demoInterpret.exe << EOF
  > CREATE
  > (andy:Swedish:Person {name: 'Andy', age: 36, belt: 'white'}),
  > (timothy:Person {name: 'Timothy', age: 25}),
  > (peter:Person {name: 'Peter', age: 35, email: 'peter_n@example.com'}),
  > (andy)-[:KNOWS {since: 2012}]->(timothy),
  > (andy)-[:KNOWS {since: 1999}]->(peter);
  > MATCH
  >   (peter:Person {name: 'Peter'}),
  >   (other:Person)-->(notpeter)
  > WHERE NOT (notpeter) = (peter)
  > RETURN other.name, other.age
  > EOF
  ((3, 2), (0, 0), [])
  ((0, 0), (0, 0),
                        [[("other.name", (OutConstant (String "Andy")));
                           ("other.age", (OutConstant (Int64 36L)))]
                          ])
  $ ./demoInterpret.exe << EOF
  > CREATE
  > (andy:Swedish:Person {name: 'Andy', age: 36, belt: 'white'}),
  > (timothy:Person {name: 'Timothy', age: 25}),
  > (peter:Person {name: 'Peter', age: 35, email: 'peter_n@example.com'}),
  > (andy)-[:KNOWS {since: 2012}]->(timothy),
  > (andy)-[:KNOWS {since: 1999}]->(peter);
  > MATCH (other:Person)-[:KNOWS]-({name: 'Timothy'})
  > RETURN other.name, other.age
  > EOF
  ((3, 2), (0, 0), [])
  ((0, 0), (0, 0),
                        [[("other.name", (OutConstant (String "Andy")));
                           ("other.age", (OutConstant (Int64 36L)))]
                          ])
  $ ./demoInterpret.exe << EOF
  > CREATE
  > (andy:Swedish:Person {name: 'Andy', age: 36, belt: 'white'}),
  > (timothy:Person {name: 'Timothy', age: 25}),
  > (peter:Person {name: 'Peter', age: 35, email: 'peter_n@example.com'}),
  > (andy)-[:KNOWS {since: 2012}]->(timothy),
  > (andy)-[:KNOWS {since: 1999}]->(peter);
  > MATCH (a:Person)
  > WHERE a.name = 'Peter' OR a.name = 'Timothy'
  > RETURN a.name, a.age
  > EOF
  ((3, 2), (0, 0), [])
  ((0, 0), (0, 0),
                        [[("a.name", (OutConstant (String "Timothy")));
                           ("a.age", (OutConstant (Int64 25L)))];
                          [("a.name", (OutConstant (String "Peter")));
                            ("a.age", (OutConstant (Int64 35L)))]
                          ])
  $ ./demoInterpret.exe << EOF
  > CREATE
  > (andy:Swedish:Person {name: 'Andy', age: 36, belt: 'white'}),
  > (timothy:Person {name: 'Timothy', age: 25}),
  > (peter:Person {name: 'Peter', age: 35, email: 'peter_n@example.com'}),
  > (andy)-[:KNOWS {since: 2012}]->(timothy),
  > (andy)-[:KNOWS {since: 1999}]->(peter);
  > MATCH (n:Person)
  > WHERE n.belt = 'white'
  > RETURN n.name, n.age, n.belt
  > EOF
  ((3, 2), (0, 0), [])
  ((0, 0), (0, 0),
                        [[("n.name", (OutConstant (String "Andy")));
                           ("n.age", (OutConstant (Int64 36L)));
                           ("n.belt", (OutConstant (String "white")))]
                          ])
  $ ./demoInterpret.exe << EOF
  > CREATE
  > (andy:Swedish:Person {name: 'Andy', age: 36, belt: 'white'}),
  > (timothy:Person {name: 'Timothy', age: 25}),
  > (peter:Person {name: 'Peter', age: 35, email: 'peter_n@example.com'}),
  > (andy)-[:KNOWS {since: 2012}]->(timothy),
  > (andy)-[:KNOWS {since: 1999}]->(peter);
  > MATCH (n:Person)
  > WHERE n.belt = 'white' OR n.belt IS NULL
  > RETURN n.name, n.age, n.belt
  > ORDER BY n.name
  > EOF
  ((3, 2), (0, 0), [])
  ((0, 0), (0, 0),
                        [[("n.name", (OutConstant (String "Andy")));
                           ("n.age", (OutConstant (Int64 36L)));
                           ("n.belt", (OutConstant (String "white")))];
                          [("n.name", (OutConstant (String "Peter")));
                            ("n.age", (OutConstant (Int64 35L)));
                            ("n.belt", (OutConstant Null))];
                          [("n.name", (OutConstant (String "Timothy")));
                            ("n.age", (OutConstant (Int64 25L)));
                            ("n.belt", (OutConstant Null))]
                          ])
  $ ./demoInterpret.exe << EOF
  > CREATE
  > (andy:Swedish:Person {name: 'Andy', age: 36, belt: 'white'}),
  > (timothy:Person {name: 'Timothy', age: 25}),
  > (peter:Person {name: 'Peter', age: 35, email: 'peter_n@example.com'}),
  > (andy)-[:KNOWS {since: 2012}]->(timothy),
  > (andy)-[:KNOWS {since: 1999}]->(peter);
  > MATCH (person:Person)
  > WHERE person.name = 'Peter' AND person.belt IS NULL
  > RETURN person.name, person.age, person.belt
  > EOF
  ((3, 2), (0, 0), [])
  ((0, 0), (0, 0),
                        [[("person.name", (OutConstant (String "Peter")));
                           ("person.age", (OutConstant (Int64 35L)));
                           ("person.belt", (OutConstant Null))]
                          ])
  $ ./demoInterpret.exe << EOF
  > CREATE
  > (andy:Swedish:Person {name: 'Andy', age: 36, belt: 'white'}),
  > (timothy:Person {name: 'Timothy', age: 25}),
  > (peter:Person {name: 'Peter', age: 35, email: 'peter_n@example.com'}),
  > (andy)-[:KNOWS {since: 2012}]->(timothy),
  > (andy)-[:KNOWS {since: 1999}]->(peter);
  > MATCH (a:Person)
  > WHERE a.name >= 'Peter'
  > RETURN a.name, a.age
  > EOF
  ((3, 2), (0, 0), [])
  ((0, 0), (0, 0),
                        [[("a.name", (OutConstant (String "Timothy")));
                           ("a.age", (OutConstant (Int64 25L)))];
                          [("a.name", (OutConstant (String "Peter")));
                            ("a.age", (OutConstant (Int64 35L)))]
                          ])
  $ ./demoInterpret.exe << EOF
  > CREATE
  > (andy:Swedish:Person {name: 'Andy', age: 36, belt: 'white'}),
  > (timothy:Person {name: 'Timothy', age: 25}),
  > (peter:Person {name: 'Peter', age: 35, email: 'peter_n@example.com'}),
  > (andy)-[:KNOWS {since: 2012}]->(timothy),
  > (andy)-[:KNOWS {since: 1999}]->(peter);
  > MATCH (a:Person)
  > WHERE a.name > 'Andy' AND a.name < 'Timothy'
  > RETURN a.name, a.age
  > EOF
  ((3, 2), (0, 0), [])
  ((0, 0), (0, 0),
                        [[("a.name", (OutConstant (String "Peter")));
                           ("a.age", (OutConstant (Int64 35L)))]
                          ])
  $ ./demoInterpret.exe << EOF
  > CREATE
  > (andy:Swedish:Person {name: 'Andy', age: 36, belt: 'white'}),
  > (timothy:Person {name: 'Timothy', age: 25}),
  > (peter:Person {name: 'Peter', age: 35, email: 'peter_n@example.com'}),
  > (andy)-[:KNOWS {since: 2012}]->(timothy),
  > (andy)-[:KNOWS {since: 1999}]->(peter);
  > WITH 2000 AS minYear
  > MATCH (a:Person)-[r:KNOWS]->(b:Person)
  > WHERE r.since < minYear
  > RETURN r.since
  > EOF
  ((3, 2), (0, 0), [])
  ((0, 0), (0, 0),
                        [[("r.since", (OutConstant (Int64 1999L)))]])
  $ ./demoInterpret.exe << EOF
  > CREATE
  > (andy:Swedish:Person {name: 'Andy', age: 36, belt: 'white'}),
  > (timothy:Person {name: 'Timothy', age: 25}),
  > (peter:Person {name: 'Peter', age: 35, email: 'peter_n@example.com'}),
  > (andy)-[:KNOWS {since: 2012}]->(timothy),
  > (andy)-[:KNOWS {since: 1999}]->(peter);
  > WITH 2000 AS minYear
  > MATCH (a:Person {name: 'Andy'})-[r:KNOWS]->(b:Person)
  > WHERE r.since < minYear
  > RETURN r.since AS years
  > EOF
  ((3, 2), (0, 0), [])
  ((0, 0), (0, 0),
                        [[("years", (OutConstant (Int64 1999L)))]])
