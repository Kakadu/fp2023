$ dune exec demoFact
[ LetDecl
    ( true
    , "fact"
    , [ NoLabel ("n", IntType) ]
    , UndefinedType
    , IfThenElse
        ( BinOp (Less, Var "n", Const (Int 1))
        , Const (Int 1)
        , BinOp
            (Asterisk, Var "n", Apply (Var "fact", BinOp (Dash, Var "n", Const (Int 1))))
        ) )
]