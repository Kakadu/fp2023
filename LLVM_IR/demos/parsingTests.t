Tests about parsing go here. It's expected that programs parse something and
output a parse tree.
For example, where your test correctness of AST it's recommend to put both
input and output into this file. In this case it will be easier to check that
answer is correct

  $ ./demoParse.exe < ./attachments/fac.ll
  [((TFunc ((TInteger 32), [(TInteger 32)])), (GlobalVar "fac"),
    (CFunc
       { parameters = [(LocalVar "0")];
         basic_blocks =
         [((LocalVar "<start>"),
           (CLabel
              [(Terminator
                  (BrCond ((FromVariable ((LocalVar "5"), (TInteger 1))),
                     (FromVariable ((LocalVar "6"), TLabel)),
                     (FromVariable ((LocalVar "7"), TLabel)))));
                (Other
                   (Icmp ((LocalVar "5"), "slt", (TInteger 32),
                      (FromVariable ((LocalVar "4"), (TInteger 32))),
                      (Const (CInteger (32, 1))))));
                (MemoryAddress
                   (Load ((LocalVar "4"), (TInteger 32),
                      (FromVariable ((LocalVar "3"), TPointer)), 4)));
                (MemoryAddress
                   (Store ((TInteger 32),
                      (FromVariable ((LocalVar "0"), (TInteger 32))),
                      (FromVariable ((LocalVar "3"), TPointer)), 4)));
                (MemoryAddress
                   (Alloca ((LocalVar "3"), (TInteger 32),
                      (Const (CInteger (1, 1))), 4)));
                (MemoryAddress
                   (Alloca ((LocalVar "2"), (TInteger 32),
                      (Const (CInteger (1, 1))), 4)))
                ]));
           ((LocalVar "6"),
            (CLabel
               [(Terminator (Br (FromVariable ((LocalVar "13"), TLabel))));
                 (MemoryAddress
                    (Store ((TInteger 32), (Const (CInteger (32, 1))),
                       (FromVariable ((LocalVar "2"), TPointer)), 4)))
                 ]));
           ((LocalVar "7"),
            (CLabel
               [(Terminator (Br (FromVariable ((LocalVar "13"), TLabel))));
                 (MemoryAddress
                    (Store ((TInteger 32),
                       (FromVariable ((LocalVar "12"), (TInteger 32))),
                       (FromVariable ((LocalVar "2"), TPointer)), 4)));
                 (Binary
                    (Mul
                       ((LocalVar "12"), (TInteger 32),
                        (FromVariable ((LocalVar "10"), (TInteger 32))),
                        (FromVariable ((LocalVar "11"), (TInteger 32))))));
                 (MemoryAddress
                    (Load ((LocalVar "11"), (TInteger 32),
                       (FromVariable ((LocalVar "3"), TPointer)), 4)));
                 (Other
                    (Call ((LocalVar "10"), (TInteger 32),
                       (Const (CPointer (PointerGlob (GlobalVar "fac")))),
                       [(FromVariable ((LocalVar "9"), (TInteger 32)))])));
                 (Binary
                    (Sub
                       ((LocalVar "9"), (TInteger 32),
                        (FromVariable ((LocalVar "8"), (TInteger 32))),
                        (Const (CInteger (32, 1))))));
                 (MemoryAddress
                    (Load ((LocalVar "8"), (TInteger 32),
                       (FromVariable ((LocalVar "3"), TPointer)), 4)))
                 ]));
           ((LocalVar "13"),
            (CLabel
               [(Terminator
                   (Ret ((TInteger 32),
                      (FromVariable ((LocalVar "14"), (TInteger 32))))));
                 (MemoryAddress
                    (Load ((LocalVar "14"), (TInteger 32),
                       (FromVariable ((LocalVar "2"), TPointer)), 4)))
                 ]))
           ]
         }))
    ]
 
