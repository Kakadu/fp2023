Copyright 2023-2024, Efremov Alexey 
SPDX-License-Identifier: CC0-1.0

  $ ./demoParse.exe < ./attachments/fac.ll
  [((TFunc ((TInteger 32), [(TInteger 32)])), (GlobalVar "fac"),
    (CFunc
       { ftp = (TFunc ((TInteger 32), [(TInteger 32)]));
         parameters = [(LocalVar "0")];
         basic_blocks =
         [((LocalVar "<start>"),
           (CLabel
              [(MemoryAddress
                  (Alloca ((LocalVar "2"), (TInteger 32),
                     (Const (CInteger (1, 1L))), 4)));
                (MemoryAddress
                   (Alloca ((LocalVar "3"), (TInteger 32),
                      (Const (CInteger (1, 1L))), 4)));
                (MemoryAddress
                   (Store ((TInteger 32),
                      (FromVariable ((LocalVar "0"), (TInteger 32))),
                      (FromVariable ((LocalVar "3"), TPointer)), 4)));
                (MemoryAddress
                   (Load ((LocalVar "4"), (TInteger 32),
                      (FromVariable ((LocalVar "3"), TPointer)), 4)));
                (Other
                   (Icmp ((LocalVar "5"), "slt", (TInteger 32),
                      (FromVariable ((LocalVar "4"), (TInteger 32))),
                      (Const (CInteger (32, 1L))))));
                (Terminator
                   (BrCond ((FromVariable ((LocalVar "5"), (TInteger 1))),
                      (FromVariable ((LocalVar "6"), TLabel)),
                      (FromVariable ((LocalVar "7"), TLabel)))))
                ]));
           ((LocalVar "6"),
            (CLabel
               [(MemoryAddress
                   (Store ((TInteger 32), (Const (CInteger (32, 1L))),
                      (FromVariable ((LocalVar "2"), TPointer)), 4)));
                 (Terminator (Br (FromVariable ((LocalVar "13"), TLabel))))]));
           ((LocalVar "7"),
            (CLabel
               [(MemoryAddress
                   (Load ((LocalVar "8"), (TInteger 32),
                      (FromVariable ((LocalVar "3"), TPointer)), 4)));
                 (Binary
                    (Sub
                       ((LocalVar "9"), (TInteger 32),
                        (FromVariable ((LocalVar "8"), (TInteger 32))),
                        (Const (CInteger (32, 1L))))));
                 (Other
                    (Call ((LocalVar "10"), (TInteger 32),
                       (Const (CPointer (PointerGlob (GlobalVar "fac")))),
                       [(FromVariable ((LocalVar "9"), (TInteger 32)))])));
                 (MemoryAddress
                    (Load ((LocalVar "11"), (TInteger 32),
                       (FromVariable ((LocalVar "3"), TPointer)), 4)));
                 (Binary
                    (Mul
                       ((LocalVar "12"), (TInteger 32),
                        (FromVariable ((LocalVar "10"), (TInteger 32))),
                        (FromVariable ((LocalVar "11"), (TInteger 32))))));
                 (MemoryAddress
                    (Store ((TInteger 32),
                       (FromVariable ((LocalVar "12"), (TInteger 32))),
                       (FromVariable ((LocalVar "2"), TPointer)), 4)));
                 (Terminator (Br (FromVariable ((LocalVar "13"), TLabel))))]));
           ((LocalVar "13"),
            (CLabel
               [(MemoryAddress
                   (Load ((LocalVar "14"), (TInteger 32),
                      (FromVariable ((LocalVar "2"), TPointer)), 4)));
                 (Terminator
                    (Ret ((TInteger 32),
                       (FromVariable ((LocalVar "14"), (TInteger 32))))))
                 ]))
           ]
         }),
    1);
    ((TFunc ((TInteger 32), [])), (GlobalVar "main"),
     (CFunc
        { ftp = (TFunc ((TInteger 32), [])); parameters = [];
          basic_blocks =
          [((LocalVar "<start>"),
            (CLabel
               [(MemoryAddress
                   (Alloca ((LocalVar "1"), (TInteger 32),
                      (Const (CInteger (1, 1L))), 4)));
                 (MemoryAddress
                    (Store ((TInteger 32), (Const (CInteger (32, 0L))),
                       (FromVariable ((LocalVar "1"), TPointer)), 4)));
                 (Other
                    (Call ((LocalVar "2"), (TInteger 32),
                       (Const (CPointer (PointerGlob (GlobalVar "fac")))),
                       [(Const (CInteger (32, 5L)))])));
                 (Terminator
                    (Ret ((TInteger 32),
                       (FromVariable ((LocalVar "2"), (TInteger 32))))))
                 ]))
            ]
          }),
     1)
    ]
 
  $ ./demoParse.exe < ./attachments/test.ll
  [((TInteger 32), (GlobalVar "dd"), (CInteger (32, 0L)), 4);
    ((TInteger 32), (GlobalVar "bb"), (CInteger (32, 32L)), 4);
    ((TFunc ((TInteger 32), [])), (GlobalVar "ds"),
     (CFunc
        { ftp = (TFunc ((TInteger 32), [])); parameters = [];
          basic_blocks =
          [((LocalVar "<start>"),
            (CLabel
               [(MemoryAddress
                   (Alloca ((LocalVar "1"), (TInteger 32),
                      (Const (CInteger (1, 1L))), 4)));
                 (MemoryAddress
                    (Store ((TInteger 32), (Const (CInteger (32, 5L))),
                       (FromVariable ((LocalVar "1"), TPointer)), 4)));
                 (MemoryAddress
                    (Load ((LocalVar "2"), (TInteger 32),
                       (FromVariable ((LocalVar "1"), TPointer)), 4)));
                 (MemoryAddress
                    (Load ((LocalVar "3"), (TInteger 32),
                       (Const (CPointer (PointerGlob (GlobalVar "bb")))), 4)));
                 (Binary
                    (Add
                       ((LocalVar "4"), (TInteger 32),
                        (FromVariable ((LocalVar "3"), (TInteger 32))),
                        (FromVariable ((LocalVar "2"), (TInteger 32))))));
                 (MemoryAddress
                    (Store ((TInteger 32),
                       (FromVariable ((LocalVar "4"), (TInteger 32))),
                       (Const (CPointer (PointerGlob (GlobalVar "bb")))), 4)));
                 (MemoryAddress
                    (Load ((LocalVar "5"), (TInteger 32),
                       (Const (CPointer (PointerGlob (GlobalVar "dd")))), 4)));
                 (MemoryAddress
                    (Store ((TInteger 32),
                       (FromVariable ((LocalVar "5"), (TInteger 32))),
                       (FromVariable ((LocalVar "1"), TPointer)), 4)));
                 (MemoryAddress
                    (Load ((LocalVar "6"), (TInteger 32),
                       (FromVariable ((LocalVar "1"), TPointer)), 4)));
                 (Terminator
                    (Ret ((TInteger 32),
                       (FromVariable ((LocalVar "6"), (TInteger 32))))))
                 ]))
            ]
          }),
     1);
    ((TFunc ((TInteger 32), [])), (GlobalVar "main"),
     (CFunc
        { ftp = (TFunc ((TInteger 32), [])); parameters = [];
          basic_blocks =
          [((LocalVar "<start>"),
            (CLabel
               [(Other
                   (Call ((LocalVar "1"), (TInteger 32),
                      (Const (CPointer (PointerGlob (GlobalVar "ds")))), 
                      [])));
                 (Other
                    (Call ((LocalVar "2"), (TInteger 32),
                       (Const (CPointer (PointerGlob (GlobalVar "ds")))), 
                       [])));
                 (Terminator (Ret ((TInteger 32), (Const (CInteger (32, 0L))))))
                 ]))
            ]
          }),
     1)
    ]
