Copyright 2023-2024, Kuarni, AlexShmak

SPDX-License-Identifier: LGPL-3.0-or-later

Cram tests here. They run and compare program output to the expected output
https://dune.readthedocs.io/en/stable/tests.html#cram-tests
Use `dune promote` after you change things that should runned

  $ ./demoFactorial.exe
  40320 

  $ ./demoObjects.exe
  { prop1: 4, prop2: 'Hi!', getProp1: [Function: getProp1] } 
  Hi! 10 10 

  $ ./demoClosures.exe
  6 7 8 

  $ ./demo.exe <<-EOF
  > ./javascriptSamples/demoArray.js
  0 3 1 4 
  100,101 
  100,101,,,,,,,,,Hello world! 
  100,101,3,4,5,6,7,,,,Hello world! 
