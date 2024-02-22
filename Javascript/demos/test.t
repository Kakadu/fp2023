Copyright 2023-2024, Kuarni, AlexShmak

SPDX-License-Identifier: LGPL-3.0-or-later

Cram tests here. They run and compare program output to the expected output
https://dune.readthedocs.io/en/stable/tests.html#cram-tests
Use `dune promote` after you change things that should runned

  $ ./demoString.exe << -EOF
  > let a = 10; console.log("a = "+a)
  a = 10 

  $ ./demoFile.exe <<-EOF
  > ./demoFactorial.js
  40320 
  $ nodejs ./demoFactorial.js
  40320

  $ ./demoFile.exe <<-EOF
  > ./demoCPSFactorial.js
  120 
  $ nodejs ./demoCPSFactorial.js
  120

  $ ./demoFile.exe <<-EOF
  > ./demoFixFactorial.js
  120 
  $ nodejs ./demoFixFactorial.js
  120

  $ ./demoFile.exe <<-EOF
  > ./demoObjects.js
  { prop1: 4, prop2: 'Hi!', getProp1: [Function: getProp1] } 
  Hi! 10 10 
  { name: 'Alex', age: 34 } 
  $ nodejs ./demoObjects.js
  { prop1: 4, prop2: 'Hi!', getProp1: [Function: getProp1] }
  Hi! 10 10
  Person { name: 'Alex', age: 34 }

  $ ./demoFile.exe <<-EOF
  > ./demoClosures.js
  6 7 8 
  $ nodejs ./demoClosures.js
  6 7 8

  $ ./demoFile.exe <<-EOF
  > ./demoArray.js
  0 3 1 4 
  100,101 
  100,101,,,,,,,,,Hello world! 
  100,101,3,4,5,6,7,,,,Hello world! 
  $ nodejs ./demoArray.js
  0 3 1 4
  100,101
  100,101,,,,,,,,,Hello world!
  100,101,3,4,5,6,7,,,,Hello world!
