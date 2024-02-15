  $ ./demoInterpreter.exe <<-EOF
  > ../examples/anonymous.rb
  Annonymous function

  $ ruby ../examples/anonymous.rb
  Annonymous function

  $ ./demoInterpreter.exe <<-EOF
  > ../examples/closure.rb
  2

  $ ruby ../examples/closure.rb
  2

  $ ./demoInterpreter.exe <<-EOF
  > ../examples/method_missing.rb
  42
  no such method!

  $ ruby ../examples/method_missing.rb
  42
  no such method!

  $ ./demoInterpreter.exe <<-EOF
  > ../examples/multiple_assignment.rb
  1
  2
  13
  :)
  20

  $ ruby ../examples/multiple_assignment.rb
  1
  2
  13
  :)
  20

  $ ./demoInterpreter.exe <<-EOF
  > ../examples/object.rb
  13
  1998

  $ ruby ../examples/object.rb
  13
  1998

  $ ./demoInterpreter.exe <<-EOF
  > ../examples/recursion.rb
  55
  3628800

  $ ruby ../examples/recursion.rb
  55
  3628800
