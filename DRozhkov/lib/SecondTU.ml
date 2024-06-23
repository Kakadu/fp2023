(*
  Since this code is inside library (not just a module)
  `Parser.parser` in this code is considered as `DRozhkov_lib.Parser.parser`, 
  but the .mli parser parses it as `Parser.parser`
  TODO: find a way to extract name (DRozhkov_lib.Parser vs Parser)
  of file-based module from .clti 
*)

let x = Parser.parser ;;
