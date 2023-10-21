open! Base

let parse str =
  Angstrom.parse_string ~consume:Angstrom.Consume.All Structure.parse_structure
    str
