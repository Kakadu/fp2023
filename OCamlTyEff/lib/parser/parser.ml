open! Base

let parse str =
  match
    Angstrom.parse_string Structure.parse_structure
      ~consume:Angstrom.Consume.All str
  with
  | Result.Ok x ->
      Result.Ok x
  | Error err ->
      Result.Error (`ParsingError err)
