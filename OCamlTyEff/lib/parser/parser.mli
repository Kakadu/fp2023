open! Base
open Ast

val parse : string -> (structure, [> `ParsingError of string]) result
