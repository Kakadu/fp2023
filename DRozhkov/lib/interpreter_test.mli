val interpret
  :  Ast.expression list
  -> ( (string, Interpreter.value, Base.String.comparator_witness) Base.Map.t
       * Interpreter.value list
       , Interpreter.error )
       result

val extract_expressions : ('a * 'b) list -> 'a list
val process_program : string -> unit
