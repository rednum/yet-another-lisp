module type INTERPRETER =
  functor (Core : CORE) ->
   sig 
     val prompt : string -> string
     val build_ast : string -> Core.token
     val print_result : Core.token -> string
     exception ParsingError
   end;;
