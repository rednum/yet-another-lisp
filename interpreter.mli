module type INTERPRETER =
  functor (Eval : EVALUATOR) ->
   sig 
     val prompt : string -> string
     val build_ast : string -> Eval.token
     val print_result : Eval.token -> string
     exception ParsingError
   end
