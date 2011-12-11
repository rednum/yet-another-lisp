module type EVALUATOR =
  functor (Core : CORE) ->
sig 
  (* type token = TString of string | TList of token list *)
  val eval_ast : Core.token -> Core.token
  exception RuntimeError
end
