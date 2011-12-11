module type EVALUATOR =
   sig 
     type token = TString of string | TList of token list
     val eval_ast : token -> token
     exception RuntimeError
   end
