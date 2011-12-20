open Core

module type EVALUATOR =
sig 
  type token = TString of string | TList of token list
  val eval_ast : token -> token
end

module Make (Core : CORE) : EVALUATOR
