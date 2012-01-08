open Core
open Library

module type EVALUATOR =
sig 
  type token = TString of string | TList of token list

  val eval_ast : token -> token
end

module Make (Core : CORE) (Library : LIBRARY with type expression = Core.expr and type environment = Core.environment) : EVALUATOR 
