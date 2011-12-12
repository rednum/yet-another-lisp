open Core;;

module type EVALUATOR =
    (* functor (Core : CORE) -> *)
sig 
  (* type token = TString of string | TList of token list *)
  type token = TString of string | TList of token list
  val eval_ast : token -> token

  (* val eval_ast : Core.token -> Core.token *)
  exception RuntimeError
end


module Make (Core : CORE) : EVALUATOR
