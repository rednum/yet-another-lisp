open Core
open Evaluator

module type PARSER =
sig 
  type token
  val prompt : string -> string
  val build_ast : string -> token
  val print_result : token -> string

  exception ParsingError
end

module Make (Evaluator : EVALUATOR) : PARSER
