open Core;;
open Evaluator;;

module type INTERPRETER =
  (* functor (Eval : EVALUATOR) -> *)
    (* functor (Core : CORE) -> *)
sig 
  type token
  val prompt : string -> string
  val build_ast : string -> token
  val print_result : token -> string

     exception ParsingError
   end;;

(* module Interpreter : (functor (Eval : EVALUATOR) -> functor (Core : CORE) -> (INTERPRETER with type token = Evaluator.token)) = *)
(*   functor (Eval : EVALUATOR) -> *)
(*     functor (Core : CORE) -> *)

module Make (Evaluator : EVALUATOR) : INTERPRETER = 
struct 
  type token = Evaluator.token
  exception ParsingError

  let tokenize str = 
    let lewe_nawiasy = Str.global_replace (Str.regexp "[ ]*)") " ) "
    and prawe_nawiasy = Str.global_replace (Str.regexp "[ ]*(") " ( "
    and potnij = Str.split (Str.regexp "[ \t]+")
    in
      potnij (lewe_nawiasy (prawe_nawiasy (str)))

  (* type token = Evaluator.TString of string | Evaluator.TList of token list *)

  let parse (tokens : string list) =
    let rec pom tokens (Evaluator.TList acc) = 
      match tokens with 
        | "(" :: t -> let (tokens_2, acc_2) = pom t (Evaluator.TList []) in pom tokens_2 (Evaluator.TList (acc_2 :: acc))
        | ")" :: t -> (t, (Evaluator.TList acc))
        | h :: t -> pom t (Evaluator.TList (Evaluator.TString h :: acc))
        | _ -> (tokens, Evaluator.TList acc)
    and rev t = match t with
      | Evaluator.TString s -> Evaluator.TString s
      | Evaluator.TList l -> Evaluator.TList (List.rev (List.map rev l))
    in
    let (_, tokeny) = pom tokens (Evaluator.TList [])
    in match tokeny with
      | Evaluator.TList [Evaluator.TList t] -> rev (Evaluator.TList t)
      | tokeny -> (rev tokeny)

  let build_ast (t : string) = parse (tokenize t)
  let rec print_result (t : Evaluator.token) = 
    match t with 
      | Evaluator.TString s -> s
      | Evaluator.TList l -> String.concat "" ["("; (String.concat " " (List.map print_result l)); ")"]


  (* let f x = E.RuntimeError *)
  (* module EvalC = (Eval (Core)) *)

  let pseudo_prompt (t : string) = print_result (Evaluator.eval_ast (build_ast t))
  let prompt = pseudo_prompt
end
