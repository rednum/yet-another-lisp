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

module Make (Evaluator : EVALUATOR) : PARSER = 
struct 
  type token = Evaluator.token
  exception ParsingError

  let tokenize str = 
    let lefts = Str.global_replace (Str.regexp "[ ]*)") " ) "
    and rights = Str.global_replace (Str.regexp "[ ]*(") " ( "
    and cut = Str.split (Str.regexp "[ \t]+")
    in
      cut (lefts (rights (str)))

  let parse (tokens : string list) =
    let rec pom tokens acc = 
      match tokens with 
        | "(" :: t -> let (tokens_2, acc_2) = pom t [] in pom tokens_2 (acc_2::acc)
        | ")" :: t -> (t, (Evaluator.TList acc))
        | h :: t -> pom t (Evaluator.TString h::acc)
        | _ -> (tokens, Evaluator.TList acc)
    and rev t = match t with
      | Evaluator.TString s -> Evaluator.TString s
      | Evaluator.TList l -> Evaluator.TList (List.rev (List.map rev l))
    in
    let (_, tokeny) = pom tokens []
    in match tokeny with
      | Evaluator.TList [Evaluator.TList t] -> rev (Evaluator.TList t)
      | Evaluator.TList [Evaluator.TString s] -> Evaluator.TString s
      | _ -> raise ParsingError

  let build_ast (t : string) = parse (tokenize t)
  let rec print_result (t : Evaluator.token) = 
    match t with 
      | Evaluator.TString s -> s
      | Evaluator.TList l -> String.concat "" ["("; (String.concat " " (List.map print_result l)); ")"]

  let pseudo_prompt (t : string) = print_result (Evaluator.eval_ast (build_ast t))
  let prompt = pseudo_prompt
end
