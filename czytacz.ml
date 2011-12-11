module Interpreter : functor (Eval : EVALUATOR) -> INTERPRETER =
  functor (Eval : EVALUATOR) ->
    functor (Core : CORE) ->
struct 
  exception ParsingError

  let tokenize str = 
    let lewe_nawiasy = global_replace (regexp "[ ]*)") " ) "
    and prawe_nawiasy = global_replace (regexp "[ ]*(") " ( "
    and potnij = split (regexp "[ \t]+")
    in
      potnij (lewe_nawiasy (prawe_nawiasy (str)))

  (* type token = Core.TString of string | Core.TList of token list *)

  let parse (tokens : string list) =
    let rec pom tokens (Core.TList acc) = 
      match tokens with 
        | "(" :: t -> let (tokens_2, acc_2) = pom t (Core.TList []) in pom tokens_2 (Core.TList (acc_2 :: acc))
        | ")" :: t -> (t, (Core.TList acc))
        | h :: t -> pom t (Core.TList (Core.TString h :: acc))
        | _ -> (tokens, Core.TList acc)
    and rev t = match t with
      | Core.TString s -> Core.TString s
      | Core.TList l -> Core.TList (List.rev (List.map rev l))
    in
    let (_, tokeny) = pom tokens (Core.TList [])
    in match tokeny with
      | Core.TList [Core.TList t] -> rev (Core.TList t)
      | tokeny -> (rev tokeny)

  let build_ast (t : string) = parse (tokenize t)
  let rec print_result (t : Core.token) = 
    match t with 
      | Core.TString s -> s
      | Core.TList l -> String.concat "" ["("; (String.concat " " (List.map print_result l)); ")"]

  module E = Evaluator (Core)
  let pseudo_prompt (t : string) = print_result (E.eval_ast (build_ast t))
  let prompt = pseudo_prompt
end
