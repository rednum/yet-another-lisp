module Inpterpreter : INTERPRETER =
  functor (Eval : EVALUATOR) ->
struct 
  exception ParsingError

  let tokenize str = 
    let lewe_nawiasy = global_replace (regexp "[ ]*)") " ) "
    and prawe_nawiasy = global_replace (regexp "[ ]*(") " ( "
    and potnij = split (regexp "[ \t]+")
    in
      potnij (lewe_nawiasy (prawe_nawiasy (str)))

  (* type token = TString of string | TList of token list *)

  let parse (tokens : string list) =
    let rec pom tokens (TList acc) = 
      match tokens with 
        | "(" :: t -> let (tokens_2, acc_2) = pom t (TList []) in pom tokens_2 (TList (acc_2 :: acc))
        | ")" :: t -> (t, (TList acc))
        | h :: t -> pom t (TList (TString h :: acc))
        | _ -> (tokens, TList acc)
    and rev t = match t with
      | TString s -> TString s
      | TList l -> TList (List.rev (List.map rev l))
    in
    let (_, tokeny) = pom tokens (TList [])
    in (rev tokeny)

  let build_ast (t : string) = Eval.TString "F"
  let print_result (t : Eval.token) = "f"
  let prompt (t : string) = t
end
