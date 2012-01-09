open Core
open Library

module type EVALUATOR = 
sig
  type token = TString of string | TList of token list

  val eval_ast : token -> token
end

module Make (Core : CORE) (Library : LIBRARY with type expression = Core.expr and type environment = Core.environment) : EVALUATOR =
struct
  type token = TString of string | TList of token list

  let rec untranslate (tr : Core.expr) = 
    let untranslate_builtin b =
      (match b with
         | Core.Quote -> "quote"
         | Core.List -> "list" 
         | Core.If -> "if" 
         | Core.Set -> "set"
         | Core.Define -> "define"
         | Core.Lambda -> "lambda"
         | Core.Begin -> "begin")
    in
      match tr with 
        | Core.Builtin b -> TString (untranslate_builtin b)
        | Core.Number n -> TString (string_of_int n)
        | Core.Label l -> TString l
        | Core.Symbol s -> TString s
        | Core.Lista l -> TList (List.map untranslate l)
        | Core.Procedure p -> TString "<funkcja>"

  let rec print_result (t : token) = 
    match t with 
      | TString s -> s
      | TList l -> String.concat "" ["("; (String.concat " " (List.map print_result l)); ")"]
      
  let rec eval (expression : Core.expr) (envi : Core.environment) =
    let rec eval_builtin b tail envi = 
      match b, tail with
        | Core.Begin, _ -> List.fold_left (fun _ h -> eval h envi) (Core.Lista []) tail
        | Core.Lambda, Core.Lista vars :: [body] ->
            Core.Procedure (fun (Core.Lista args) env -> 
                              eval body (Core.new_scope env vars args))
        | Core.Quote, ([Core.Label l]) -> Core.Symbol l
        | Core.List, _ -> Core.Lista (List.map (fun x -> eval x envi) tail)
        | Core.If, (test :: etrue :: [efalse]) -> 
                   (match (eval test envi) with
                      | Core.Lista [] -> (eval efalse envi)
                      | Core.Number 0 -> (eval efalse envi)
                      | _ -> (eval etrue envi))
        | Core.Set, (Core.Label name) :: [value] -> 
            let vall = (eval value envi) 
            in Core.replace envi name vall
        | Core.Define, (Core.Label name) :: [value] ->
            let vall = (eval value envi) 
            in Core.add envi name vall;
        | _, _ -> raise Core.RuntimeError
    in
      match expression with 
        | Core.Lista (head::tail) ->
            begin
              match head with
                | Core.Builtin b -> (eval_builtin b tail envi)
                | Core.Lista l -> eval (Core.Lista ((eval head envi)::tail)) envi
                | Core.Label l -> (let Core.Procedure p = (Core.find envi l) 
                                   in 
                                   let res = p (Core.Lista (List.map (fun t -> eval t envi) tail)) envi
                                     in res)
                | Core.Procedure p -> 
                    begin
                      let res = (p (Core.Lista tail) envi) in
                        res
                    end
                | _ -> expression
            end
        | Core.Label l -> eval (Core.find envi l) envi
        | _ -> expression

  let rec translate tr =
    let translate_one t = 
      begin
        match t with 
          | "quote" -> Core.Builtin Core.Quote
          | "list" -> Core.Builtin Core.List
          | "if" -> Core.Builtin Core.If
          | "set" -> Core.Builtin Core.Set
          | "define" -> Core.Builtin Core.Define
          | "lambda" -> Core.Builtin Core.Lambda
          | "begin" -> Core.Builtin Core.Begin
          | _ -> (if (Str.string_match (Str.regexp "-*[0-9]") t 0)
                  then Core.Number (int_of_string t)
                  else Core.Label t) 
      end
    in
      match tr with 
        | TString t -> translate_one t
        | TList l -> Core.Lista (List.map translate l)


  let env0 = (Library.create_globals ())
  let eval_ast (tr : token) = untranslate (eval (translate tr) env0)
end

