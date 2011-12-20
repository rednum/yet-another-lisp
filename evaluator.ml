open Core

module type EVALUATOR = 
sig
  type token = TString of string | TList of token list
  val eval_ast : token -> token
end

module Make (Core : CORE) : EVALUATOR =
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
        | Core.Procedure p -> TString "procedura"
        | Core.NotImplemented -> TString "not implementation"
        | Core.Ok -> TString "ok"

  let rec print_result (t : token) = 
    match t with 
      | TString s -> s
      | TList l -> String.concat "" ["("; (String.concat " " (List.map print_result l)); ")"]

  let dumpp drz = print_string (print_result (untranslate drz)); print_string "\n"


  let add_bindings env vars args =
    let add_binding var arg = 
      match var with
        | (Core.Label v) -> Hashtbl.add env v arg
        | _ -> raise Core.RuntimeError
    in
    print_string "dodaje\n";
    dumpp (Core.Lista vars);
    dumpp (Core.Lista args);
    ignore (List.map2 add_binding vars args);
    env

      
  let rec eval (wyrazenie : Core.expr) envi =
    let rec eval_builtin b tail envi = 
      match b with
        | Core.Begin -> Core.NotImplemented
        | Core.Lambda ->
            let (Core.Lista vars :: [body]) = tail in
            Core.Procedure (fun (Core.Lista args) env 
                              -> eval body (add_bindings env vars args))
        | Core.Quote -> (Core.Lista tail)
        | Core.List -> Core.Lista (List.map (fun x -> eval x envi) tail)
        | Core.If -> let (test :: etrue :: [efalse]) = tail in
            (match (eval test envi) with
               | Core.Lista [] -> (eval efalse envi)
               | Core.Number 0 -> (eval efalse envi)
               | _ -> (eval etrue envi))
        | Core.Set -> 
            let ((Core.Label name) :: [value]) = tail in
            (Hashtbl.add envi name (eval value envi); Core.Ok)
        | Core.Define ->  let ((Core.Label name) :: [value]) = tail in
            (Hashtbl.add envi name (eval value envi); Core.Ok)
    in
      match wyrazenie with 
        | Core.Lista (head :: tail) ->
            begin
              match head with
                | Core.Builtin b -> (eval_builtin b tail envi)
                | Core.Lista l -> eval (Core.Lista ((eval head envi)::tail)) envi
                | Core.Label l -> (let Core.Procedure p = (Hashtbl.find envi l) 
                                   in dumpp (Core.Lista tail); 
                                   let res = p (Core.Lista (List.map (fun t -> eval t envi) tail))
                                     (* let res = p (Core.Lista tail) *)
                                                    envi
                                     in print_string "res: "; dumpp res; res)
                | Core.Procedure p -> 
                    begin
                      dumpp (Core.Lista tail);
                      let res = (p (Core.Lista tail) envi) in
                        dumpp res;
                        res
                    end
                | _ -> wyrazenie
                (* | l -> l *)
            end
        | Core.Label l -> eval (Hashtbl.find envi l) envi
        | _ -> wyrazenie

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
          | _ -> (if (Str.string_match (Str.regexp "[0-9]") t 0)
                  then Core.Number (int_of_string t)
                  else Core.Label t) 
      end
    in
      match tr with 
        | TString t -> translate_one t
        | TList l -> Core.Lista (List.map translate l)


  let env0 = (Core.give_env ())
  let eval_ast (tr : token) = untranslate (eval (translate tr) env0)
end

