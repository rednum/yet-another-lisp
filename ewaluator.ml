module Evaluator : EVALUATOR =
  functor (Core : CORE) ->
struct
    (* type builtin = Quote | List | If | Set | Define | Lambda | Begin  *)
    (* type expr = Builtin of builtin | Label of string | Lista of expr list | Number of int | Procedure of (expr -> expr) | Symbol of string | NotImplemented | Ok *)
    (* type token = TString of string | Core.TList of token list *)

    exception RuntimeError

    let add_bindings env vars args = 
      map2 (fun (Core.Symbol v) a -> Hashtbl.add env v a) vars args;
      env

    let rec eval (wyrazenie : Core.expr) srodowisko =
      let rec eval_builtin b tail srodowisko = 
        match b with
          | Core.Begin -> Core.NotImplemented
          | Core.Lambda -> let (Core.Lista vars :: [body]) = tail in
              Core.Procedure (fun (Core.Lista args) -> eval body (add_bindings srodowisko vars args))
          | Core.Quote -> (Core.Lista tail)
          | Core.List -> Core.Lista (List.map (fun x -> eval x srodowisko) tail)
          | Core.If -> let (test :: etrue :: [efalse]) = tail in
              (match (eval test srodowisko) with
                 | Core.Number 0 -> (eval etrue srodowisko)
                 | _ -> (eval efalse srodowisko))
          | Core.Set -> let ((Core.Symbol name) :: [value]) = tail in
              (Hashtbl.add srodowisko name (eval value srodowisko); Core.Ok)
          | Core.Define ->  let ((Core.Symbol name) :: [value]) = tail in
              (Hashtbl.add srodowisko name (eval value srodowisko); Core.Ok)
          | _ -> Core.NotImplemented
      in
        match wyrazenie with 
          | Core.Lista (head :: tail) ->
              begin
                match head with
                  | Core.Builtin b -> (eval_builtin b tail srodowisko)
                      (* No chyba zle \/ :( *)
                  | Core.Lista l -> eval (Core.Lista ((eval (Core.Lista l) srodowisko)::tail)) srodowisko
                  | Core.Label l -> Hashtbl.find srodowisko l
                  | Core.Procedure p -> (p (Core.Lista tail))
                  | _ -> Core.Ok
              end
          | Core.Label l -> eval (Hashtbl.find srodowisko l) srodowisko
          | Core.Procedure p -> p (Core.Lista [])
          | _ -> wyrazenie

    let rec translate tr =
      let translate_one (Core.TString t) = 
        begin
          match t with 
            | "quote" -> Core.Builtin Core.Quote
            | "list" -> Core.Builtin Core.List
            | "if" -> Core.Builtin Core.If
            | "det" -> Core.Builtin Core.Set
            | "define" -> Core.Builtin Core.Define
            | "lambda" -> Core.Builtin Core.Lambda
            | "begin" -> Core.Builtin Core.Begin
            | _ -> (if (Str.string_match (regexp "[0-9]") t 0)
                    then Core.Number (int_of_string t)
                    else Core.Label t)  (* Jeszcze liczby? *)
        end
      in
        match tr with 
          | Core.TString t -> translate_one tr
          | Core.TList l -> Core.Lista (map translate l)

    let eval_ast (tr : Core.token) = tr
  end
;;
