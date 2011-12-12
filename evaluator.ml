open Core;;

module type EVALUATOR =
  (* functor (Core : CORE) -> *)
sig
  (* type token = TString of string | TList of token list *)

  type token = TString of string | TList of token list
  val eval_ast : token -> token
  exception RuntimeError
end

module Make (Core : CORE) : EVALUATOR
=
    (* type builtin = Quote | List | If | Set | Define | Lambda | Begin  *)
    (* type expr = Builtin of builtin | Label of string | Lista of expr list | Number of int | Procedure of (expr -> expr) | Symbol of string | NotImplemented | Ok *)
    (* type token = TString of string | TList of token list *)
  (* functor (Core : CORE) -> *)
struct
  type token = TString of string | TList of token list
  exception RuntimeError

    let add_bindings env vars args = 
      List.map2 (fun (Core.Label v) a -> Hashtbl.add env v a) vars args;
      env
        
    let rec eval (wyrazenie : Core.expr) srodowisko =
      let rec eval_builtin b tail srodowisko = 
        match b with
          | Core.Begin -> Core.NotImplemented
          | Core.Lambda -> let (Core.Lista vars :: [body]) = tail in
              Core.Procedure (fun (Core.Lista args) -> eval body (add_bindings srodowisko vars args))
               (* -> body *)
          | Core.Quote -> (Core.Lista tail)
          | Core.List -> Core.Lista (List.map (fun x -> eval x srodowisko) tail)
          | Core.If -> let (test :: etrue :: [efalse]) = tail in
              (match (eval test srodowisko) with
                 | Core.Number 0 -> (eval etrue srodowisko)
                 | _ -> (eval efalse srodowisko))
          | Core.Set -> let ((Core.Label name) :: [value]) = tail in
              (Hashtbl.add srodowisko name (eval value srodowisko); Core.Ok)
          | Core.Define ->  let ((Core.Label name) :: [value]) = tail in
              (Hashtbl.add srodowisko name (eval value srodowisko); Core.Ok)
          | _ -> Core.NotImplemented
      in
        match wyrazenie with 
          | Core.Lista (head :: tail) ->
              begin
                match head with
                  | Core.Builtin b -> (eval_builtin b tail srodowisko)
                      (* No chyba zle \/ :( *)
                  | Core.Lista l -> eval (Core.Lista ((eval head srodowisko)::tail)) srodowisko
                      (* Tutaj MECZ! bo moze byc zmienna tez *)
                  | Core.Label l -> (let Core.Procedure p = (Hashtbl.find srodowisko l) 
                                     in p (Core.Lista (List.map (fun t -> eval t srodowisko) tail)))
                  | Core.Procedure p -> (p (Core.Lista tail))
                  | _ -> Core.Ok
              end
          | Core.Label l -> eval (Hashtbl.find srodowisko l) srodowisko
          | Core.Procedure p -> p (Core.Lista [])
          | _ -> wyrazenie

    let rec translate tr =
      let translate_one (TString t) = 
        begin
          match t with 
            | "quote" -> Core.Builtin Core.Quote
            | "list" -> Core.Builtin Core.List
            | "if" -> Core.Builtin Core.If
            | "det" -> Core.Builtin Core.Set
            | "define" -> Core.Builtin Core.Define
            | "lambda" -> Core.Builtin Core.Lambda
            | "begin" -> Core.Builtin Core.Begin
            | _ -> (if (Str.string_match (Str.regexp "[0-9]") t 0)
                    then Core.Number (int_of_string t)
                    else Core.Label t)  (* Jeszcze liczby? *)
        end
      in
        match tr with 
          | TString t -> translate_one tr
          | TList l -> Core.Lista (List.map translate l)

    let rec untranslate (tr : Core.expr) = 
      let untranslate_builtin b =
        (match b with
          | Core.Quote -> "quote"
          | Core.List -> "list" 
          | Core.If -> "if" 
          | Core.Set -> "set"
          | Core.Define -> "define"
          | Core.Lambda -> "lambda"
          | Core.Begin -> "begin"
          | _ -> "ok")
      in
        match tr with 
        | Core.Builtin b -> TString (untranslate_builtin b)
        | Core.Number n -> TString (string_of_int n)
        | Core.Label l -> TString l
        | Core.Symbol s -> TString s
        | Core.Lista l -> TList (List.map untranslate l)
        | _ -> TString "ok?"
            

    let env0 = (Core.give_env ())
    let eval_ast (tr : token) = untranslate (eval (translate tr) env0)
  end
;;
