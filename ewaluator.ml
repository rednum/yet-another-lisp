module Evaluator : EVALUATOR =
  struct
    type builtin = Quote | List | If | Set | Define | Lambda | Begin 
    type expr = Builtin of builtin | Label of string | Lista of expr list | Number of int | Procedure of (expr -> expr) | Symbol of string | NotImplemented | Ok
    type token = TString of string | TList of token list

    exception RuntimeError

    let add_bindings env vars args = 
      map2 (fun (Symbol v) a -> Hashtbl.add env v a) vars args;
      env

    let rec eval (wyrazenie : expr) srodowisko =
      let rec eval_builtin b tail srodowisko = 
        match b with
          | Begin -> NotImplemented
          | Lambda -> let (Lista vars :: [body]) = tail in
              Procedure (fun (Lista args) -> eval body (add_bindings srodowisko vars args))
          | Quote -> (Lista tail)
          | List -> Lista (List.map (fun x -> eval x srodowisko) tail)
          | If -> let (test :: etrue :: [efalse]) = tail in
              (match (eval test srodowisko) with
                 | Number 0 -> (eval etrue srodowisko)
                 | _ -> (eval efalse srodowisko))
          | Set -> let ((Symbol name) :: [value]) = tail in
              (Hashtbl.add srodowisko name (eval value srodowisko); Ok)
          | Define ->  let ((Symbol name) :: [value]) = tail in
              (Hashtbl.add srodowisko name (eval value srodowisko); Ok)
          | _ -> NotImplemented
      in
        match wyrazenie with 
          | Lista (head :: tail) ->
              begin
                match head with
                  | Builtin b -> (eval_builtin b tail srodowisko)
                      (* No chyba zle \/ :( *)
                  | Lista l -> eval (Lista ((eval (Lista l) srodowisko)::tail)) srodowisko
                  | Label l -> Hashtbl.find srodowisko l
                  | Procedure p -> (p (Lista tail))
                  | _ -> Ok
              end
          | Label l -> eval (Hashtbl.find srodowisko l) srodowisko
          | Procedure p -> p (Lista [])
          | _ -> wyrazenie
    ;;

    let rec translate tr =
      let translate_one (TString t) = 
        begin
          match t with 
            | "quote" -> Builtin Quote
            | "list" -> Builtin List
            | "if" -> Builtin If
            | "det" -> Builtin Set
            | "define" -> Builtin Define
            | "lambda" -> Builtin Lambda
            | "begin" -> Builtin Begin
            | _ -> (if (Str.string_match (regexp "[0-9]") t 0)
                    then Number (int_of_string t)
                    else Label t)  (* Jeszcze liczby? *)
        end
      in
        match tr with 
          | TString t -> translate_one tr
          | TList l -> Lista (map translate l)

    let eval_ast tr = tr
  end
;;
