module type CORE =
sig
  type builtin = Quote | List | If | Set | Define | Lambda | Begin 
  type expr = Builtin of builtin | Label of string | Lista of expr list | Number of int | Procedure of (expr -> (string, expr) Hashtbl.t -> expr) | Symbol of string | NotImplemented | Ok


  val give_env : unit -> (string, expr) Hashtbl.t
end

module Make : CORE =
struct
  type builtin = Quote | List | If | Set | Define | Lambda | Begin 
  type expr = Builtin of builtin | Label of string | Lista of expr list | Number of int | Procedure of (expr -> (string, expr) Hashtbl.t -> expr) | Symbol of string | NotImplemented | Ok
  type token = TString of string | TList of token list

  let give_env () = 
    let env = Hashtbl.create 10 
    and dodaj = Procedure (fun (Lista l) _ -> Number (List.fold_left (fun a (Number h) -> a + h) 0 l))
    and pomnoz = Procedure (fun (Lista l) _ -> Number (List.fold_left (fun a (Number h) -> a + h) 1  l))
    and car = Procedure (fun x _ -> match x with
                           | Lista [Lista (a::_)] -> a
                           | _ -> Number 0)
    and cdr = Procedure (fun x _ -> match x with
                           | Lista [Lista (_::a)] -> Lista a
                           | _ -> Number 0)
    in
      begin
        Hashtbl.add env "+" dodaj;
        Hashtbl.add env "*" pomnoz;
        Hashtbl.add env "car" car;
        Hashtbl.add env "cdr" cdr;
        env
      end
end
