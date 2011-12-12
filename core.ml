module type CORE =
  sig
    (* type builtin *)
    (* type expr *)
    (* type token *)
    type builtin = Quote | List | If | Set | Define | Lambda | Begin 
    type expr = Builtin of builtin | Label of string | Lista of expr list | Number of int | Procedure of (expr -> expr) | Symbol of string | NotImplemented | Ok
    type token = TString of string | TList of token list

    val give_env : unit -> (string, expr) Hashtbl.t
  end;;

module Make : CORE =
  struct
    type builtin = Quote | List | If | Set | Define | Lambda | Begin 
    type expr = Builtin of builtin | Label of string | Lista of expr list | Number of int | Procedure of (expr -> expr) | Symbol of string | NotImplemented | Ok
    type token = TString of string | TList of token list

    let give_env () = 
      let env = Hashtbl.create 10 
      and dodaj = Procedure (fun (Lista l) -> Number (List.fold_left (fun a (Number h) -> a + h) 0 l))
      and pomnoz = Procedure (fun (Lista l) -> Number (List.fold_left (fun a (Number h) -> a + h) 1  l))
      in
        begin
          Hashtbl.add env "+" dodaj;
          Hashtbl.add env "*" pomnoz;
          env
        end
  end;;
