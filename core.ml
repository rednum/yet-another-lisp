module type CORE =
sig
  type builtin = Quote | List | If | Set | Define | Lambda | Begin 
  type expr = Builtin of builtin | Label of string | Lista of expr list | Number of int | Procedure of (expr -> (string, expr) Hashtbl.t -> expr) | Symbol of string | NotImplemented | Ok

  val give_env : unit -> (string, expr) Hashtbl.t

  exception RuntimeError
end

module Make : CORE =
struct
  type builtin = Quote | List | If | Set | Define | Lambda | Begin 
  type expr = Builtin of builtin | Label of string | Lista of expr list | Number of int | Procedure of (expr -> (string, expr) Hashtbl.t -> expr) | Symbol of string | NotImplemented | Ok
  type token = TString of string | TList of token list

  exception RuntimeError

  let give_env () = 
    let env = Hashtbl.create 10 
      (* Arytmetyka *)
    and dodaj = Procedure (fun args _ -> match args with
                             | Lista l -> Number (List.fold_left (fun a (Number h) -> a + h) 0 l)
                             | _ -> raise RuntimeError)
    and pomnoz = Procedure (fun args _ -> match args with
                              | Lista l -> Number (List.fold_left (fun a (Number h) -> a * h) 1  l)
                              | _ -> raise RuntimeError)
    and odejmij = Procedure (fun args _ -> match args with
                             | Lista (Number h::t) -> Number (List.fold_left (fun a (Number h) -> a - h) h t)
                             | Lista [Number x] -> Number (-x)
                             | _ -> raise RuntimeError)
    and podziel = Procedure (fun args _ -> match args with
                              | Lista l -> Number (List.fold_left (fun a (Number h) -> a / h) 1  l)
                              | _ -> raise RuntimeError)
    and equal = Procedure (fun args _ -> 
                             let rec equal_2 a b =
                               match (a,b) with 
                                 | Number c, Number d -> c == d
                                 | (Lista c), (Lista d) -> ((List.length c == List.length d)
                                                            && (List.for_all2
                                                                  (fun x y -> 
                                                                     (equal_2 x y)) 
                                                                  c d))
                                 | _, _ -> false
                             in 
                               if
                                 begin
                                   match args with
                                     | Lista [l] -> true
                                     | Lista (h::t) -> (snd 
                                                          (List.fold_left
                                                             (fun (el, res) h -> 
                                                                (el, res && equal_2 el h)) 
                                                             (h, true) t))
                                     | _ -> raise RuntimeError
                                 end
                               then Symbol "true"
                               else Lista [])
      (* Listy *)
    and car = Procedure (fun args _ -> match args with
                           | Lista [Lista (a::_)] -> a
                           | _ -> raise RuntimeError)
    and cdr = Procedure (fun args _ -> match args with
                           | Lista [Lista (_::a)] -> Lista a
                           | _ -> raise RuntimeError)
    and cons = Procedure (fun args _ -> match args with 
                            | Lista (head::[Lista tail]) -> Lista (head::tail)
                            | _ -> raise RuntimeError)
                              
    in
      begin
        Hashtbl.add env "+" dodaj;
        Hashtbl.add env "*" pomnoz;
        Hashtbl.add env "-" odejmij;
        Hashtbl.add env "/" podziel;
        Hashtbl.add env "=" equal;
        Hashtbl.add env "car" car;
        Hashtbl.add env "cdr" cdr;
        Hashtbl.add env "cons" cons;
        env
      end
end
