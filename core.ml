module type CORE =
sig
  type builtin = Quote | List | If | Set | Define | Lambda | Begin 
  (* type environment *)
  (* type expr = Builtin of builtin | Label of string | Lista of expr list | Number of int | Procedure of (expr -> environment -> expr) | Symbol of string | NotImplemented | Ok *)
  type expr = Builtin of builtin | Label of string | Lista of expr list | Number of int | Procedure of (expr -> environment -> expr) | Symbol of string | NotImplemented | Ok
  (* type token = TString of string | TList of token list *)
  and environment = Global of (string, expr) Hashtbl.t | Local of (string, expr) Hashtbl.t * environment


  val give_empty : unit -> environment
  val give_env : unit -> environment
  val add : environment -> string -> expr -> expr
  val find : environment -> string -> expr
  val replace : environment -> string -> expr -> expr

  val new_scope : environment -> expr list -> expr list -> environment

  exception RuntimeError
end

module Make : CORE =
struct
  type builtin = Quote | List | If | Set | Define | Lambda | Begin 
  type expr = Builtin of builtin | Label of string | Lista of expr list | Number of int | Procedure of (expr -> environment -> expr) | Symbol of string | NotImplemented | Ok
  (* type token = TString of string | TList of token list *)
  and environment = Global of (string, expr) Hashtbl.t | Local of (string, expr) Hashtbl.t * environment

  exception RuntimeError

  let give_empty () = 
    Global (Hashtbl.create 10)

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
    and mniejszy = Procedure (fun args _ -> match args with
                                | Lista (Number a::[Number b]) -> 
                                    if a < b
                                    then Symbol "true"
                                    else Lista []
                                | _ -> raise RuntimeError)
    and equal = 
      Procedure (fun args _ -> 
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
                           | Lista (h::t) -> 
                               (snd 
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
        Hashtbl.add env "<" mniejszy;
        Hashtbl.add env "car" car;
        Hashtbl.add env "cdr" cdr;
        Hashtbl.add env "cons" cons;
        Global env
      end


  let rec find env label =
    match env with 
      | Global table -> Hashtbl.find table label
      | Local (table, outer) -> 
          if Hashtbl.mem table label
          then Hashtbl.find table label
          else find outer label
           
  let rec add env label value = 
    let put = 
      match env with
        | Global table -> Hashtbl.add table
        | Local (table, _) -> Hashtbl.add table
    in put label value; value

  let rec replace env label (value : expr) =
    let put l v = 
      match env with
        | Global table -> Hashtbl.replace table l v
        | Local (table, outer) -> 
          if Hashtbl.mem table l
          then Hashtbl.replace table l v
          else ignore (replace outer l v)
    in put label value; value

  let new_scope env vars args = 
    let table = Hashtbl.create (List.length vars)
    in let add_one var arg =
      match var with
        | Label v -> Hashtbl.add table v arg
        | _ -> raise RuntimeError
    in ignore (List.map2 add_one vars args);
      Local (table, env)
           
end
