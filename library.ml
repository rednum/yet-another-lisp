open Core

module type LIBRARY =
sig 
  type environment
  type expression

  val create_globals : unit -> environment
end


(* module Make (Core : CORE) : LIBRARY = *)
module Make (Core : CORE) : LIBRARY with type expression = Core.expr and type environment = Core.environment = 
struct 
  type expression = Core.expr
  type environment = Core.environment

  let create_globals () = 
    let env = Core.give_empty () 
      (* Arytmetyka *)
    and dodaj = Core.Procedure
      (fun args _ -> match args with
         | Core.Lista l -> Core.Number (List.fold_left (fun a (Core.Number h) -> a + h) 0 l)
         | _ -> raise Core.RuntimeError)
    and pomnoz = Core.Procedure 
      (fun args _ -> match args with
         | Core.Lista l -> Core.Number (List.fold_left (fun a (Core.Number h) -> a * h) 1  l)
         | _ -> raise Core.RuntimeError)
    and odejmij = Core.Procedure 
      (fun args _ -> match args with
         | Core.Lista (Core.Number h::t) -> Core.Number (List.fold_left (fun a (Core.Number h) -> a - h) h t)
         | Core.Lista [Core.Number x] -> Core.Number (-x)
         | _ -> raise Core.RuntimeError)
    and podziel = Core.Procedure 
      (fun args _ -> match args with
         | Core.Lista l -> Core.Number (List.fold_left (fun a (Core.Number h) -> a / h) 1  l)
         | _ -> raise Core.RuntimeError)
    and mniejszy = Core.Procedure 
      (fun args _ -> match args with
         | Core.Lista [Core.Number a] -> Core.Symbol "true"
         | Core.Lista (Core.Number a::tail) -> 
             if snd (List.fold_left 
                       (fun (last, res) (Core.Number next) -> (next, res && (last < next)))
                       (a, true)
                       tail)
             then Core.Symbol "true"
             else Core.Lista []
         | _ -> raise Core.RuntimeError)
    and wiekszy = Core.Procedure 
      (fun args _ -> match args with
         | Core.Lista [Core.Number a] -> Core.Symbol "true"
         | Core.Lista (Core.Number a::tail) -> 
             if snd (List.fold_left 
                       (fun (last, res) (Core.Number next) -> (next, res && (last > next)))
                       (a, true)
                       tail)
             then Core.Symbol "true"
             else Core.Lista []
         | _ -> raise Core.RuntimeError)
    and equal = 
      Core.Procedure 
        (fun args _ -> 
           let rec equal_2 a b =
             match (a,b) with 
               | Core.Number c, Core.Number d -> c == d
               | (Core.Lista c), (Core.Lista d) -> 
                   ((List.length c == List.length d)
                    && (List.for_all2
                          (fun x y -> 
                             (equal_2 x y)) 
                          c d))
               | _, _ -> false
           in 
             if
               begin
                 match args with
                   | Core.Lista [l] -> true
                   | Core.Lista (h::t) -> 
                       (snd 
                          (List.fold_left
                             (fun (el, res) h -> 
                                (el, res && equal_2 el h)) 
                             (h, true) t))
                   | _ -> raise Core.RuntimeError
               end
             then Core.Symbol "true"
             else Core.Lista [])
        (* Listy *)
    and car = Core.Procedure 
      (fun args _ -> match args with
         | Core.Lista [Core.Lista (a::_)] -> a
         | _ -> raise Core.RuntimeError)
    and cdr = Core.Procedure 
      (fun args _ -> match args with
         | Core.Lista [Core.Lista (_::a)] -> Core.Lista a
         | _ -> raise Core.RuntimeError)
    and cons = Core.Procedure 
      (fun args _ -> match args with 
         | Core.Lista (head::[Core.Lista tail]) -> Core.Lista (head::tail)
         | _ -> raise Core.RuntimeError)
      
    in let builtins = 
        ["+", dodaj;
         "*", pomnoz;
         "-", odejmij;
         "/", podziel;
         "=", equal;
         "<", mniejszy;
         ">", wiekszy;
         "car", car;
         "cdr", cdr;
         "cons", cons]
    in begin
        ignore (List.map (fun (name, func) -> Core.add env name func) builtins);
        env
      end

        
        
end
