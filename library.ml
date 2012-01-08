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
    and car = Core.Procedure (fun args _ -> match args with
                           | Core.Lista [Core.Lista (a::_)] -> a
                           | _ -> raise Core.RuntimeError)

    and ttt = Core.Procedure (fun args _ -> match args with
                           | Core.Lista [] -> Core.Number 42
                           | _ -> raise Core.RuntimeError)
    in begin
        ignore (Core.add env "sens-zycia" ttt);
        ignore (Core.add env "car" car);
        env
      end
      
      
end
