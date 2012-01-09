module type CORE =
sig
  type builtin = Quote | List | If | Set | Define | Lambda | Begin 
  (* type environment *)
  (* type expr = Builtin of builtin | Label of string | Lista of expr list | Number of int | Procedure of (expr -> environment -> expr) | Symbol of string | NotImplemented | Ok *)
  type expr = Builtin of builtin | Label of string | Lista of expr list | Number of int | Procedure of (expr -> environment -> expr) | Symbol of string
  (* type token = TString of string | TList of token list *)
  and environment = Global of (string, expr) Hashtbl.t | Local of (string, expr) Hashtbl.t * environment

  val give_empty : unit -> environment
  (* val give_env : unit -> environment *)
  val add : environment -> string -> expr -> expr
  val find : environment -> string -> expr
  val replace : environment -> string -> expr -> expr

  val new_scope : environment -> expr list -> expr list -> environment

  exception RuntimeError
end

module Make : CORE =
struct
  type builtin = Quote | List | If | Set | Define | Lambda | Begin 
  type expr = Builtin of builtin | Label of string | Lista of expr list | Number of int | Procedure of (expr -> environment -> expr) | Symbol of string 
  and environment = Global of (string, expr) Hashtbl.t | Local of (string, expr) Hashtbl.t * environment

  exception RuntimeError

  let give_empty () = 
    Global (Hashtbl.create 10)

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
