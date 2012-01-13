module type CORE =
sig
  type builtin = Quote | List | If | Set | Define | Lambda | Begin 
  type expr = Builtin of builtin | Label of string | Lista of expr list | Number of int | Procedure of (expr -> environment -> expr) | Symbol of string 

  and environment = Global of (string, expr) Hashtbl.t | Local of (string, expr) Hashtbl.t * environment
      

  val give_empty : unit -> environment
  val add : environment -> string -> expr -> expr
  val find : environment -> string -> expr
  val replace : environment -> string -> expr -> expr

  val new_scope : environment -> expr list -> expr list -> environment

  exception RuntimeError
end

module Make : CORE
