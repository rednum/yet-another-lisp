module type CORE =
sig
  type builtin = Quote | List | If | Set | Define | Lambda | Begin 
  type expr = Builtin of builtin | Label of string | Lista of expr list | Number of int | Procedure of (expr -> (string, expr) Hashtbl.t -> expr) | Symbol of string | NotImplemented | Ok

  val give_env : unit -> (string, expr) Hashtbl.t
end

module Make : CORE
