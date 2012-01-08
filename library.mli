open Core

module type LIBRARY =
sig 
  type environment
  type expression

  val create_globals : unit -> environment
end

module Make (Core : CORE) : LIBRARY with type expression = Core.expr and type environment = Core.environment
