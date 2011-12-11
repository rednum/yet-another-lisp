module type CORE =
  sig
    type expr = Builtin of builtin | Label of string | Lista of expr list | Number of int | Procedure of (expr -> expr) | Symbol of string | NotImplemented | Ok
    type builtin = Quote | List | If | Set | Define | Lambda | Begin 
  end;;
