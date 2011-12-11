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
