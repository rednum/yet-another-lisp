
(** To co musze wpisca do interepretra zeby dzialalo *)
(* module Inttt = Evaluator (Core);; *)

(* #load "str.cma" *)
module Intt = (Interpreter (Evaluator)) (Core);;
