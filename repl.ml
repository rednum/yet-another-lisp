open Core
open Evaluator
open Parser
module Corr = (Core.Make)

module Pars = (Parser.Make (Evaluator.Make (Corr) (Library.Make (Corr))))

let reader() = Pervasives.read_line()

let rec main_loop() =
  Pervasives.print_string "> ";
  flush stdout;

  while true do
    let wyrazenie = reader() in
      Pervasives.print_string (Pars.prompt(wyrazenie));
      (* Pervasives.print_string wyrazenie; *)
      Pervasives.print_string "\n";
      Pervasives.print_string "> ";
      flush stdout;

  done;
;;

try
  main_loop ();
with 
  | End_of_file -> Pervasives.print_string "goodbye!\n";
;;
