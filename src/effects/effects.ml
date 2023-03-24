open Printf
open Effect
open Effect.Deep

type _ Effect.t += E : string Effect.t

let comp () =
  print_string "0 ";
  print_string (perform E);
  print_string "3 "

let main () =
  match_with comp () {
    retc = Fun.id;
    effc = (fun (type a) (eff : a Effect.t) ->
      match eff with
      | E -> Some (fun (k : (a, unit) continuation) ->
          print_string "1 "; continue k "2 "; print_string "4 ")
      | _ -> None);
    exnc = (fun e -> raise e);
  }

let _ = main (); print_newline ()
