external print_and_call_ocaml_h : unit -> unit = "print_and_call_ocaml_h"

exception ExnA
exception ExnB

let r = ref 0
let [@inline never] race () = ignore @@ !r

let [@inline never] i () =
  raise ExnB

let [@inline never] h () =
  i ()

let _ = Callback.register "ocaml_h" h

let [@inline never] g () =
  print_and_call_ocaml_h ()

let [@inline never] f () =
  try g () with
  | ExnB -> race ()

let () =
  let d = Domain.spawn (fun () -> Unix.sleep 1; r := 1) in
  f (); Unix.sleep 1;
  Domain.join d
