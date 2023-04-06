module type Queueable = sig
  type 'a t

  val create : unit -> 'a t
  val push : 'a -> 'a t ->  unit
end


module Exercise (Q : Queueable) = struct
  let exercise queue = 
    for i = 0 to 4 do
      Format.printf "Adding %d\n%!" i;
      Q.push i queue
    done

  let work () =
    let go = Atomic.make false in
    let q = Q.create () in
    let d = Domain.spawn (fun () -> Atomic.set go true; exercise q) in
    while not (Atomic.get go) do Domain.cpu_relax () done;
    exercise q;
    Domain.join d
end

module Seq = Exercise (Queue)
module Par = Exercise (struct
  include Lockfree.Michael_scott_queue
  let push i q = Fun.flip push i q
end)

let () =
  print_endline "With a non domain-safe queue";
  Seq.work ();
  print_endline "With a domain-safe queue";
  Par.work ()
