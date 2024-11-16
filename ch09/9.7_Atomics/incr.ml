let twice_in_parallel f =
  let d1 = Domain.spawn f in
  let d2 = Domain.spawn f in
  Domain.join d1;
  Domain.join d2

let plain_ref n =
  let r = ref 0 in
  let f () = for _i=1 to n do incr r done in
  twice_in_parallel f;
  Printf.printf "Non-atomic ref count: %d\n" !r

let atomic_ref n =
  let r = Atomic.make 0 in
  let f () = for _i=1 to n do Atomic.incr r done in
  twice_in_parallel f;
  Printf.printf "Atomic ref count: %d\n" (Atomic.get r)

let main () =
  let n = try int_of_string Sys.argv.(1) with _ -> 1 in
  plain_ref n;
  atomic_ref n

let _ = main ()