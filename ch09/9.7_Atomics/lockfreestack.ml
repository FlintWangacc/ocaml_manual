module Lockfree_stack : sig
  type 'a t
  val make : unit -> 'a t
  val push : 'a t -> 'a -> unit
  val pop : 'a t -> 'a option
end = struct
  type 'a t = 'a list Atomic.t

  let make () = Atomic.make []

  let rec push r v =
    let s = Atomic.get r in
    if Atomic.compare_and_set r s (v::s) then ()
    else (Domain.cpu_relax (); push r v)
  
  let rec pop r =
    let s = Atomic.get r in
    match s with
    | [] -> None
    | x::xs ->
        if Atomic.compare_and_set r s xs then Some x
        else (Domain.cpu_relax (); pop r)
end