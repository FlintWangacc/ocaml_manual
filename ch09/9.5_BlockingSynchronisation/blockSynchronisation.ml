module Blocking_stack : sig
  type 'a t
  val make : unit -> 'a t
  val push : 'a t -> 'a -> unit
  val pop : 'a t -> 'a
end = struct
  type 'a t = {
    mutable contents: 'a list;
    mutex : Mutex.t;
    nonempty : Condition.t
  }

  let make () = {
    contents = [];
    mutex = Mutex.create ();
    nonempty = Condition.create ()
  }

  let push r v =
    Mutex.lock r.mutex;
    r.contents <- v::r.contents;
    Condition.signal r.nonempty;
    Mutex.unlock r.mutex

  let pop r =
    Mutex.lock r.mutex;
    let rec loop () =
      match r.contents with
      | [] ->
          Condition.wait r.nonempty r.mutex;
          loop ()
      | x::xs -> r.contents <- xs; x
    in
    let res = loop () in
    Mutex.unlock r.mutex;
    res
end