(* Chapter 6: Polymorphism and its limitations *)

(* 6.1 Weak polymorphism and mutation *)
(* 6.1.1 Weakly polymorphic types *)
let store = ref None

let another_store = ref None

let _ = another_store := Some 0

let swap store x = match !store with
  | None -> store := Some x; x
  | Some y -> store := Some x; y

let one = swap store 1
let one_again = swap store 2
let two = swap store 3

(*let error = swap (fun x -> x) *)

(* let _ = store := Some (fun x -> x) *)

let _ = store

let option_ref = ref None

(* 6.1.2 The value restriction *)
let make_fake_id () =
  let store = ref None in
  fun x -> swap store x

let fake_id = make_fake_id ()

let not_id = (fun x -> x) (fun x -> x)

let id_again = fun x -> (fun x -> x) (fun x -> x) x

(* 6.1.3 The relaxed value restriction *)
let f () = []

let empty = f ()

(* 6.1.4 Variance and value restriction *)

type x = [ `X ]

type xy = [ `X | `Y ]

let x:x = `X

let x' = (  x :> xy )

let l:x list = [`X; `X]

let l' = ( l :> xy list )

let f: xy -> unit = function
  | `X -> ()
  | `Y -> ()

let f' = (f :> x -> unit)

type 'a proc = 'a -> unit
let f' = (f: xy proc :> x proc)

let x: x ref = ref `X

(* 6.1.5 Abstract data types *)

module type COLLECTION = sig
  type 'a t
  val empty : unit -> 'a t
end

module Implementation = struct
  type 'a t = 'a list
  let empty () = []
end

module List2: COLLECTION = Implementation

let _ = List2.empty ()

module type COLLECTION = sig
  type + 'a t
  val empty: unit -> 'a t
end

module List2: COLLECTION = Implementation

let _ = List2.empty ()