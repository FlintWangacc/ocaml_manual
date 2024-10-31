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

(* 6.2 Polymorphic recursion *)
type 'a regular_nested = List of 'a list | Nested of 'a regular_nested list

let l = Nested[ List [1]; Nested [List [2; 3]]; Nested [Nested[]]]

let rec maximal_depth = function
  | List _ -> 1
  | Nested [] -> 0
  | Nested (a::q) -> 1 + max (maximal_depth a) (maximal_depth (Nested q))

type 'a nested = List of 'a list | Nested of 'a list nested

(* 6.2.1 Explicitly polymorphic annotations *)
let rec depth: 'a. 'a nested -> int = function
  | List _ -> 1
  | Nested n -> 1 + depth n

let sum: 'a -> 'b -> 'c = fun x y -> x + y

let rec depth: 'a. 'a nested -> _ = function
  | List _ -> 1
  | Nested n -> 1 + depth n

(* 6.2.2 More examples *)
let len nested =
  let map_and_sum f = List.fold_left (fun acc x -> acc + f x) 0 in
  let rec len: 'a. ('a list -> int ) -> 'a nested -> int =
  fun nested_len n ->
    match n with
    | List l -> nested_len l
    | Nested n -> len (map_and_sum nested_len) n
  in
  len List.length nested

let _ = len (Nested(Nested (List [ [ [1;2]; [3] ]; [[]; [4];[5;6;7]];[[]]])))

let shape n =
  let rec shape: 'a 'b. ('a nested -> int nested) ->
    ('b list list -> 'a list) -> 'b nested -> int nested
    = fun nest nested_shape ->
      function
      | List l -> raise
        (Invalid_argument "shape equires nested_list of depth greater than !")
      | Nested (List l) -> nest @@ List (nested_shape l)
      | Nested n ->
        let nested_shape = List.map nested_shape in
        let nest x = nest (Nested x) in
        shape nest nested_shape n in
  shape (fun n -> n) (fun l -> List.map List.length l ) n


let _ = shape (Nested(Nested (List [ [ [1;2]; [3] ]; [[]; [4];[5;6;7]];[[]]])))

(* 6.3 Higher-rank polymorphic functions *)
let average_depth x y = (depth x + depth y) / 2

let average_len x y = (len x + len y) / 2

let one = average_len (List [2]) (List [[]])

let average f x y = (f x + f y) / 2

let _ = average_len (List [2]) (List [[]])

(*let _ = average len (List[2]) (List [[]])*)

type 'a nested_reduction = {f:'elt. 'elt nested -> 'a}

let boxed_len = { f = len }

let obj_len = object method f:'a. 'a nested -> 'b = len end

let average nsm x y = (nsm.f x + nsm.f y) / 2

let average (obj:<f:'a. 'a nested -> _ > ) x y = (obj#f x + obj#f y) / 2