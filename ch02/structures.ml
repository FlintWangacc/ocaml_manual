(* 2.1 Structures *)
module Fifo =
  struct
    type 'a queue = { front: 'a list; rear: 'a list}
    let make front rear =
      match front with
      | [] -> { front = List.rev rear; rear = [] }
      | _ -> { front; rear }
    let empty = { front = []; rear = []}
    let is_empty = function {front = []; _} -> true | _ -> false
    let add x q = make q.front (x :: q.rear)
    exception Empty
    let top = function
      | { front = []; _ } -> raise Empty
      | { front = x :: _; _} -> x
    let pop = function
      | { front = []; _} -> raise Empty
      | { front = _ :: f; rear = r} -> make f r
  end

let at_most_one_element x = match x with
  | Fifo.{ front = ([] | [_]); rear = [] } ->true
  | _ -> false

module FifoOpt =
struct
  include Fifo
  let top_opt q = if is_empty q then None else Some (top q)
  let pop_opt q = if is_empty q then None else Some (pop q)
end

(* 2.2 Signatures *)
module type FIFO =
  sig
    type 'a queue
    val empty : 'a queue
    val add : 'a -> 'a queue -> 'a queue
    val top : 'a queue -> 'a
    val pop : 'a queue -> 'a queue
    exception Empty
  end

  module type FIFO_WITH_OPT =
    sig
      include FIFO
      val top_opt: 'a queue -> 'q option
      val pop_opt: 'a queue -> 'a queue option 
    end

(* 2.3 Functors *)
type comparison = Less | Equal | Greater

module type ORDERED_TYPE =
    sig
      type t
      val compare: t -> t -> comparison
    end

module Set =
    functor (Elt: ORDERED_TYPE) ->
      struct
        type element = Elt.t
        type set = element list
        let empty = []
        let rec add x s =
          match s with
            [] -> [x]
          | hd::tl ->
              match Elt.compare x hd with
                Equal -> s
              | Less -> x :: s
              | Greater -> hd :: add x tl
        let rec member x s =
          match s with
            [] -> false
          | hd::tl ->
              match Elt.compare x hd with
                Equal -> true
              | Less -> false
              | Greater -> member x tl
      end

module OrderedString =
  struct
    type t = string
    let compare x y = if x = y then Equal else if x < y then Less else Greater
  end

module StringSet = Set(OrderedString)

(* Functors and type abstraction *)
module type SETFUNCTOR =
  functor (Elt: ORDERED_TYPE) ->
    sig
      type element = Elt.t
      type set
      val empty : set
      val add : element -> set -> set
      val member : element -> set -> bool
    end

module AbstractSet = (Set : SETFUNCTOR)

module AbstractStringSet = AbstractSet(OrderedString)