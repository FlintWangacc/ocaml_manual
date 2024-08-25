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