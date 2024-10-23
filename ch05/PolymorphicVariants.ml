(* 5.1 Basic use *)
[`On; `Off]

let _ = `Number 1

let f = function `On -> 1 | `Off -> 0 | `Number n -> n

let _ = List.map f [`On; `Off]

type 'a vlist = [`Nil | `Cons of 'a * 'a vlist]

let rec map f : 'a vlist -> 'b vlist = function
  | `Nil -> `Nil
  | `Cons(a, l) -> `Cons(f a, map f l)

(* 5.2 Advanced use *)
let f = function `A -> `C | `B -> `D | x -> x

let _ = f `E

let f1 = function `A x -> x = 1 | `B -> true | `C -> false
let f2 = function `A x -> x = "a" | `B -> true
let f x = f1 x && f2 x

type 'a wlist = [`Nil | `Cons of 'a * 'a wlist | `Snoc of 'a wlist * 'a]

let wlist_of_vlist l = (l : 'a vlist :> 'a wlist)

let open_vlist l = (l : 'a vlist :> [> 'a vlist])

let _ = fun x -> (x :> [`A|`B|`C])

let split_cases = function
  | `Nil | `Cons _ as x -> `A x
  | `Snoc _ as x -> `B x

let num x = `Num x
let eval1 eval (`Num x) = x
let rec eval x = eval1 eval x

let plus x y = `Plus (x, y)
let eval2 eval = function
  | `Plus(x, y) -> eval x + eval y
  | `Num _ as x -> eval1 eval x
let rec eval x = eval2 eval x

(*let f = function
  | #myvariant -> "myvariant"
  | `Tag3 -> "Tag3"*)

let g1 = function `Tag1 _ -> "Tag1" | `Tag2 _ -> "Tag2"

(*let g = function
  | #myvariant as x -> g1 x
  | `Tag3 -> "Tag3"*)

(* 5.3 Weaknesses of polymorphic variants *)
type abc = [`A | `B | `C]

let f = function
  | `As -> "A"
  | #abc -> "other"

let f : abc -> string = f

(*let f : abc -> string = function
  | `As -> "A"
  | #abc -> "other"*)