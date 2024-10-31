type _ term =
  | Int : int -> int term
  | Add : (int -> int -> int) term
  | App : ('b -> 'a) term * 'b term -> 'a term

let rec eval : type a. a term -> a = function
  | Int n     -> n
  | Add       -> (fun x y -> x+y)
  | App(f, x) -> (eval f) (eval x)

let two = eval (App (App (Add, Int 1), Int 1))

(*
let rec eval (type a) : a term -> a = function
  | Int n       -> n
  | Add         -> (fun x y -> x + y)
  | App (f, x)  -> (eval f) (eval x)
*)

(* 7.2 Type inference *)
let rec sum : type a. a term -> _ = fun x ->
  let y =
    match x with
    | Int n -> n
    | Add   -> 0
    | App(f,x) -> sum f + sum x
  in y + 1

let get_int : int term -> int = function
  | Int n   -> n
  | App (_,_)    -> 0

(* 7.3 Refutation cases *)
type _ t =
  | Int : int t
  | Bool : int t

let deep : (char t * int) option -> char = function
  | None -> 'c'
  | _ -> .