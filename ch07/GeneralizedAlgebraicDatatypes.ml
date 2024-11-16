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

(* 7.4 Advanced examples *)
type _ typ =
  | Int : int typ
  | String : string typ
  | Pair : 'a typ * 'b typ -> ('a * 'b) typ

let rec to_string : type t. t typ -> t -> string =
  fun t x ->
  match t with
  | Int -> Int.to_string x
  | String -> Printf.sprintf "%S" x
  | Pair (t1, t2) ->
      let (x1, x2) = x in
      Printf.sprintf "(%s,%s)" (to_string t1 x1) (to_string t2 x2)

type (_,_) eq = Eq : ('a,'a) eq

let rec eq_type : type a b .a typ ->b typ -> (a,b) eq option =
  fun a b ->
  match a, b with
  | Int, Int -> Some Eq
  | String, String -> Some Eq
  | Pair(a1,a2), Pair(b1,b2) ->
      begin match eq_type a1 b1, eq_type a2 b2 with
      | Some Eq, Some Eq -> Some Eq
      | _ -> None
      end
  | _ -> None

type dyn = Dyn : 'a typ * 'a -> dyn

let get_dyn : type a. a typ -> dyn -> a option =
  fun a (Dyn(b,x)) ->
  match eq_type a b with
  | None -> None
  | Some Eq -> Some x

(* 7.6 Explict naming of existentials *)
type _ closure = Closure : ('a -> 'b) * 'a -> 'b closure
let eval = fun (Closure (type a) (f, x : (a -> _) * _)) -> f (x : a)

(* 7.7 Equations on non-local abstract types *)
module M : sig type t val x : t val e : (t, int) eq end = struct
  type t = int
  let x = 33
  let e = Eq
end