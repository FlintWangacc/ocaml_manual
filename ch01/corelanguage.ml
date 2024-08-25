(* 1.1 Basics *)

1 + 2 * 3

let pi = 4.0 *. atan 1.0

let square x = x *. x

let _ = square (sin pi) +. square (cos pi)

(* 1.0 * 2 *)

let rec fib n =
  if n < 2 then n else fib (n - 1) + fib (n - 2)

let _ = fib 10

(* 1.2 Data types *)
let _ = (1 < 2) = false

let one = if true then 1 else 2

let _ =  'a'

let _ = int_of_char '\n'

let _ = "Hello" ^ " " ^ "world"

let _ = {|This is a quoted string, here, neithter \ nor " are special characters|}

let _ = {|"\\"|} = "\"\\\\\""

let _ = {delimiter|the end of this |}quoted string is here|delimiter}
= "the end of this |}quoted string is here"

let l = ["is"; "a"; "tale"; "told"; "etc."]

let _ = "Life" :: l

let rec sort lst =
  match lst with
    [] -> []
  | head :: tail -> insert head (sort tail)
  and insert elt lst =
    match lst with
      [] -> [elt]
    | head :: tail -> if elt <= head then elt :: lst else head :: insert elt tail

let _ = sort l

let _ = sort [6; 2; 5; 3]

let _ = sort [3.14; 2.718]

(* 1.3 Functions as values *)

let deriv f dx = fun x -> (f (x +. dx) -. f x) /. dx

let sin' = deriv sin 1e-6

let _ = sin' pi

let compose f g = fun x -> f (g x)

let cos2 = compose square cos

let _ = List.map (fun n -> n * 2 + 1) [0;1;2;3;4]

let rec map f l =
  match l with
    [] -> []
  | hd :: tl -> f hd :: map f tl

(* 1.4 Records and variants *)
type ratio = {num: int; denom: int}

let add_ratio r1 r2 =
  {num = r1.num * r2.denom + r2.num * r1.denom;
   denom = r1.denom * r2.denom}

let _ = add_ratio {num=1; denom=3} {num=2; denom=5}

let integer_part r =
  match r with
    {num=num; denom=denom} -> num / denom

let integer_part {num=num; denom=denom} = num / denom

let get_denom {denom=denom} = denom

let get_num {num=num; _} = num

let integer_part {num; denom} = num / denom

let ratio num denom = {num; denom}

let integer_product integer ratio = { ratio with num = integer * ratio.num }

type number = Int of int | Float of float | Error

type sign = Positive | Negative

let sign_int n = if n >= 0 then Positive else Negative

let add_num n1 n2 =
  match (n1, n2) with
    (Int i1, Int i2) ->
      if sign_int i1 = sign_int i2 && sign_int (i1 + i2) <> sign_int i1
      then Float(float i1 +. float i2)
      else Int(i1 + i2)
  | (Int i1, Float f2) -> Float(float i1 +. f2)
  | (Float f1, Int i2) -> Float(f1 +. float i2)
  | (Float f1, Float f2) -> Float (f1 +. f2)
  | (Error, _) -> Error
  | (_, Error) -> Error

let _ = add_num (Int 123) (Float 3.14159)

type 'a option = Some of 'a | None

let safe_square_root x = if x >= 0. then Some (sqrt x) else None

type 'a btree = Empty | Node of 'a * 'a btree * 'a btree

let rec member x btree =
  match btree with
    Empty -> false
  | Node (y, left, right) ->
      if x = y then true else
      if x < y then member x left else member x right

let rec insert x btree =
  match btree with
    Empty -> Node (x, Empty, Empty)
  | Node(y, left, right) ->
      if x <= y then Node(y, insert x left, right)
                else Node(y, left, insert x right)

(* 1.4.1 Record and variant disambiguation *)
type first_record = { x:int; y:int; z:int}
type middle_record = {x:int; z:int}
type last_record = {x:int}

type first_variant = A | B | C
type last_variant = A

let look_at_x_then_z (r:first_record) =
  let x = r.x in
  x + r.z

let permute (x:first_variant) = match x with
  | A -> (B:first_variant)
  | B -> A
  | C -> C

type wrapped = First of first_record
let f (First r) = r, r.x

let project_and_rotate {x; y; _} = {x= - y; y=x; z=0}

let look_at_xz {x; z} = x;;

(*let look_at_x_then_y r =
  let x = r.x in (* Ocaml deduces [r:last_record] *)
  x + r.y*)

(*let is_a_or_b x = match x with
  | A -> true (* OCaml infers [x:last_variant] *)
  | B -> true*)

(* Imperative features *)
let add_vect v1 v2 =
  let len = min (Array.length v1) (Array.length v2) in
  let res = Array.make len 0.0 in
  for i = 0 to len - 1 do
    res.(i) <- v1.(i) +. v2.(i)
  done;
  res

let _ = add_vect [| 1.0; 2.0 |] [|3.0; 4.0|]

type mutable_point = {mutable x : float; mutable y: float}

let translate p dx dy =
  p.x <- p.x +. dx; p.y <- p.y +. dy

let mypoint = { x = 0.0; y = 0.0 }

let _ = translate mypoint 1.0 2.0

let _ = mypoint

let insertion_sort a =
  for i = 1 to Array.length a - 1 do
    let val_i = a.(i) in
    let j = ref i in
    while !j > 0 && val_i < a.(!j - 1) do
      a.(!j) <- a.(!j - 1);
      j := !j - 1
    done;
    a.(!j) <- val_i
  done

let current_rand = ref 0

let random () =
  current_rand := !current_rand * 25713 + 1345;
  !current_rand

(*type 'a ref = { mutable contents: 'a }

let ( ! ) r = r.contents

let ( := ) r newval = r.contents <- newval

type idref = { mutable id: 'a.'a -> 'a}

let r = {id = fun x -> x}

let g s = (s.id 1, s.id true)

let _ = r.id <- (fun x -> print_string "called id\n"; x) 

let _ = g r*)

exception Empty_list

let head l =
  match l with
    [] -> raise Empty_list
  | hd :: tl -> hd

(*let _ = head [1; 2]

let _ = head []

let _ = List.assoc 1 [(0, "zero"); (1, "one")]

let _ = List.assoc 2 [(0, "zero"); (1, "one")]*)

let name_of_binary_digit digit =
  try
    List.assoc digit [0,"zero"; 1, "one"]
  with Not_found ->
    "not a binary digit"

let _ = name_of_binary_digit 0

let _ = name_of_binary_digit (-1)

let rec first_named_value values names =
  try
    List.assoc (head values) names
  with
  | Empty_list -> "no named value"
  | Not_found -> first_named_value (List.tl values) names

let _ = first_named_value [0; 10] [1, "one"; 10, "ten"]

let temporarily_set_reference ref newval funct =
  let oldval = !ref in
  try
    ref := newval;
    let res = funct () in
    ref := oldval;
    res
  with x ->
    ref := oldval;
    raise x

let assoc_may_map f x l =
  match List.assoc x l with
  | exception Not_found -> None
  | y -> f y

let fixpoint f x =
  let exception Done in
  let x = ref x in
  try while true do
      let y = f !x in
      if !x = y then raise Done else x := y
     done
  with Done -> !x

(* 1.7 Lazy expressions *)
let lazy_two = lazy (print_endline "lazy_two evalution"; 1 + 1)

let _ = Lazy.force lazy_two

let _ = lazy_two

let _ = Lazy.force lazy_two

let lazy_l = lazy ([1; 2] @ [3; 4])

let lazy_l = lazy_l

let maybe_eval lazy_guard lazy_expr =
  match lazy_guard, lazy_expr with
  | lazy false, _ -> "matches if (Lazy.force lazy_guard = false); lazy_expr not forced"
  | lazy true, lazy _ -> "matches if (Lazy.force lazy_guard = true); lazy_expr forced"

(* 1.8 Symbolic processing of expressions *)
type expression =
    Const of float
  | Var of string
  | Sum of expression * expression
  | Diff of expression * expression
  | Prod of expression * expression
  | Quot of expression * expression

exception Unbound_variable of string

let rec eval env exp =
  match exp with
    Const c -> c
  | Var v ->
      (try List.assoc v env with Not_found -> raise (Unbound_variable v))
  | Sum (f, g) -> eval env f +. eval env g
  | Diff (f, g) -> eval env f -. eval env g
  | Prod (f, g) -> eval env f *. eval env g
  | Quot (f, g) -> eval env f /. eval env g

let _ = eval [("x", 1.0); ("y", 3.14)] (Prod(Sum(Var "x", Const 2.0), Var "y"))

let rec deriv exp dv =
  match exp with
    Const c -> Const 0.0
  | Var v -> if v = dv then Const 1.0 else Const 0.0
  | Sum (f, g) -> Sum(deriv f dv, deriv g dv)
  | Diff (f, g) -> Diff(deriv f dv, deriv g dv)
  | Prod (f, g) -> Sum(Prod(f, deriv g dv), Prod(deriv f dv, g))
  | Quot (f, g) -> Quot(Diff(Prod(deriv f dv, g), Prod (f, deriv g dv)),  Prod(g, g))

let _ = deriv (Quot (Const 1.0, Var "x")) "x"

(* 1.9 Pretty-printing *)
let print_expr exp =
  let open_paren prec op_prec =
    if prec > op_prec then print_string "(" in
  let close_paren prec op_prec =
    if prec > op_prec then print_string ")" in
  let rec print prec exp =
    match exp with
      Const c -> print_float c
    | Var v -> print_string v
    | Sum (f, g) ->
        open_paren prec 0;
        print 0 f; print_string " + "; print 0 g;
        close_paren prec 0
    | Diff (f, g) ->
        open_paren prec 0;
        print 0 f; print_string " - "; print 1 g;
        close_paren prec 0
    | Prod(f, g) ->
        open_paren prec 2;
        print 2 f; print_string " * "; print 2 g;
        close_paren prec 2
    | Quot(f, g) ->
        open_paren prec 2;
        print 2 f; print_string " / "; print 3 g;
        close_paren prec 2
    in print 0 exp

let e = Sum(Prod (Const 2.0, Var "x"), Const 1.0)

let _ = print_expr e; print_newline ()

let _ = print_expr (deriv e "x"); print_newline ()

let _ = Printf.printf "%i + %i is an integer value, %F * %F is a float, %S\n"
        3 2 4.5 1. "this is a string"

(* let _ = Printf.printf "Float value %F" 42*)

let pp_int ppf n = Printf.fprintf ppf "%d" n

let _ = Printf.printf "Outputting an integer using a custom printer: %a" pp_int 42

let pp_option printer ppf = function 
  | None -> Printf.fprintf ppf "None"
  | Some v -> Printf.fprintf ppf "Some (%a)" printer v

let _ = Printf.fprintf stdout
          "This current setting is %a. \nThere is only %a\n"
          (pp_option pp_int) (Some 3)
          (pp_option pp_int) None

let pp_expr ppf expr =
  let open_paren prec op_prec output =
    if prec > op_prec then Printf.fprintf output "%s" "(" in
  let close_paren prec op_prec output =
    if prec > op_prec then Printf.fprintf output "%s" ")" in
  let rec print prec ppf expr =
    match expr with
    | Const c -> Printf.fprintf ppf "%F" c
    | Var v -> Printf.fprintf ppf "%s" v
    | Sum (f, g) ->
        open_paren prec 0 ppf;
        Printf.fprintf ppf "%a + %a" (print 0) f (print 0) g;
        close_paren prec 0 ppf
    | Diff (f, g) ->
        open_paren prec 0 ppf;
        Printf.fprintf ppf "%a - %a" (print 0) f (print 1) g;
        close_paren prec 0 ppf
    | Prod(f, g) ->
        open_paren prec 2 ppf;
        Printf.fprintf ppf "%a * %a" (print 2) f (print 2) g;
        close_paren prec 2 ppf
    | Quot(f, g) ->
        open_paren prec 2 ppf;
        Printf.fprintf ppf "%a / %a" (print 2) f (print 3) g;
        close_paren prec 2 ppf
  in print 0 ppf expr

let _ = pp_expr stdout e; print_newline ()

let _ = pp_expr stdout (deriv e "x"); print_newline ()

let str : _ format =
    "%i is an integer value, %F is float, %S\n"

let _ = Printf.printf str 3 4.5 "string value"