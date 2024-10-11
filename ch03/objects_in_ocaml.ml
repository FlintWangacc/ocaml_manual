(* Chapter 3: Objects in OCaml *)

(* 3.1 Classes and objects *)
class point =
  object
    val mutable x = 0
    method get_x = x
    method move d = x <- x + d
  end

let p = new point

let _ = p#get_x

let _ = p#move 3

let x0 = ref 0

class point =
  object
    val mutable x = incr x0; !x0
    method get_x = x
    method move d = x <- x + d
  end

let _ = new point#get_x

let _ = new point#get_x

class point = fun x_init ->
  object
    val mutable x = x_init
    method get_x = x
    method move d = x <- x + d
  end

class point x_init =
  object
    val mutable x = x_init
    method get_x = x
    method move d = x <- x + d
  end

let _ = new point

let p = new point 7

(*class point x_init =
  object
    val mutable x = x_init
    method get_x = x
    method get offset = x - x_init
    method move d = x <- x + d
  end*)

class adjusted_point x_init =
  let origin = (x_init / 10) * 10 in
  object
    val mutable x = origin
    method get_x = x
    method get_offse = x - origin
    method move d = x <- x + d
  end

class adjusted_point x_init = point ((x_init / 10) * 10)

let new_adjusted_point x_init = new point ((x_init / 10) * 10)

(* 3.2 Immediate objects *)
let p =
  object
    val mutable x = 0
    method get_x = x
    method move d = x <- x + d
  end

let _ = p#get_x

let _ = p#move 3

let _ = p#get_x

let minmax x y =
  if x < y then object method min = x method max = x end
  else object method  min = y method max = x end

(* 3.3 Reference to self *)
class printable_point x_init =
  object (s)
    val mutable x = x_init
    method get_x = x
    method move d = x <- x + d
    method print = print_int s#get_x
  end

let p = new printable_point 7

let _ = p#print

let ints = ref []

(*class my_int =
  object (self)
    method n = 1
    method register = ints := self :: !ints
  end*)

let my_int =
  object (self)
    method n = 1
    method register = ints := self :: !ints
  end

(* 3.4 Iniializers *)
class printable_point x_init =
  let origin = (x_init / 10) * 10 in
  object (self)
    val mutable x = origin
    method get_x = x
    method move d = x <- x + d
    method print = print_int self#get_x
    initializer print_string "new point at "; self#print; print_newline ()
  end

let p = new printable_point 17

(* 3.5 Virtual methods *)
class virtual abstract_point x_init =
  object (self)
    method virtual get_x : int
    method get_offset = self#get_x - x_init
    method virtual move : int -> unit
  end

class point x_init =
  object
    inherit abstract_point x_init
    val mutable x = x_init
    method get_x = x
    method move d = x <- x + d
  end

class virtual abstract_point2 =
  object
    val mutable virtual x : int
    method move d = x <- x + d
  end

class point2 x_init =
  object
    inherit abstract_point2
    val mutable x = x_init
    method get_offset = x - x_init
  end

class restricted_point x_init =
  object (self)
    val mutable x = x_init
    method get_x = x
    method private move d = x <- x + d
    method bump = self#move 1
  end

let p = new restricted_point 0

class point_again1 x =
  object (self)
    inherit restricted_point x
    method virtual move : _
  end

class point_again2 x =
  object (self : < move : _; ..> )
    inherit restricted_point x
  end

class point_again3 x =
  object
    inherit restricted_point x as super
    method move = super#move
  end

(* 3.7 Class interface *)
class type restricted_point_type =
  object
    method get_x : int
    method bump : unit
  end

let _ = fun (x : restricted_point_type) -> x

class restricted_point' x = (restricted_point x : restricted_point_type)

class restricted_point' x = (restricted_point : int -> restricted_point_type)

module type POINT = sig
  class restricted_point' : int ->
    object
      method get_x : int
      method bump : unit
    end
end

module Point : POINT = struct
  class restricted_point' = restricted_point
end

(* 3.8 Inheritance *)
class colored_point x (c : string) =
  object
    inherit point x
    val c = c
    method color = c
  end

let p' = new colored_point 5 "red"

let _ = p'#get_x, p'#color

let get_succ_x p = p#get_x + 1

let _ = get_succ_x p + get_succ_x p

let set_x p = p#set_x

let incr p = set_x p (get_succ_x p)

(* 3.9 Multiple inheritance *)
class printable_colored_point y c =
  object (self)
    val c = c
    method color = c
    inherit printable_point y as super
    method! print =
      print_string "(";
      super#print;
      print_string ", ";
      print_string (self#color);
      print_string ")"
  end

let p' = new printable_colored_point 17 "red"

let _ = p'#print

class another_printable_colored_point y c c' =
    object (self)
    inherit printable_point y
    inherit! printable_colored_point y c
    val! c = c'
  end

(* 3.10 Parameterized class *)
(*class oref x_init =
  object
    val mutable x = x_init
    method get = x
    method set y = x <- y
  end*)

class oref (x_init:int) =
  object
    val mutable x = x_init
    method get = x
    method set y = x <- y
  end

let new_oref x_init =
  object
    val mutable x = x_init
    method get = x
    method set y = x <- y
  end

class ['a] oref x_init =
  object
    val mutable x = (x_init : 'a)
    method get = x
    method set y = x <- y
  end

(*utop only*)
(*let r = new oref 1 in r#set 2; (r#get);;*)

class ['a] oref_succ (x_init:'a) =
  object
    val mutable x = x_init + 1
    method get = x
    method set y = x <- y
  end

class ['a] circle (c : 'a) =
  object
    val mutable center = c
    method center = center
    method set_censter c = center <- c
    method move = (center#move : int -> unit)
  end

class ['a] circle (c : 'a) =
  object
    constraint 'a = #point
    val mutable center = c
    method center = center
    method set_center c = center <- c
    method move = center#move
  end

class ['a] colored_circle c =
  object
    constraint 'a = #colored_point
    inherit ['a] circle c
    method color = center#color
  end

(* 3.11 Polymorphic methods *)
class ['a] intlist (l : int list) =
  object
    method empty = (l = [])
    method fold f (accu : 'a) = List.fold_left f accu l
  end

let l = new intlist [1; 2; 3]

class intlist (l: int list) =
  object
    method empty = (l = [])
    method fold : 'a. ('a -> int -> 'a) -> 'a -> 'a =
      fun f accu -> List.fold_left f accu l
  end

class intlist_rev l =
  object
    inherit intlist l
    method! fold f accu = List.fold_left f accu (List.rev l)
  end

class type ['a] iterator =
  object method fold : ('b -> 'a -> 'b) -> 'b -> 'b end

class intlist' l =
  object (self : int #iterator)
    method empty = (l = [])
    method fold f accu = List.fold_left f accu l
  end

class type point0 = object method get_x : int end

class distance_point x =
  object
    inherit point x
    method distance : 'a. (#point0 as 'a) -> int =
      fun other -> abs (other#get_x - x)
  end

class multi_poly =
  object
    method m1 : 'a. (< n1 : 'b. 'b -> 'b; .. > as 'a) -> _ =
      fun o -> o#n1 true, o#n1 "hello"
    method m2 : 'a 'b. (< n2 : 'b -> bool; .. > as 'a) -> 'b -> _ =
      fun o x -> o#n2 x
  end

(* 3.12 Using coercions *)
let colored_point_to_point cp = (cp : colored_point :> point)

let p = new point 3 and q = new colored_point 4 "blue"

let l = [p; (colored_point_to_point q)]

(*let _ = (p : point :> colored_point)*)

class c0 = object method m = {< >} method n = 0 end

class type c1 = object method m : c1 end

let _ = fun (x:c0) -> (x : c0 :> c1)

class type c2 = object ('a) method m : 'a end

let _ = fun (x:c0) -> (x :> c2)

let to_c1 x = (x :> c1)

let to_c2 x = (x :> c2)

let _ = fun x -> (x :> 'a)

(* class c = object method m = 1 end
and d = object (self)
  inherit c
  method n = 2
  method as_c = (self :> c)
end *)

class c = object (self) method m = (self :> c) end

let all_c = ref []

class c (m : int) =
  object (self)
    method m = m
    initializer all_c := (self :> c) :: !all_c
  end

let rec lookup_obj obj = function [] -> raise Not_found
  | obj' :: l ->
      if (obj :> < >) = (obj' :> < >) then obj' else lookup_obj obj l

let lookup_c obj = lookup_obj obj !all_c

class type c' = object method m : int end

class c : c' = object method m = 1 end
and d = object (self)
  inherit c
  method n = 2
  method as_c = (self :> c')
end

class virtual c' = object method virtual m : int end

class c = object (self) inherit c' method  m = 1 end

type c' = <m : int >

type 'a c'_class = 'a constraint 'a = < m : int; .. >

(* 3.13 Functional objects *)
class functional_point y =
  object
    val x = y
    method get_x = x
    method move d = {< x = x + d>}
    method move_to x = {< x >}
  end

let p = new functional_point 7

let _ = p#get_x

let _ = (p#move 3)#get_x

let _ = (p#move_to 15)#get_x

let _ = p#get_x

class bad_functional_point y =
  object
    val x = y
    method get_x = x
    method move d = new bad_functional_point (x+d)
    method move_to x = new bad_functional_point x
  end

(* 3.14 Cloning objects *)
let p = new point 5

let q = Oo.copy p

let _ = q#move 7; (p#get_x, q#get_x)

let q = Oo.copy p

let _ = p = q, p = p

class copy =
  object
    method copy = {< >}
  end

class copy =
  object (self)
    method copy = Oo.copy self
  end

class backup =
  object (self : 'mytype)
    val mutable copy = None
    method save = copy <- Some {< copy = None >}
    method restore = match copy with Some x -> x | None -> self
  end

class ['a] backup_ref x = object inherit ['a] oref x inherit backup end

let rec get p n = if n = 0 then p # get else get (p # restore) (n-1)

(*let p = new backup_ref 0 in
p # save; p # set 1; p # save; p # set 2;
[get p 0; get p 1; get p 2; get p 3; get p 4]*)

class backup =
  object (self : 'mytype)
    val mutable copy = None
    method save = copy <- Some {< >}
    method restore = match copy with Some x -> x | None -> self
    method clear = copy <- None
  end

(*let p = new backup_ref 0 in
p # save; p # set 1; p # save; p # set 2;
[get p 0; get p 1; get p 2; get p 3; get p 4]*)

(* 3.15 Recursive classes *)
class window =
  object
    val mutable top_widget = (None : widget option)
    method top_widget = top_widget
  end
and widget (w : window) =
  object
    val window = w
    method window = window
  end

(* 3.16 Binary methods *)
class virtual comparable =
  object (_ : 'a)
    method virtual leq : 'a -> bool
  end

class money (x : float) =
  object
    inherit comparable
    val repr = x
    method value = repr
    method leq p = repr <= p#value
  end

class money2 x =
  object
    inherit money x
    method times k = {< repr = k *. repr >} 
  end

let min (x : #comparable) y =
  if x#leq y then x else y

let _ = (min (new money 1.3) (new money 3.1))#value

let _ = (min (new money2 5.0) (new money2 3.14))#value

class money x =
  object (self : 'a)
    val repr = x
    method value = repr
    method print = print_float repr
    method times k = {< repr = k *. x >}
    method leq (p : 'a) = repr <= p#value
    method plus (p : 'a) = {< repr = x +. p #value >}
  end