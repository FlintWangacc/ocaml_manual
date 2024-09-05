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

class point x_init =
  object
    val mutable x = x_init
    method get_x = x
    method get offset = x - x_init
    method move d = x <- x + d
  end

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