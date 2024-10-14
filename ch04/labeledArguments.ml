(* Chapter 4 Labeled argument *)
let _ = ListLabels.map

let _ = StringLabels.sub

let f ~x ~y = x - y

let _= let x = 3 and y = 2 in f ~x ~y

let f ~x:x1 ~y:y1 = x1 - y1

let _ = f ~x:3 ~y:2

let f ~x ~y = x - y

let _ = f ~y:2 ~x:3

let _ = ListLabels.fold_left

let _ = ListLabels.fold_left [1;2;3] ~init:0 ~f:( + )

let _ = ListLabels.fold_left ~init:0

let hline ~x:x1 ~x:x2 ~y = (x1, x2, y)

let _ = hline ~x:3 ~y:2 ~x:5

(* 4.1 Optional arguments *)
let bump ?(step = 1) x = x + step

let _ = bump 2

let _ = bump ~step:3 2

let test ?(x = 0) ?(y = 0) () ?(z = 0) () = (x, y, z)

let _ = test ()

let _ = test ~x:2 ~z:3 () ()

let _ = test ~y:2 ~z:3 () ()

let _ = test () () ~z:1 ~y:2 ~x:3

(* Error:
  (test () ()) ~z:1 *)

let bump ?step x =
  match step with
  | None -> x * 2
  | Some y -> x + y

let test2 ?x ?y () = test ?x ?y () ()

let _ = test2 ?x:None

(* 4.2 Labels an type inference *)
let h' g= g ~y:2 ~x:3

(* let _ = h' f *)

let bump_it (bump : ?step:int->int->int) x =
  bump ~step:2 x

let _ = bump_it bump 1

let twice f (x : int) = f (f x)

let _ = twice bump 2