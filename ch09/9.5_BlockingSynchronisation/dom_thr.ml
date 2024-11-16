let m = Mutex.create ()
let r = ref None

let task () =
  let my_thr_id = Thread.(id (self ())) in
  let my_dom_id :> int = Domain.self () in
  Mutex.lock m;
  begin match !r with
  | None ->
      Printf.printf "Thread %d running on domain %d saw initial wite\n%!"
        my_thr_id my_dom_id
  | Some their_thr_id ->
      Printf.printf "Thread %d running on domain %d saw the write bt thread %d\n%!"
        my_thr_id my_dom_id their_thr_id;
  end;
  r := Some my_thr_id;
  Mutex.unlock m

let task' () =
  let t = Thread.create task () in
  task ();
  Thread.join t

let main () =
  let d = Domain.spawn task' in
  task' ();
  Domain.join d

let _ = main ()