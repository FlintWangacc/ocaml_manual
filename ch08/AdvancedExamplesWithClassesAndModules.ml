module type MONEY =
  sig
    type t
    class c : float ->
      object ('a)
        val repr : t
        method value : t
        method print : unit
        method times : float -> 'a
        method leq : 'a -> bool
        method plus : 'a -> 'a
      end
  end

(*module Euro  =
  struct
    type t = float
    class c x =
      object (self : 'a)
        val repr = x
        method value = repr
        method print = print_float_repr
        method times k = {< repr = k *. x>}
        method leq (p : 'a) = repr <= p#value
        method plus (p : 'a) = {< repr = x +. p#value >}
      end
  end*)

module Euro : MONEY =
  struct
    type t = float
    class c x =
      object (self : 'a)
      val repr = x
      method value = repr
      method print = print_float repr
      method times k = {< repr = k *. x >}
      method leq (p : 'a) = repr <= p#value
      method plus (p : 'a) = {< repr = x +. p#value >}
    end
  end

(* Chapter 8: Advanced examples with classes and modules *)

(* 8.1 Extended example: bank accounts *)

let euro = new Euro.c

let zero = euro 0.

let neg x = x#times (-1.)


class account =
  object
    val mutable balance = zero
    method balance = balance
    method deposit x = balance <- balance # plus x
    method withdraw x =
      if x#leq balance then (balance <- balance # plus (neg x); x) else zero
    end

class account_with_interests =
  object (self)
    inherit account
    method private interest = self # deposit (self # balance # times 0.03)
  end

class safe_account =
  object
    inherit account
    method deposit x = if zero#leq x then balance <- balance#plus x
  end

class safe_account =
  object
  inherit account as unsafe
  method deposit x =
    if zero#leq x then unsafe # deposit x
    else raise (Invalid_argument "deposit")
  end

type 'a operation = Deposit of 'a | Retrieval of 'a

class account_with_history =
  object (self)
    inherit safe_account as super
    val mutable history = []
    method private trace x = history <- x :: history
    method deposit x = self#trace (Deposit x); super#deposit x
    method withdraw x = self#trace (Retrieval x); super#withdraw x
    method history = List.rev history
  end

class account_with_deposit x =
  object
    inherit account_with_history
    initializer balance <- x
  end

class account_with_deposit x =
  object (self)
    inherit account_with_history
    initializer self#deposit x
  end

(*let ccp = new account_with_deposit (euro 100.) in
let _balance = ccp#withdraw (euro 50.) in
ccp#history*)

let close c = c#withdraw c#balance

let today () = (01,01,2000)
module Account (M:MONEY) =
  struct
    type m = M.c
    let m = new M.c
    let zero = m 0.
    class bank =
      object(self)
        val mutable balance = zero
        method balance = balance
        val mutable history = []
        method private trace x = history <- x::history
        method deposit x =
          self#trace (Deposit x);
          if zero#leq x then balance <- balance # plus x
          else raise (Invalid_argument "deposit")
        method withdraw x =
          if x#leq balance then
            (balance <- balance # plus (neg x); self#trace (Retrieval x); x)
          else zero
        method history = List.rev history
      end
    
    class type client_view =
      object
        method deposit : m -> unit
        method history : m operation list
        method withdraw : m -> m
        method balance : m
      end

    class virtual check_client x =
      let y = if (m 100.)#leq x then x
      else raise (Failure "Insufficient initial deposit") in
      object (self)
        initializer self#deposit y
        method virtual deposit : m -> unit
      end
    
    module Client (B : sig class bank : client_view end) =
      struct
        class account x : client_view =
          object
            inherit B.bank
            inherit check_client x
          end
        
        let discount x =
          let c = new account x in
          if today () < (1998, 10, 30) then c # deposit (m 100.); c
      end
  end

module Euro_account = Account(Euro)

module Client = Euro_account.Client (Euro_account)

module Investment_account (M : MONEY) =
  struct
    type m = M.c
    module A = Account(M)
    class bank =
      object
        inherit A.bank
        method mail s = print_string s
      end
    
    class type client_view =
      object
        method deposit : m -> unit
        method history : m operation list
        method withdraw : m -> m
        method balance : m
        method mail : string -> unit
      end
    
    module Client (B : sig class bank : client_view end) =
      struct
        class account x : client_view =
          object
            inherit B.bank
            inherit A.check_client x
          end
      end
  end

(* 8.2 Simple modules as classes *)
(* 8.2.1 Strings *)
class ostring s =
  object
    method get n = String.get s n
    method print = print_string s
    method escaped = new ostring (String.escaped s)
  end

class sub_string s =
  object
    inherit ostring s
    method sub start len = new sub_string (String.sub s start len)
  end

class better_string s =
  object
    val repr = s
    method get n = String.get repr n
    method print = print_string repr
    method escaped = {< repr = String.escaped repr >}
    method sub start len = {< repr = String.sub s start len >}
  end

class ostring s =
  object (self : 'mytype)
    val repr = s
    method repr = repr
    method get n = String.get repr n
    method print = print_string repr
    method escaped = {< repr = String.escaped repr >}
    method sub start len = {< repr = String.sub s start len >}
    method concat (t : 'mytype) = {< repr = repr ^ t#repr >}
  end

class cstring n = ostring (String.make n ' ')

(* 8.2.2 Stacks *)
exception Empty

class ['a] stack =
  object
    val mutable l = ([] : 'a list)
    method push x = l <- x::l
    method pop = match l with [] -> raise Empty | a::l' -> l <- l'; a
    method clear = l <- []
    method length = List.length l
  end

class ['a, 'b] stack2 =
  object
    inherit ['a] stack
    method fold f  (x : 'b) = List.fold_left f x l
  end

let s = new stack2

let _ = s#fold ( + ) 0

let _ = s

class ['a] stack3 =
  object
    inherit ['a] stack
    method fold : 'b. ('b -> 'a -> 'b) -> 'b -> 'b
                = fun f x -> List.fold_left f x l
  end

(* 8.2.3 Hashtbl *)
class type ['a, 'b] hash_table =
  object
    method find : 'a -> 'b
    method add : 'a -> 'b -> unit
  end

class ['a, 'b] small_hashtbl : ['a, 'b] hash_table =
  object
    val mutable table = []
    method find key = List.assoc key table
    method add key value = table <- (key, value) :: table
  end

class ['a, 'b] hashtbl size : ['a, 'b] hash_table =
  object(self)
    val table = Array.init size (fun i -> new small_hashtbl)
    method private hash key =
      (Hashtbl.hash key) mod (Array.length table)
    method find key = table.(self#hash key) # find key
    method add key = table.(self#hash key) # add key
  end

(* 8.2.4 Sets *)
module type SET =
  sig
    type 'a tag
    class ['a] c:
      object ('b)
        method is_empty : bool
        method mem : 'a -> bool
        method add : 'a -> 'b
        method union : 'b -> 'b
        method iter : ('a -> unit) -> unit
        method tag : 'a tag
    end
  end

module Set : SET =
  struct
    let rec merge l1 l2 =
      match l1 with
        [] -> l2
      | h1 :: t1 ->
          match l2 with
            [] -> l1
          | h2 :: t2 ->
              if h1 < h2 then h1 :: merge t1 l2
              else if h1 > h2 then h2 :: merge l1 t2
              else merge t1 l2
    type 'a tag = 'a list
    class ['a] c =
      object (_ : 'b)
        val repr = ([] : 'a list)
        method is_empty = (repr = [])
        method mem x = List.exists (( = ) x) repr
        method add x = {< repr = merge [x] repr >}
        method union (s : 'b) = {< repr = merge repr s#tag >}
        method iter (f : 'a -> unit) = List.iter f repr
        method tag = repr
      end
  end