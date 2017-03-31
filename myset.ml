(*
                         CS 51 Problem Set 5
                   A Web Crawler and Search Engine
                             Spring 2017

An interface and simple implementation of a set abstract datatype.
 *)

open Order ;;

(* Interface for set modules *)

module type SET =
  sig
    (* type of elements in the set *)
    type elt

    (* abstract type for the set *)
    type set

    val empty : set

    val is_empty : set -> bool

    val insert : set -> elt -> set

    val singleton : elt -> set

    val union : set -> set -> set

    val intersect : set -> set -> set

    (* remove an element from the set -- if the element isn't present,
      returns set unchanged *)
    val remove : set -> elt -> set

    (* returns true iff the element is in the set *)
    val member : set -> elt -> bool

    (* chooses some member from the set, removes it and returns that
       element plus the new set.  If the set is empty, returns
       None. *)
    val choose : set -> (elt * set) option

    (* fold a function across the elements of the set in some
       unspecified order, using the calling convention of fold_left,
       that is, if the set s contains s1, ..., sn, then
          fold f u s
       returns
          (f ... (f (f u s1) s2) ... sn)
     *)
    val fold : ('a -> elt -> 'a) -> 'a -> set -> 'a

    (* functions to convert values of these types to a string
       representation; useful for debugging. *)
    val string_of_set : set -> string
    val string_of_elt : elt -> string

    (* runs the tests. See TESTING EXPLANATION *)
    val run_tests : unit -> unit
  end

(* COMPARABLE signature -- A module that provides for elements that
   can be compared as to ordering and converted to a string
   representation. Includes functinos for generating values for
   testing purposes.
 *)

module type COMPARABLE =
  sig
    type t
    val compare : t -> t -> ordering
    val string_of_t : t -> string

    (* The functions below are used for testing. See TESTING EXPLANATION *)

    (* Generate a value of type t. The same t is always returned *)
    val gen : unit -> t

    (* Generate a random value of type t. *)
    val gen_random : unit -> t

    (* Generate a t greater than the argument. *)
    val gen_gt : t -> t

    (* Generate a t less than the argument. *)
    val gen_lt : t -> t

    (* Generate a t between the two arguments. Return None if no such
       t exists. *)
    val gen_between : t -> t -> t option
  end

(* An example implementation of the COMPARABLE signature. Use this
   struct for testing. *)

module IntComparable : COMPARABLE =
  struct
    type t = int
    let compare x y =
      if x < y then Less
      else if x > y then Greater
      else Equal
    let string_of_t = string_of_int
    let gen () = 0
    let gen_random =
      let _ = Random.self_init () in
      (fun () -> Random.int 10000)
    let gen_gt x = x + 1
    let gen_lt x = x - 1
    let gen_between x y =
      let (lower, higher) = (min x y, max x y) in
      if higher - lower < 2 then None else Some (higher - 1)
  end

(*----------------------------------------------------------------------
  Implementation 1: List-based implementation of sets, represented as
  sorted lists with no duplicates.
 *)

module ListSet (C: COMPARABLE) : (SET with type elt = C.t) =
  struct
    type elt = C.t
    type set = elt list

    (* INVARIANT: sorted, no duplicates *)
    let empty = []

    let is_empty xs =
      match xs with
      | [] -> true
      | _ -> false

    let singleton x = [x]

    let rec insert xs x =
      match xs with
      | [] -> [x]
      | y :: ys ->
          match C.compare x y with
          | Greater -> y :: (insert ys x)
          | Equal -> xs
          | Less -> x :: xs

    let union xs ys = List.fold_left insert xs ys

    let rec remove xs y =
      match xs with
      | [] -> []
      | x :: xs1 -> 
          match C.compare y x with
          | Equal -> xs1
          | Less -> xs
          | Greater -> x :: (remove xs1 y)

    let rec intersect xs ys =
      match xs, ys with
      | [], _ -> []
      | _, [] -> []
      | xh :: xt, yh :: yt -> 
          match C.compare xh yh with
          | Equal -> xh :: (intersect xt yt)
          | Less -> intersect xt ys
          | Greater -> intersect xs yt

    let rec member xs x =
      match xs with
      | [] -> false
      | y :: ys ->
          match C.compare x y with
          | Equal -> true
          | Greater -> member ys x
          | Less -> false

    let choose xs =
      match xs with
      | [] -> None
      | x :: rest -> Some (x, rest)

    let fold = List.fold_left

    let string_of_elt = C.string_of_t

    let string_of_set (s: set) : string =
      let f = (fun y e -> y ^ "; " ^ C.string_of_t e) in
      "set([" ^ (List.fold_left f "" s) ^ "])"


    (* Tests for the ListSet functor -- These are just examples of
    tests, your tests should be a lot more thorough than these. *)

    (* adds a list of (key,value) pairs in left-to-right order *)
    let insert_list (d: set) (lst: elt list) : set =
      List.fold_left (fun r k -> insert r k) d lst

    let rec generate_random_list (size: int) : elt list =
      if size <= 0 then []
      else (C.gen_random ()) :: (generate_random_list (size - 1))

    let test_insert () =
      let elts = generate_random_list 100 in
      let s1 = insert_list empty elts in
      List.iter (fun k -> assert(member s1 k)) elts;
      ()

    let test_remove () =
      let elts = generate_random_list 100 in
      let s1 = insert_list empty elts in
      let s2 = List.fold_right (fun k r -> remove r k) elts s1 in
      List.iter (fun k -> assert(not (member s2 k))) elts;
      ()

    let test_union () =
      ()

    let test_intersect () =
      ()

    let test_member () =
      ()

    let test_choose () =
      ()

    let test_fold () =
      ()

    let test_is_empty () =
      ()

    let test_singleton () =
      ()

    let run_tests () =
      test_insert () ;
      test_remove () ;
      test_union () ;
      test_intersect () ;
      test_member () ;
      test_choose () ;
      test_fold () ;
      test_is_empty () ;
      test_singleton () ;
      ()

  end

(*----------------------------------------------------------------------
  Implementation 2: Sets as dictionaries
 *)
(*
  TODO: Use the skeleton code for the DictSet module below and
  complete the implementation, making sure that it conforms to the
  appropriate signature.

  Add appropriate tests for the functor and make sure that your
  implementation passes the tests. Once you have the DictSet functor
  working, you can use it instead of the ListSet implementation by
  updating the definition of the Make functor below.
*)
module DictSet(C : COMPARABLE) : (SET with type elt = C.t) =
  struct
      module D = Dict.Make 
        (struct
          type key = C.t
          type value = unit
          let compare key1 key2 = C.compare key1 key2
          let string_of_key k = C.string_of_t k
          let string_of_value _ = "()" 
          let gen_key () = C.gen ()
          let gen_key_random () = C.gen_random ()
          let gen_key_gt k = C.gen_gt k
          let gen_key_lt k = C.gen_lt k
          let gen_key_between key1 key2 = C.gen_between key1 key2
          let gen_value () = ()
          let gen_pair () = (C.gen (), ())
        end)

    type elt = D.key
    type set = D.dict
    let empty = D.empty

    let is_empty (s : set) : bool = s = empty

    let insert (s : set) (e : elt) = D.insert s e ()

    let singleton (e : elt) : set = insert empty e

    let choose (s : set) : (elt * set) option =
      match D.choose s with
      | None -> None
      | Some (e, (), rest) -> Some (e, rest)

    let rec union (set1 : set) (set2 : set) : set =
      match choose set1 with
      | None -> set2
      | Some (e, rest) -> union rest (insert set2 e)

    let member (s : set) (e : elt) : bool = D.member s e

    let rec intersect (set1 : set) (set2 : set) : set =
      match choose set1 with
      | None -> empty
      | Some (e, rest) -> if member set2 e then
                            insert (intersect rest set2) e
                          else intersect rest set2

    let remove (s : set) (e : elt) : set = D.remove s e

    let rec fold f u d = fold (fun a b -> f a b) u d

    (* implement the rest of the functions in the signature! *)

    let string_of_elt = D.string_of_key
    let string_of_set s = D.string_of_dict s

    (* Tests for the DictSet functor -- Use the tests from the ListSet
       functor to see how you should write tests. However, you must
       write a lot more comprehensive tests to test ALL your
       functions. *)

    (* Add your test functions to run_tests *)
    
    let rec create_list (length : int) : elt list =
      if length <= 0 then []
      else (C.gen_random ()) :: (create_list (length - 1))

    let ordered_list (length : int) : elt list = 
      let rec ordered s length = 
        if length <= 0 then []
        else s :: (ordered (C.gen_gt s) (length - 1)) in
      ordered (C.gen()) length

    let add_to_lists (s : set) (l : elt list) : set = 
      List.fold_left (fun a b -> insert a b) s l 

    (* tested in test_insert_and remove *)
    let test_insert () =
      let e1 = create_list 10 in 
      let e2 = create_list 20 in
      let s1 = add_to_lists empty e1 in
      let s2 = add_to_lists s1 e2 in
      List.iter (fun e -> assert (member s1 e)) e1 ;
      List.iter (fun e -> assert (member s2 e)) e1 ;
      List.iter (fun e -> assert (member s2 e)) e2 ;
      ()


    (* tested in test_insert_and remove *)
    let test_remove () =
      let e1 = create_list 20 in
      let s1 = add_to_lists empty e1 in
      (*let s1_empty = List.fold_right (fun a b -> remove a b) s1 e1 in*)
      List.iter (fun e -> assert( not (member empty e))) e1 ;
      () 

    let test_union () =
      assert((union empty empty) = empty);
      let e1 = create_list 20 in 
      let e2 = create_list 20 in
      let s1 = add_to_lists empty e1 in
      let s2 = add_to_lists empty e2 in      
      let together = union s1 s2 in
      assert((union empty s1) = s1);
      List.iter (fun e -> assert(member together e)) e1;
      List.iter (fun e -> assert(member together e)) e2;
      ()

    let test_intersect () =
      assert((intersect empty empty) = empty);
      let e1 = create_list 20 in 
      let e2 = create_list 20 in
      let s1 = add_to_lists empty e1 in
      let s2 = add_to_lists empty e2 in      
      let overlap = intersect s1 s2 in
      assert((intersect empty s1) = empty);
      assert (fold (fun a b ->
                    member s1 a && member s2 a && b) true overlap);
      ()

    let test_member () =
      let e1 = create_list 20 in
      let s1 = add_to_lists empty e1 in
      List.iter (fun e -> assert (not (member empty e))) e1;
      (*let s2 = List.fold_right (fun a b -> remove a b) e1 s1 in*)
      List.iter (fun e -> assert (not (member empty e))) e1;
      ()

    let test_choose () =
      assert ((choose empty) = None);
      let e1 = C.gen() in
      let s1 = insert empty e1 in
      assert (choose s1 = Some (e1, empty));
      ()

    let test_fold () =
      let e1 = create_list 20 in
      let e2 = create_list 20 in
      let s1 = add_to_lists empty e1 in
      let s2 = add_to_lists empty e2 in
      let overlap = intersect s1 s2 in
      assert (fold (fun a b -> member s1 a && b) true empty);
      assert (not (fold (fun a b -> member s1 a && b) false empty));
      assert (fold (fun a b -> member s1 a || member s2 a && b) true overlap);
      ()

    let test_is_empty () =
      assert (is_empty empty);
      let e1 = create_list 20 in 
      let s1 = add_to_lists empty e1 in
      assert (not (is_empty s1));
      ()

    let test_singleton () =
      let e1 = C.gen() in 
      let s1 = insert empty e1 in
      let s2 = singleton e1 in
      assert (s1 = s2);
      ()

    let run_tests () =
      test_insert () ;
      test_remove () ;
      test_union () ;
      test_intersect () ;
      test_member () ;
      test_choose () ;
      test_fold () ;
      test_is_empty () ;
      test_singleton () ;
  end


(*----------------------------------------------------------------------
  Running the tests.
 *)

(* Create a module for sets of ints using the ListSet functor and test
   it. *)
module IntListSet = ListSet(IntComparable) ;;


(* Create a set of ints using the DictSet functor and test it.

   Uncomment out the lines below when you are ready to test your set
   implementation based on dictionaries. *)


module IntDictSet = DictSet(IntComparable) ;;

let _ = IntDictSet.run_tests ();;
 

(*----------------------------------------------------------------------
  Make -- a functor that creates a set module by calling the ListSet
  or DictSet functors.

  This allows switching between th two implementations for all sets
  just by changing one place in the code.  *)

module Make (C : COMPARABLE) : (SET with type elt = C.t) =
  (* Change this line to use the dictionary implementation of sets
     when you are finished. *)
  (*ListSet (C)*)
  DictSet (C)
