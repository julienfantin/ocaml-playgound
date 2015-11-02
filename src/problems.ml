(* 99 problems in OCaml
   https://ocaml.org/learn/tutorials/99problems.html *)

(* Lists *)

(* 1. Write a function last : 'a list -> 'a option that returns the last element of a list. (easy) *)

let rec last = function
  | [] -> None
  | [x] -> Some x
q  | _ :: t -> last t

let _ = last [ "a" ; "b" ; "c" ; "d" ]
let _ = last []

(* 2. Find the last but one (last and penultimate) elements of a list. (easy) *)

let rec last_two = function
  | [] -> None
  | [x ; y] -> Some (x, y)
  | _ :: t -> last_two t;;

let _ = last_two [ "a" ; "b" ; "c" ; "d" ]
let _ = last_two [ "a" ]

(* 3. Find the k'th element of a list *)

let rec at n = function
  | [] -> None
  | h :: t -> if n = 1 then Some h else at (n - 1) t

let _ = at 3 [ "a" ; "b"; "c"; "d"; "e" ]
let _ = at 3 [ "a" ]

(* 4. Find the number of elements of a list. (easy) *)

let length list =
  let rec aux    n = function
    | [] -> n
    | _ :: t -> aux    ( n + 1) t in
   aux    0 list

   let _ = length [ "a" ; "b" ; "c"]
let _ = length []

(* Reverse a list *)

let rec rev list =
  let rec aux acc = function
    | [] -> acc
    | x :: t -> aux (x :: acc) t in
  aux [] list;;

let _ = rev [ 1; 2; 3 ]

(* Find out whether a list is a palindrome *)

let is_palindrome list =
  list = rev list

let _ = is_palindrome [1 ; 2]
let _ = is_palindrome [1 ; 2; 1]

(* Flatten a nested list structure. (medium)

   There is no nested list type in OCaml, so we need to define one first. A node
   of a nested list is either an element, or a list of nodes. *)

type 'a node =
  | One of 'a
  | Many of 'a node list;;

let flatten list =
  let rec aux acc = function
    | [] -> acc
    | One h :: t -> aux (h :: acc) t
    | Many h :: t -> aux (aux acc h) t
  in
  rev(aux [] list)

let _ = flatten [ One "a" ; Many [ One "b" ; Many [ One "c" ; One "d" ] ; One "e" ] ];;

(* Eliminate consecutive duplicates of list elements. (medium) *)

let rec compress = function
  | a -> a
  | a :: (b :: _ as t) -> if a = b then compress t else a :: compress t

let _ = compress ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"]

(* Pack consecutive duplicates of list elements into sublists. (medium) *)

let pack list =
  let rec aux curr acc = function
    | [] -> []
    | [x] -> (x :: curr) :: acc
    | a :: (b :: _ as t) ->
      if a = b then aux (a :: curr) acc t
      else aux [] ((a :: curr) :: acc) t in
  List.rev(aux [] [] list)

let _ = pack ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"d";"e";"e";"e";"e"]

(* Run-length encoding of a list http://en.wikipedia.org/wiki/Run-length_encoding *)

let encode list =
  let rec aux n acc = function
    | [] -> []
    | [x] -> (n + 1, x) :: acc
    | a :: (b :: _ as t) ->
      if a = b then aux (n + 1) acc t
      else aux 1 ((n , a) :: acc) t in
  List.rev(aux 1 [] list)

let _ = encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"]

(* Modified run-length encoding. (easy)

   Modify the result of the previous problem in such a way that if an element
   has no duplicates it is simply copied into the result list. Only elements
   with duplicates are transferred as (N E) lists. Since OCaml lists are
   homogeneous, one needs to define a type to hold both single elements and
   sub-lists. *)

type 'a rle =
  | One of 'a
  | Many of int * 'a;;

let encode2 list =
  let as_tuple n x =
    if n = 1 then One x
    else Many (n, x) in
  let rec aux n acc = function
    | [] -> []
    | [x] -> (as_tuple n x) :: acc
    | a :: (b :: _ as t) ->
      if a = b then aux (n + 1) acc t
      else aux 1 ((as_tuple n a) :: acc) t
  in List.rev(aux 1 [] list)

let _ = encode2 ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"]

(* Decode a run-length encoded list. (medium) Given a run-length code list
   generated as specified in the previous problem, construct its uncompressed
   version *)

let decode list =
  let rec many acc n x =
    if n = 0 then acc
    else many (x :: acc) (n - 1) x in
  let rec aux acc = function
    | [] -> acc
    | (One x) :: t -> aux (x :: acc) t
    | (Many (n, x)) :: t -> aux (many acc n x) t in
  List.rev(aux [] list)

let _ = decode [Many (4,"a"); One "b"; Many (2,"c"); Many (2,"a"); One "d"; Many (4,"e")]

(* Run-length encoding of a list (direct solution). (medium)

   Implement the so-called run-length encoding data compression method
   directly. I.e. don't explicitly create the sublists containing the
   duplicates, as in problem "Pack consecutive duplicates of list elements into
   sublists", but only count them. As in problem "Modified run-length encoding",
   simplify the result list by replacing the singleton lists (1 X) by X. *)

let encode3 list =
  let rle n x = if n = 1 then One x else Many (n, x) in
  let rec aux n acc = function
    | [] -> acc
    | [a] -> (rle n a) :: acc
    | a :: (b :: _ as t) ->
      if a = b then aux (n + 1) acc t
      else aux 1 ((rle n a) :: acc) t in
  List.rev(aux 1 [] list)

let _ = encode3 ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"]

(* Duplicate the elements of a list. (easy) *)

let duplicate list =
  let rec aux acc = function
    | [] -> acc
    | h :: t -> aux (h :: h :: acc) t in
  List.rev (aux [] list)

let rec duplicate = function
  | [] -> []
  | h :: t -> h :: h :: duplicate t

let _ = duplicate ["a";"b";"c";"c";"d"]

(* Replicate the elements of a list a given number of times *)

let replicate list n =
  let rec n_prepend n x acc =
    if n = 0 then acc else n_prepend (n - 1) x (x :: acc) in
  let rec aux acc = function
    | [] -> acc
    | h :: t -> aux (n_prepend n h acc) t in
  List.rev (aux [] list)

let _ = replicate ["a";"b";"c"] 3;;

(* Drop every N'th element from a list. (medium) *)

let drop list n =
  let rec aux count = function
    | [] -> []
    | h :: t -> if count = n then aux 1 t else h :: aux (count + 1) t in
  aux 1 list

let _ = drop ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 3

(* Split a list into two parts; the length of the first part is given. (easy)

   If the length of the first part is longer than the entire list, then the
   first part is the list and the second part is empty *)

let split list n =
  let rec aux i head = function
    | [] -> List.rev head, []
    | x :: t as tail ->
      if i = n then (List.rev head, tail)
      else aux (i + 1) (x :: head) t in
  aux 0 [] list

let _ = split ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 3
let _ = split ["a";"b";"c";"d"] 5

(* Extract a slice from a list. (medium)

   Given two indices, i and k, the slice is the list containing the elements
   between the i'th and k'th element of the original list (both limits
   included). Start counting the elements with 0 (this is the way the List
   module numbers elements). *)

let slice list i k =
  let rec drop n = function
    | [] -> []
    | h :: t as l -> if n = 0 then l else drop (n - 1) t
  in
  let rec take n = function
    | [] -> []
    | h :: t -> if n = 0 then [] else h :: take (n - 1) t
  in
  (* take 0 k [] (drop 0 i list) *)
  take (k - i + 1) (drop i list)

let _ = slice ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 2 6
let _ = slice ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 1 2

(* TODO Rotate a list N places to the left *)

(* Remove the K'th element from a list. (easy) *)

let rec remove_at n = function
    | [] -> []
    | h :: t -> if n = 0 then t else h :: remove_at (n - 1) t

let _ = remove_at 1 ["a";"b";"c";"d"]

(* Insert an element at a given position into a list. (easy)

   Start counting list elements with 0. If the position is larger or equal to
   the length of the list, insert the element at the end. (The behavior is
   unspecified if the position is negative.) *)

let rec insert_at e n = function
  | [] -> [e]
  | h :: t as l -> if n = 0 then e :: l else h :: insert_at e (n - 1) t

let _ = insert_at "alfa" 1 ["a";"b";"c";"d"];;
let _ = insert_at "alfa" 3 ["a";"b";"c";"d"];;
let _ = insert_at "alfa" 4 ["a";"b";"c";"d"];;

(* Create a list containing all integers within a given range. (easy)

   If first argument is smaller than second, produce a list in decreasing
   order. *)

let range a b =
  let s = if a < b then (+) else (-) in
  let rec aux = function
    | [] -> []
    | h :: _ as l -> if h = b then l else aux ((s h 1) :: l) in
  List.rev (aux [a])

let _ = range 4 9
let _ = range 9 4

(* Extract a given number of randomly selected elements from a list. (medium)

   The selected items shall be returned in a list. We use the Random module but
   do not initialize it with Random.self_init for reproducibility. *)

let rand_select list n =
  let rand_n l = Random.int (List.length l)  in
  let rec extract n acc = function
    | [] -> raise Not_found
    | h :: t -> if n = 0 then h, (acc @ t) else extract (n - 1) (h :: acc) t in
  let extract_rand l = extract (rand_n l) [] l in
  let rec aux n acc = function
    | [] -> acc
    | l ->
      if n = 0 then acc
      else
        let picked, rest = extract_rand l in
        aux (n - 1) (picked :: acc) rest
  in aux n [] list

let _ = rand_select ["a";"b";"c";"d";"e";"f";"g";"h"] 4

(* Lotto: Draw N different random numbers from the set 1..M. (easy)

     The selected numbers shall be returned in a list *)

let lotto_select (n : int) (ub : int) : int list =
    rand_select(range 1 ub) n

let _ = lotto_select 6 49

(* Generate a random permutation of the elements of a list. (easy) *)

let permutation list =
  let rand_n l = (Random.int (List.length l)) in
  let rec extract n acc = function
    | [] -> raise Not_found
    | h :: t -> if n = 0 then h, (acc @ t) else extract (n - 1) (h :: acc) t in
  let extract_rand list = extract (rand_n list) [] list in
  let rec aux acc = function
    | [] -> acc
    | l ->
      let extracted, rest = extract_rand l in
      aux (extracted :: acc) rest
  in aux [] list


let _ =  permutation ["a"; "b"; "c"; "d"; "e"; "f"];;

(* Generate the combinations of K distinct objects chosen from the N elements of
   a list. (medium)

   In how many ways can a committee of 3 be chosen from a group of 12 people? We
   all know that there are C(12,3) = 220 possibilities (C(N,K) denotes the
   well-known binomial coefficients). For pure mathematicians, this result may
   be great. But we want to really generate all the possibilities in a list. *)

(* Group the elements of a set into disjoint subsets. (medium)

   In how many ways can a group of 9 people work in 3 disjoint subgroups of 2, 3
   and 4 persons? Write a function that generates all the possibilities and
   returns them in a list.  Generalize the above function in a way that we can
   specify a list of group sizes and the function will return a list of
   groups. *)

(* Sorting a list of lists according to length of sublists. (medium)

   We suppose that a list contains elements that are lists themselves. The
   objective is to sort the elements of this list according to their
   length. E.g. short lists first, longer lists later, or vice versa.

   Again, we suppose that a list contains elements that are lists
   themselves. But this time the objective is to sort the elements of this list
   according to their length frequency; i.e., in the default, where sorting is
   done ascendingly, lists with rare lengths are placed first, others with a
   more frequent length come later *)

(* Arithmetic *)

(* ... *)

(* Logic and Codes *)

(* Let us define a small "language" for boolean expressions containing
   variables: *)

type bool_expr =
  | Var of string
  | Not of bool_expr
  | And of bool_expr * bool_expr
  | Or of bool_expr * bool_expr

(* A logical expression in two variables can then be written in prefix
   notation. For example, (a ∨ b) ∧ (a ∧ b) is written: *)
let _= And(Or(Var "a", Var "b"), And(Var "a", Var "b"))

(* Truth tables for logical expressions (2 variables). (medium)

   Define a function, table2 which returns the truth table of a given logical
   expression in two variables (specified as arguments). The return value must
   be a list of triples containing (value_of_a, balue_of_b, value_of_expr). *)

let table2 a b expr =
  let rec aux a_val b_val = function
    | Var x ->
      if x = a then a_val
      else if x = b then b_val
      else failwith "Invalid variable"
    | Not e -> not (aux a_val b_val e)
    | And(e1, e2) -> ((aux a_val b_val e1) && (aux a_val b_val e2))
    | Or(e1, e2) -> ((aux a_val b_val e1) || (aux a_val b_val e2)) in
  let table a_val b_val =
     [a_val ; b_val ; (aux a_val b_val expr)]
  in
  [(table true true) ;
   (table true false) ;
   (table false true) ;
   (table false false)]

let _ = table2 "a" "b" (And(Var "a", Or(Var "a", Var "b")))

(* Truth tables for logical expressions. (medium)

   Generalize the previous problem in such a way that the logical expression may
   contain any number of logical variables. Define table in a way that table
   variables expr returns the truth table for the expression expr, which
   contains the logical variables enumerated in variables. *)

let table vars expr =
  let rec eval var_vals = function
    | Var x -> List.assoc x var_vals
    | Not e -> not (eval var_vals e)
    | And(e1, e2) -> ((eval var_vals e1) && (eval var_vals e2))
    | Or(e1, e2) -> ((eval var_vals e1) || (eval var_vals e2)) in
  let rec make_table expr var_vals = function
    | [] -> [(List.rev var_vals, eval var_vals expr)]
    | h :: t ->
      make_table expr ((h, true)  :: var_vals) t
      @ make_table expr ((h, false) :: var_vals) t in
  make_table expr [] vars

let _ = table ["a"; "b"] (And(Var "a", Or(Var "a", Var "b")));;
let _ =
  let a = Var "a" and b = Var "b" and c = Var "c" in
  table ["a"; "b"; "c"] (Or(And(a, Or(b,c)), Or(And(a,b), And(a,c))));;

(* Gray code. (medium)

   An n-bit Gray code is a sequence of n-bit strings constructed according to
   certain rules. For example,

   n = 1: C(1) = ['0','1'].
   n = 2: C(2) = ['00','01','11','10'].
   n = 3: C(3) = ['000','001','011','010',´110´,´111´,´101´,´100´].

   Find out the construction rules and write a function with the following
   specification: gray n returns the n-bit Gray code. *)

(* Binary Trees

   A binary tree is either empty or it is composed of a root element and two
   successors, which are binary trees themselves.

   In OCaml, one can define a new type binary_tree that carries an arbitrary
   value of type 'a at each node. *)

type 'a binary_tree =
    | Empty
    | Node of 'a * 'a binary_tree * 'a binary_tree

(* In OCaml, the strict type discipline guarantees that, if you get a value of type
   binary_tree, then it must have been created with the two constructors Empty and
   Node. *)

(* Construct completely balanced binary trees. (medium)

   In a completely balanced binary tree, the following property holds for every
   node: The number of nodes in its left subtree and the number of nodes in its
   right subtree are almost equal, which means their difference is not greater
   than one.

   Write a function cbal_tree to construct completely balanced binary trees for
   a given number of nodes. The function should generate all solutions via
   backtracking. Put the letter 'x' as information into all nodes of the
   tree. *)
