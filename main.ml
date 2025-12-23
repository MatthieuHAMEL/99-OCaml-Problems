(*** 1. Tail of a list ***)
(* last : 'a list -> 'a option *)
let rec last iList = 
  match iList with 
    | [] -> None
    | [e] -> Some e    (* cleaner than:  | head::rest when rest = [] -> Some head *)
    | _::rest -> last rest;;

last ["a" ; "b" ; "c" ; "d"];;
(*  - : string option = Some "d" *)

last [];;
(* - : 'a option = None *)
(**************************************)

(*** 2. Last two elements of a list ***)
let rec last_two iList = 
  match iList with
    | [] -> None
    | [a] -> None    (* solution proposes: | [] | [_] -> None  ... noted! *)
    | [a; b] -> Some (a, b)
    | _::rest -> last_two rest;;

last_two ["a"; "b"; "c"; "d"];;
(* - : (string * string) option = Some ("c", "d") *)
last_two ["a"];;
(* - : (string * string) option = None *)
(*********************************)

(*** 3. N'th Element of a List ***)
let rec at n iList = 
  match iList with
    | [] -> None (* raise (Failure "out of bounds") *)
    | head::rest -> if n = 0 then Some head else at (n-1) rest;;

at 2 ["a"; "b"; "c"; "d"; "e"];;
(* - : string option = Some "c" *)
at 2 ["a"];;
(* - : string option = None *)
(***************************)

(*** 4. Length of a list ***)
(* simplest version: *)
let rec length iList = 
  match iList with
    | [] -> 0
    | _::rest -> 1 + length rest;;

length ["a"; "b"; "c"];;
(* - : int = 3 *)
length [];;
(* - : int = 0 *)

(* tail recursive version *)
let length_tailrec iList = 
  let rec aux acc iList = 
    match iList with
      | [] -> acc
      | _::rest -> aux (acc+1) rest (* tail recursive == no computation after the recursive call *)
  in  
  aux 0 iList;;

length_tailrec ["a"; "b"; "c"];;
(* - : int = 3 *)
length_tailrec [];;
(* - : int = 0 *)
(*************************)

(*** 5. Reverse a list ***)

(* intuitive version (not tail-recursive) *)
let rec rev iList = 
  match iList with
    | [] -> []
    | head::rest -> rev rest @ [head];;

rev ["a"; "b"; "c"];;
(* - : string list = ["c"; "b"; "a"] *)

let rev_tailrec iList = 
  let rec aux acc list = 
    match list with 
      | [] -> acc
      | head::rest -> aux ([head] @ acc) rest (* or : aux (head :: acc) rest *)
  in
  aux [] iList;;

rev_tailrec ["a"; "b"; "c"];;

(*************************)

(*** 6. Palindrome ***)
let is_palindrome iList = iList = (rev_tailrec iList);;

is_palindrome ["x"; "a"; "m"; "a"; "x"];;
(* - : bool = true *)
not (is_palindrome ["a"; "b"]);;
(* - : bool = true *)

(*************************)

(*** 7. Flatten a list ***)
type 'a node =
  | One of 'a 
  | Many of 'a node list;;

let rec flatten iNodeList = 
  let rec flatten_node iNode = 
    match iNode with
      | One(elt) -> [elt]
      | Many([]) -> []
      | Many(hnode::r) -> (flatten_node hnode) @ flatten r
  in  
  match iNodeList with 
    | [] -> []
    | head::rest -> (flatten_node head) @ (flatten rest)
;;

flatten [One "a"; Many [One "b"; Many [One "c" ;One "d"]; One "e"]];;
(* - : string list = ["a"; "b"; "c"; "d"; "e"] *)

let flatten_tailrec nodelist = 
  let rec aux acc nodelist = 
    match nodelist with 
      | [] -> acc
      | h::rest -> (match h with 
        | One(str) -> aux (acc @ [str]) rest (* NB: @ adds some complexity and :: is better but necessitates a List.rev at the end *)
        | Many(n_sublist) -> aux (aux acc n_sublist) rest) 
  in
  aux [] nodelist;;

flatten_tailrec [One "a"; Many [One "b"; Many [One "c" ;One "d"]; One "e"]];;

(*** 8. Eliminate (consecutive) duplicates ***)

let rec compress iList = 
  match iList with
    | [] -> []
    | [a] -> [a]    
    | h1::h2::rest -> if h1 = h2 then compress (h2::rest) else h1 :: (compress (h2::rest))
;;
compress ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"];;
(* - : string list = ["a"; "b"; "c"; "a"; "d"; "e"] *)

(* tail recursive version *)
let compress_tailrec iList = 
  let rec aux acc list =
    match list with
      | [] -> List.rev acc
      | [a] -> List.rev (a::acc)   (* or: aux (a::acc) [] to have only 1 base case *)
      | h1::h2::rest -> if h1 = h2 then aux acc (h2::rest) else aux (h1::acc) (h2::rest)
  in
  aux [] iList;;

compress_tailrec ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"];;
(* - : string list = ["a"; "b"; "c"; "a"; "d"; "e"] *)

