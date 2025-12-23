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
