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


(*** 2. Last two elements of a list ***)
