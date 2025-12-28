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
(*********************************************)

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
(*********************************************)

(*** 9. Pack consecutive duplicates ***)
(* this time I try to write directly a tail-recursive function *)

let pack iList = 
  let rec aux acc list = 
    match list with
      |[] -> List.rev acc
      |head::rest -> (* it depends on what's in the accumulator *)
        match acc with
          | [] -> aux [[head]] rest
          | headacc(*headache!*)::restacc -> if List.hd headacc = head then  (* the sublists aren't empty by construction *)
                                                aux ((head::headacc) :: restacc) rest
                                             else 
                                                aux ([head] :: acc) rest
  in 
  aux [] iList
;;

(* checking the head of the accumulator (=already packed items) is not necessary and I could use 
 * a pattern similar to problem 8. cf the solution given on ocaml.org *)

pack ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "d"; "e"; "e"; "e"; "e"];;
(* 
- : string list list =
[["a"; "a"; "a"; "a"]; ["b"]; ["c"; "c"]; ["a"; "a"]; ["d"; "d"];
 ["e"; "e"; "e"; "e"]] 
*)

(***************************)
(* 10. Run-Length Encoding *)

let encode iList = 
  let rec aux acc list = 
    match list with
      | [] -> List.rev acc
      | head::rest -> match acc with
        | [] -> aux [(1, head)] rest
        | (num, str)::restacc -> if str = head then
                                   aux ((num+1, str)::restacc) rest
                                 else
                                   aux ((1, head)::acc) rest
  in
  aux [] iList
;;

encode ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"];;
(* - : (int * string) list =
[(4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e")] *)


(************************************)
(* 11. Modified Run-Length Encoding *)

type 'a rle =
  | One of 'a
  | Many of int * 'a;;

(* I could adapt the previous solution easily but this time I pass the current count in the accumulator *)

let modified_encode iList = 
  let rec aux acc count list = 
    match list with
      | [] -> [] (* can't enter here unless iList is empty *)
      | [a] -> if count = 0 then One(a)::acc else Many(count+1, a)::acc
      (* I don't process h2 here, the goal is only to compare it to h1 *)
      | h1::h2::rest -> 
        if h1 = h2 then 
          aux acc (count+1) (h2::rest) 
        else if count = 0 then (* the element is unique so I push a "One" *)
          aux (One(h1)::acc) 0 (h2::rest)          
        else 
          aux (Many(count+1, h1)::acc) 0 (h2::rest)
  in 
  List.rev (aux [] 0 iList)
;;

modified_encode ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"];;
(* - : string rle list =
[Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d";
 Many (4, "e")] *)

(****************************************)
(* 12. Decode a Run-Length Encoded list *)

let decode iList =
  let rec aux acc cnt list =
    match list with
      | [] -> acc
      | One(a)::rest -> aux (a::acc) 0 rest
      | Many(num, e)::rest -> 
        if cnt+1 = num then
          aux (e::acc) 0 rest
        else
          aux (e::acc) (cnt+1) list (* I'm not done with e yet *)
  in
  List.rev (aux [] 0 iList)
;;

decode [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")];;
(* - : string list =
["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"] *)

(***********************************************************)
(*** 13. Run-Length Encoding of a List (Direct Solution) ***)
(** I don't see any difference with problem 11. **)

(********************************************)
(*** 14. Duplicate the Elements of a List ***)

let duplicate iList =
  let rec aux acc list =
    match list with
      | [] -> acc
      | head::rest -> aux (head::head::acc) rest
  in
  List.rev (aux [] iList)
;;

duplicate ["a"; "b"; "c"; "c"; "d"];;
(* - : string list = ["a"; "a"; "b"; "b"; "c"; "c"; "c"; "c"; "d"; "d"] *)

(********************************************************************)
(*** 15. Replicate the Elements of a List a Given Number of Times ***)

let replicate iList n =
  let rec aux acc cnt list =
    match list with
      | [] -> acc
      | head::rest -> if (cnt+1) = n then aux (head::acc) 0 rest else aux (head::acc) (cnt+1) list
  in
  List.rev (aux [] 0 iList)
;;
replicate ["a"; "b"; "c"] 3;;
(* - : string list = ["a"; "a"; "a"; "b"; "b"; "b"; "c"; "c"; "c"] *)

(***********************************************)
(*** 16. Drop Every N'th Element From a List ***)

let drop iList n =
  let rec aux acc cnt list =
    match list with
      | [] -> acc
      | h::r -> if (cnt+1) = n then aux acc 0 r else aux (h::acc) (cnt+1) r
  in
  List.rev (aux [] 0 iList)
;;

drop ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 3;;
(* - : string list = ["a"; "b"; "d"; "e"; "g"; "h"; "j"] *)


(******************************************************************************)
(*** 17. Split a List Into Two Parts; The Length of the First Part Is Given ***)

let split iList n =
  let rec aux acc cnt list =
    match list with
      | [] -> (List.rev acc, [])
      | h::r -> if cnt = n then (* done! *)
                  (List.rev acc, r)
                else aux (h::acc) (cnt+1) r
  in
  aux [] 0 iList
;;

split ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 3;;
(* - : string list * string list = 
(["a"; "b"; "c"], ["d"; "e"; "f"; "g"; "h"; "i"; "j"]) *)
split ["a"; "b"; "c"; "d"] 5;;
(* - : string list * string list = (["a"; "b"; "c"; "d"], []) *)

(***************************************)
(*** 18. Extract a Slice From a List ***)

let slice iList i k = (* both limits included *)
  let rec aux acc cnt list =
    match list with
      | [] -> acc
      | head::rest -> if cnt < i then (* not there yet *)
                        aux acc (cnt+1) rest
                      else if (cnt <= k) then (* add to the slice *)
                        aux (head::acc) (cnt+1) rest
                      else acc (* no need to continue *)
  in
  List.rev (aux [] 0 iList);;

slice ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 2 6;;
(* - : string list = ["c"; "d"; "e"; "f"; "g"] *)

(******************************************)
(*** 19. Rotate a List N Places to the Left ***)

let rotate iList n =
  let (l_1, l_2) = split iList n in l_2 @ l_1;;

rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 3;;
(* - : string list = ["d"; "e"; "f"; "g"; "h"; "a"; "b"; "c"] *)

(***********************************************)
(*** 20. Remove the K'th Element From a List ***)

let remove_at n iList =
  let rec aux acc cnt list = 
    match list with
      | [] -> []
      | head::rest -> if cnt = n then (List.rev acc) @ rest else aux (head::acc) (cnt+1) rest
  in
  aux [] 0 iList
;;

remove_at 1 ["a"; "b"; "c"; "d"];;
(* - : string list = ["a"; "c"; "d"] *)

(*************************************************************)
(*** 21. Insert an Element at a Given Position Into a List ***)

let insert_at iElt n iList =
  if n = 0 then iElt::iList 
  else
    let rec aux acc cnt list =
      match list with
        | [] -> if cnt = n then List.rev_append acc [iElt] else raise (Failure "out of bounds")
        | head::rest -> if cnt = n then List.rev_append acc (iElt::head::rest)
                        else aux (head::acc) (cnt+1) rest
    in
    aux [] 0 iList
  ;;
insert_at "alfa" 1 ["a"; "b"; "c"; "d"];;
(* - : string list = ["a"; "alfa"; "b"; "c"; "d"] *)
insert_at "Babar" 0 ["a"; "b"; "c"; "d"];;
insert_at "Babar" 4 ["a"; "b"; "c"; "d"];;
insert_at "Babar" 9 ["a"; "b"; "c"; "d"];; (* raises *)
insert_at "Babar" 0 [];;

(**********************************************************************)
(*** 22. Create a List Containing All Integers Within a Given Range ***)

let range i k = (* k included *)
  let rec aux acc cnt lowest =
    if cnt >= lowest then aux (cnt::acc) (cnt-1) lowest else acc
  in
  if i <= k then aux [] k i else List.rev (aux [] i k)
;;

range 4 9;;
range 9 4;;
(* - : int list = [4; 5; 6; 7; 8; 9] *)

(****************************************************************************)
(*** 23. Extract a Given Number of Randomly Selected Elements From a List ***)

let rand_select iList n =
  let _ = Random.init 0 in
  let rec aux_kth acc cnt_to_k k list cnt_to_n =
    match list with
      | [] -> raise (Failure "out of bounds")
      | head::rest -> if cnt_to_k = k then (* done! *)
                        aux_get_random_idx (head::acc) iList (cnt_to_n+1)
                      else
                        aux_kth acc (cnt_to_k+1) k rest cnt_to_n
   (* nb: mutual recursion not needed if I define kth (non recursive) calling aux_kth 
      and get_random_idx calling aux_get_random_idx.*)
  and aux_get_random_idx acc list cnt_to_n =
    if cnt_to_n < n then
      let k = Random.int (List.length list) in
        (aux_kth acc 0 k list cnt_to_n)
    else acc
  in
  aux_get_random_idx [] iList 0
  ;;
rand_select ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 3;;
(* - : string list = ["e"; "c"; "g"] *)

(************************************************************************)
(*** 24. Lotto: Draw N Different Random Numbers From the Set 1..M ***)

let lotto_select n max =
  rand_select (range 1 max) n;;

lotto_select 6 49;;
(* - : int list = [20; 28; 45; 16; 24; 38] *)

(*******************************************************************)
(*** 25. Generate a Random Permutation of the Elements of a List ***)

let permutation list =
  let rec aux_pick_i acc cnt_to_i i list =
    match list with
      | [] -> raise (Failure "out of bounds")
      | head::rest -> if cnt_to_i = i then
                        (head, acc @ rest)
                      else 
                        aux_pick_i (head::acc) (cnt_to_i+1) i rest

  in
  let rec aux_permutation acc list =
    match list with
      | [] -> acc (* done! *)
      | [a] -> a::acc (* no need to pick a random number between 0 and 0 *)
      | _::_ ->
    let random_index = (Random.int (List.length list)) in
    let (random_elt, rest) = aux_pick_i [] 0 random_index list in 
    aux_permutation (random_elt::acc) rest
  in
  aux_permutation [] list
;;

permutation ["a"; "b"; "c"; "d"; "e"; "f"];;
(* - : string list = ["c"; "d"; "f"; "e"; "b"; "a"] *)

(************************************************************************************************)
(*** 26. Generate the Combinations of K Distinct Objects Chosen From the N Elements of a List ***)

(*[ 3 1 0 ] -> aux_incr [ 1 0 ] 1 -> prefix is [3] , h+2 is 3, not goe len, so [3 2 0] (a c d)

[3 2 0] -> aux_incr [2 0] -> prefix is [3], h+2 is 4, which is goe len, 
  -> aux_incr 0 ->  *)

open Printf;;

let extract n list =
  let len = List.length list in
  let rec aux_incr indexes cnt = (* try to increment the head, if it's at its max, increment the 2nd elt, etc *)
    match indexes with
      | [] -> None (* can't increment more *)
      | h::r ->
        if h = (len-1) then aux_incr r (cnt+1)
        else (* return the result *)
          let prefix = if cnt = 0 then [] else range (h+2+cnt-1) (h+2) in
          if cnt >= 1 && (List.hd prefix) >= len then aux_incr r (cnt+1) else Some(prefix @  ((h+1)::r))
  in
  let incr_indexes indexes = aux_incr indexes 0 
  in
  let rec aux_extract_indexes acc indexes list = 
    match indexes with
      | [] -> acc (* done! *)
      | h::r -> aux_extract_indexes ((List.nth list h)::acc) r list
  in
  let extract_indexes indexes list = aux_extract_indexes [] indexes list
  in
  let rec aux acc_res indexes =
    let combi = extract_indexes indexes list in
    let opt_new_index = incr_indexes indexes in
      match opt_new_index with 
        | None -> List.rev (combi::acc_res)
        | Some(new_index) -> aux (combi::acc_res) new_index
  in
  aux [] (range (n-1) 0)
;;
    
extract 1 ["a"; "b"; "c"; "d"];;
extract 2 ["a"; "b"; "c"; "d"];;
extract 3 ["a"; "b"; "c"; "d"];;
extract 4 ["a"; "b"; "c"; "d"];;
(* - : string list list =
[["a"; "b"]; ["a"; "c"]; ["a"; "d"]; ["b"; "c"]; ["b"; "d"]; ["c"; "d"]] *)

(* playing with a triangular list of indexes was not a good idea. the solution 
   from ocaml.org is really better! *)

(* 27. Group the Elements of a Set Into Disjoint Subsets *)

(* The following functions are very interesting but they don't solve the problem at all *)
let mycombine list1 list2 =
  let rec aux acc l1 l2 =
    match l1 with
      | [] -> acc
      | h1::r1 -> 
        match l2 with
          | [] -> aux acc r1 list2
          | h2::r2 -> aux ([h1;h2]::acc) l1 r2
  in
  List.rev (aux [] list1 list2)
;;

mycombine ["a";"b"] ["c";"d";"e"];;

let reverse_sublists listoflists =
  let rec aux acc listoflists =
    match listoflists with
      | [] -> acc
      | headlist::restlists -> aux ((List.rev headlist)::acc) restlists
  in
  List.rev (aux [] listoflists);;

let combine_n listoflists =
  let rec aux seed acc listoflists =
    match listoflists with
      | [] -> seed (* done- the seed is the last accumulator *)
      | headlist::restlists -> 
        match headlist with
          | [] -> aux acc [] restlists (* done with the seed, move to the next list with acc as the new seed. *)
          | headelt::restelts -> (* add headelt to every element of the seed *)
            aux seed ((List.map (fun x -> headelt::x) seed)@acc) ([restelts] @ restlists)
  in
  match listoflists with [] -> raise (Failure "out of bounds")
    | head::rest -> reverse_sublists (aux (List.map (fun x -> [x]) head) [] rest)
;;  
combine_n [["a";"b";"c"]; ["d";"e";"f"]; ["i"; "j"; "k"]];;

let group_failed list sizes =
  let rec aux_extract_all acc sizes =
    match sizes with
      | [] -> acc
      | head::rest -> aux_extract_all ((extract head list)::acc) rest
  in
  let extract_all = aux_extract_all [] sizes
  in combine_n extract_all
;;
(*******end of the failed part *****)

(* let rec rep_with_elt elt list prefix = *)
(*   match list with *)
(*     | [] -> [prefix@[elt]] *)
(*     | head::rest -> (prefix @ (elt::head::rest)) :: (rep_with_elt elt rest (prefix @ [head])) *)
(* ;; *)

(* rep_with_elt 5 [2;3] [];; *)

(* let rec all_permutations list = *)
(*   match list with *)
(*     | [] -> [[]] *)
(*     | head::rest -> List.concat (List.map (fun x -> (rep_with_elt head x [])) (all_permutations rest)) *)
(* ;; *)


let rec remainder list list2 = 
  match list with
    | [] -> []
    | head::rest -> if List.mem head list2 then remainder rest list2 else head::(remainder rest list2) 
;;

let group list isizes =
  let rec aux_group list isizes rest_combis guard = (* nb: not tail-recursive *)
  match isizes with
    | [] -> [[]]
    | headsize::restsizes -> 
      let k_combis = if rest_combis = [] then 
                       if guard = 0 then 
                         extract headsize list
                       else 
                         []
                     else 
                       rest_combis in
        match k_combis with
          | [] -> []
          | headcombi::restcombis -> 
            let rem = remainder list headcombi in (* remove headcombi items from the list and get the next k-combi from that remainder *)
           (List.map (fun x -> headcombi::x) (aux_group rem restsizes [] 0)) @ (aux_group list isizes restcombis 1)
  in
  aux_group list isizes [] 0
;;


remainder ["a";"b";"c";"d"] ["c";"d"];;
group ["a"; "b"; "c"; "d"] [2; 1];;
(* - : string list list list = (* my order is not the same *)
[[["a"; "b"]; ["c"]]; [["a"; "c"]; ["b"]]; [["b"; "c"]; ["a"]];
 [["a"; "b"]; ["d"]]; [["a"; "c"]; ["d"]]; [["b"; "c"]; ["d"]];
 [["a"; "d"]; ["b"]]; [["b"; "d"]; ["a"]]; [["a"; "d"]; ["c"]];
 [["b"; "d"]; ["c"]]; [["c"; "d"]; ["a"]]; [["c"; "d"]; ["b"]]] *)


(* 28. Sorting a List of Lists According to Length of Sublists *)
let length_sort listoflists =
  let rec aux_insert_ordered acc list listoflists = (* acc will be insertion-sorted *)
    let () =  (printf " \n\nLIST\n / ") in
    let () = List.iter (printf " %s / ") list in
    match listoflists with
      | [] -> (List.rev acc) @ [list]
      | headlist::restlists -> if (List.length list) <= (List.length headlist) then
                                 (List.rev acc) @ (list::headlist::restlists)
                               else
                                 aux_insert_ordered (headlist::acc) list restlists 
  in
  let insert_ordered list listoflists = aux_insert_ordered [] list listoflists 
  in
  let rec aux acc listoflists =
    match listoflists with
      | [] -> acc
      | headlist::restlists -> aux (insert_ordered headlist acc) restlists
  in
  aux [] listoflists
;;

length_sort [["a"; "b"; "c"]; ["d"; "e"]; ["f"; "g"; "h"]; ["d"; "e"];
             ["i"; "j"; "k"; "l"]; ["m"; "n"]; ["o"]];;
(* - : string list list =
[["o"]; ["d"; "e"]; ["d"; "e"]; ["m"; "n"]; ["a"; "b"; "c"]; ["f"; "g"; "h"];
 ["i"; "j"; "k"; "l"]] *)

let frequency_sort iListoflists = 
  let listoflists_length_sorted = length_sort iListoflists in
  let lengths = List.map (fun list -> List.length list) listoflists_length_sorted in
  let enc_lengths = encode lengths in  (* ((4, 2), (1, 3), (5, 4)) -> 4 lists of len 2, 1 list of len 3 and 5 lists of len 4 *)
  let sorted_enc_lengths = List.sort (fun elt_a elt_b -> let (f1, _) = elt_a in let (f2, _) = elt_b in
                 if f1 = f2 then 0
                 else if f1 > f2 then 1
                 else -1) enc_lengths in  (* rarest frequencies first ... *)
  (* (f1, len1), (f2, len2), ... -> just extract the f1 lists of size len1, etc *)
  let () = List.iter (fun x -> let (a, b) = x in printf "%d %d " a b) sorted_enc_lengths in
  let rec aux acc sorted_freqs cnt listoflists =
    match sorted_freqs with
      | [] -> acc (* done *)
      | (headfreq, headlen)::rest -> if cnt >= headfreq then aux acc rest 0 iListoflists
                                     else match listoflists with
                                       | [] -> raise (Failure "logic error!") (* I should find the right amount of lists *)
                                       | headlist::restlists -> 
                                         if (List.length headlist) = headlen then
                                           (* I found one with the right frequency *)
                                           aux (headlist::acc) sorted_freqs (cnt+1) iListoflists
                                         else (* try again on the rest *)
                                           aux acc sorted_freqs cnt restlists
  in
  List.rev (aux [] sorted_enc_lengths 0 iListoflists)
;;

frequency_sort [["a"; "b"; "c"]; ["d"; "e"]; ["f"; "g"; "h"]; ["d"; "e"];
                ["i"; "j"; "k"; "l"]; ["m"; "n"]; ["o"]];;
(*- : string list list =
[["i"; "j"; "k"; "l"]; ["o"]; ["a"; "b"; "c"]; ["f"; "g"; "h"]; ["d"; "e"];
 ["d"; "e"]; ["m"; "n"]]*)
    
