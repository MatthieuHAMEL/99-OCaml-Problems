(****************** BINARY TREES ******************)

(* 44. Construct Completely Balanced Binary Trees *)

type 'a binary_tree =
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree;;

(* Written in List problems *)
let mycombine list1 list2 =
  let rec aux acc l1 l2 =
    match l1 with
      | [] -> acc
      | h1::r1 -> 
        match l2 with
          | [] -> aux acc r1 list2
          | h2::r2 -> aux ((h1,h2)::acc) l1 r2
  in
  List.rev (aux [] list1 list2)
;;

(* NB: the following solution is not optimal; there is no memoization. *)
let rec cbal_tree n =
  match n with
    | 0 -> [Empty]
    | 1 -> [Node ('x', Empty, Empty)]
    | _ -> let nm1 = (n-1) in
             let subtrees_half = cbal_tree (nm1/2) in
             let all_combis = 
             if nm1 mod 2 = 0 then (* put the n-1 nodes equally on the left and on the right *)
               (* Combine trees *)
               mycombine subtrees_half subtrees_half (* [(t1, u1); (t1, u2); (t1, u3); (t2, u1);...] *)
             else (* odd number of nodes to dispatch -> combine with subtrees of size (nm1/2 + 1) *)
               let subtrees_half_plus_one = cbal_tree ((nm1/2) + 1) in
               mycombine subtrees_half subtrees_half_plus_one
             in
             let trees = (List.map (fun (t1, u1) -> Node('x', t1, u1)) all_combis) (* and the other way if the two lists weren't the same *)
               @ (if nm1 mod 2 = 0 then [] else (List.map (fun (t1, u1) -> Node('x', u1, t1)) all_combis)) in
             List.sort_uniq compare trees (* eliminate duplicates *)
;;

cbal_tree 0;;
cbal_tree 1;;
cbal_tree 2;;
cbal_tree 3;;
cbal_tree 4;;


(******************************)
(* 45. Symmetric Binary Trees *)

let is_symmetric tree =
  let rec is_mirror tree1 tree2 =
    match (tree1, tree2) with
      | (Empty, Empty) -> true
      | (Node _, Empty) | (Empty, Node _) -> false
      | ((Node (_, left_1, right_1)), (Node (_, left_2, right_2))) -> 
        is_mirror left_1 right_2 && is_mirror right_1 left_2
  in
  match tree with
  | Empty -> true (* I suppose an empty tree is symmetric? *)
  | Node (_, tree1, tree2) -> is_mirror tree1 tree2
;;

let symtree = 
  Node ('A', 
    Node ('B', 
      Node('C', Empty, 
        Node('D', 
          Node('E', Empty, Empty), Node('F', Empty, Empty)))
    , Empty),
    
   Node('G', Empty, 
     Node('H', 
       Node('I', 
         Node('J', Empty, Empty), Node('K', Empty, Empty))
     , Empty)))
;;

let not_symtree = 
  Node ('A', 
    Node ('B', 
      Node('C', Empty, 
        Node('D', 
          Node('E', Empty, Empty), Node('F', Node('W', Empty, Empty), Empty)))
    , Empty),
    
   Node('G', Empty, 
     Node('H', 
       Node('I', 
         Node('J', Empty, Empty), Node('K', Empty, Empty))
     , Empty)))
;;

is_symmetric symtree;;
is_symmetric not_symtree;;

(******************************************)
(* 46. Binary Search Trees (Dictionaries) *)

let construct intlist =
  let rec insert num tree =
    match tree with
      | Empty -> Node(num, Empty, Empty)
      | Node (node_num, left_tree, right_tree) ->
        if num <= node_num then 
          Node (node_num, insert num left_tree, right_tree) 
        else 
          Node (node_num, left_tree, insert num right_tree)
  in
  let rec aux acc intlist =
    match intlist with
      | [] -> acc
      | headnum::restnums -> aux (insert headnum acc) restnums
  in
  aux Empty intlist
;; 
  

construct [3; 2; 5; 7; 1];;
(* - : int binary_tree =
Node (3, Node (2, Node (1, Empty, Empty), Empty),
 Node (5, Empty, Node (7, Empty, Empty))) *)

is_symmetric (construct [5; 3; 18; 1; 4; 12; 21]);; (* true! *)
not (is_symmetric (construct [3; 2; 5; 7; 4]));; (* true! *)

(**********************************)
(* 47. Generate-and-Test Paradigm *)

(* Generate all symmetric, completely-balanced trees of size n *)
let sym_cbal_trees n =
  let all_cbals = cbal_tree n in
  
  let rec select_sym acc trees =   (* Solution says : List.filter is_symmetric (cbal_tree n);; ... noted! ... *)
    match trees with
      | [] -> acc (* done *)
      | headtree::resttrees -> if is_symmetric headtree then 
                                 select_sym (headtree::acc) resttrees
                               else
                                 select_sym acc resttrees
  in
  select_sym [] all_cbals
;;


sym_cbal_trees 5;;
(* - : char binary_tree list =
[Node ('x', Node ('x', Node ('x', Empty, Empty), Empty),
  Node ('x', Empty, Node ('x', Empty, Empty)));
 Node ('x', Node ('x', Empty, Node ('x', Empty, Empty)),
  Node ('x', Node ('x', Empty, Empty), Empty))] *)

(**********************************************)
(* 48. Construct Height-Balanced Binary Trees *)

(* The solution was too smart for me *)

(* I tried to find a method allowing to go from step n-1 to step n: get all max depth leafs 
   and add every combination of left or right (or both) Node(Empty, Empty) to them.

   I didn't think of combining the results of hbal_tree n-1 and n-2 from the root!
   That combination "from the root" is basically 

                         new Root
                      /           \
                     /             \
    some tree in hbal n-1        some tree in hbal_n-2      (for all possible combinations)

    same with hbal_n-2 on the left, hbal n-1 on the right and then (hbal n-1, hbal n-1).

  That combination is written in a super smart way in "add_trees_with" (from the solution) 
           (I understood it but it still feels like black magic to be able to WRITE that)
  but it's just some kind of cartesian product that I could've obtained with "mycombine".
*)

let add_trees_with left right all =
  let add_right_tree all l =
    List.fold_left (fun a r -> Node ('x', l, r) :: a) all right in
  List.fold_left add_right_tree all left
;;
let rec hbal_tree n =
    if n = 0 then [Empty]
    else if n = 1 then [Node ('x', Empty, Empty)]
    else
    (* [add_trees_with left right trees] is defined in a question above. *)
      let t1 = hbal_tree (n - 1)
      and t2 = hbal_tree (n - 2) in
      add_trees_with t1 t1 (add_trees_with t1 t2 (add_trees_with t2 t1 []));;

hbal_tree 2;;
hbal_tree 3;;
