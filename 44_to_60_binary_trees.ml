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
(* 48. Construct Height-Balanced Binary Trees *) (*FAILED*)

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

let add_trees_with left right all = (* from OCaml.org *)
  let add_right_tree all l =
    List.fold_left (fun a r -> Node ('x', l, r) :: a) all right in
  List.fold_left add_right_tree all left
;;
let rec hbal_tree n = (* from OCaml.org *)
    if n = 0 then [Empty]
    else if n = 1 then [Node ('x', Empty, Empty)]
    else
      let t1 = hbal_tree (n - 1)
      and t2 = hbal_tree (n - 2) in
      add_trees_with t1 t1 (add_trees_with t1 t2 (add_trees_with t2 t1 []));;

hbal_tree 2;;
hbal_tree 3;;

(***************************************************************************)
(* 49. Construct Height-Balanced Binary Trees With a Given Number of Nodes *)

let max_nodes h = 1 lsl h - 1;;

(* The minimum nodes of an HBAL tree of height 0 is 0 (the empty tree)
   ............................................1 is 1 (Node(Empty, Empty))
   Now consider the two HBAL trees T1, T2 with the minimum nodes, of heights n-1 and n-2 respectively.
   The HBAL tree with the minimum nodes and a height of n is Node(T1, T2). Its size is size(T1) + size(T2) + 1
   
   Proof by induction : the tree to build must have one of its subtrees of height n-1 (else it won't have height n), 
    and the other of height n-1 or n-2 to be balanced. Since size(T1) > size(T2), the other subtree 
    has height n-2. So those subtrees should be T1 and T2...
*)

let rec min_nodes h = 
  match h with
    | 0 -> 0
    | 1 -> 1
    | x -> min_nodes (x-1) + min_nodes (x-2) + 1
;;

min_nodes 3;; (* 4 *)
min_nodes 4;; (* 7 *)
min_nodes 5;; (* 12 *)

(* Minimum height of a HBAL tree of size n *)
(* The most compact binary tree is the complete tree. And the complete tree is HBAL... *)

let min_height nb_nodes =
  let rec aux h =
    if nb_nodes <= (1 lsl h) - 1 then 
      (* the nb_nodes nodes can be "contained" in a complete tree of size 2^h - 1, not true with h-1 *)
      h
    else 
      aux (h+1)
  in
  aux 0
;;

min_height 0;; (* 0 *)
min_height 1;; (* 1 *)
min_height 2;; (* 2 *)
min_height 6;; (* 3 : some tree that can be contained in the complete tree of height 3 but not the complete tree of height 2 *)
min_height 7;; (* 3 : the complete tree of height 3 *)
min_height 8;; (* 4 *)

(* Maximum height of a HBAL tree of size n - not the most efficient implementation! *)

(* intuitively if min_nodes h < nb_nodes < nb_nodes h+1 then the max height is h *)
let max_height nb_nodes =
  let rec aux h =
    if nb_nodes < min_nodes h then
      h-1
    else
      aux (h+1)
  in aux 0
;;

max_height 0;; (* 0 *)
max_height 1;; (* 1 *)
max_height 2;; (* 2 *)
max_height 3;; (* 3 *)
max_height 4;; (* 3 *)

(* Construct all the height-balanced binary trees with a given number of nodes *)
(* Bruteforce strategy (... or "generate-and-test paradigm", it's more elegant)
   I generate all HBAL trees with height between min_height and max_height, thanks to problem 48
   Then I keep the ones whose sizes is n. *)

let rec btree_size btree =
  match btree with
    | Empty -> 0
    | Node (_, left_tree, right_tree) -> 1 + (btree_size left_tree) + (btree_size right_tree)
;;

let hbal_tree_nodes n =
 let minh = min_height n in
 let maxh = max_height n in
 
 let rec collect_all acc cnt =
   if cnt > maxh then acc
   else collect_all ((hbal_tree cnt) @ acc) (cnt+1)
 in
 let all_trees = collect_all [] minh in
 List.filter (fun tree -> btree_size tree = n) all_trees;; 
;;

List.length (hbal_tree_nodes 15);; (* 1553 *)

(*****************************************)
(* 50. Count the Leaves of a Binary Tree *)

let rec count_leaves tree =
  match tree with
    | Empty -> 0
    | Node(_, Empty, Empty) -> 1
    | Node(_, left_tree, right_tree) -> (count_leaves left_tree) + (count_leaves right_tree)
;;

count_leaves Empty;; (* - : int = 0 *)
count_leaves not_symtree;; (* 4 *)

(*****************************************************)
(* 51. Collect the Leaves of a Binary Tree in a List *)

let rec leaves tree =
  match tree with
    | Empty -> []
    | Node(label, Empty, Empty) -> [label]
    | Node(_, left_tree, right_tree) -> (leaves left_tree) @ (leaves right_tree)
;;

leaves Empty;; (* - : 'a list = [] *)
leaves not_symtree;; (* [E; W; J; K] *)

(*************************************************************)
(* 52. Collect the Internal Nodes of a Binary Tree in a List *)

let rec internals tree =
  match tree with
    | Empty -> []
    | Node(_, Empty, Empty) -> []
    | Node(label, left_tree, right_tree) -> [label] @ (internals left_tree) @ (internals right_tree)
;;
internals (Node ('a', Empty, Empty));; (* - : char list = [] *)
internals not_symtree;; (* ['A'; 'B'; 'C'; 'D'; 'F'; 'G'; 'H'; 'I'] *)

(****************************************************)
(* 53. Collect the Nodes at a Given Level in a List *)

let rec at_level tree n =
    match tree with
      | Empty -> []
      | Node(label, left, right) -> if n = 1 then (* I reached the right level *) 
                                      [label]
                                    else
                                      (at_level left (n-1)) @ (at_level right (n-1))
;;
      
let example_tree = 
  Node ('a', Node ('b', Node ('d', Empty, Empty), Node ('e', Empty, Empty)),
  Node ('c', Empty, Node ('f', Node ('g', Empty, Empty), Empty)));;

at_level example_tree 2;;
(* - : char list = ['b'; 'c'] *)

(****************************************)
(* 54. Construct a Complete Binary Tree *)

(* Much more concise than the official solution and probably less efficient *)
let complete_binary_tree list =
  let rec aux curlabel =
    if List.mem curlabel list then
      Node(curlabel, aux (2*curlabel), aux (2*curlabel + 1))
    else
      Empty
  in
  aux 1
;;

let my_complete_tree = complete_binary_tree [1; 2; 3; 4; 5; 6];;
(* - : int binary_tree =
Node (1, Node (2, Node (4, Empty, Empty), Node (5, Empty, Empty)),
 Node (3, Node (6, Empty, Empty), Empty)) *)

let is_complete_binary_tree n t =
  let rec aux parent_label tree is_left =
    match tree with
      | Empty -> true
      | Node(label, left_tree, right_tree) ->
          if label <= 0 || label > n then false
          else if is_left && label != 2*parent_label then false
          else if (not is_left) && label != 2*parent_label + 1 then false
          else
            (aux label left_tree true) && (aux label right_tree false)
  in
  let rec count_nodes tree =
    match tree with Empty -> 0 | Node (_, left, right) -> 1 + (count_nodes left) + (count_nodes right)
  in
  if count_nodes t != n then false
  else 
    match t with Empty -> false | Node(_, left, right) -> 
      (aux 1 left true) && (aux 1 right false)
;;

is_complete_binary_tree 6 my_complete_tree;; (* true *)

let my_other_tree = Node (1, Node (2, Node (4, Empty, Empty), Node (5, Empty, Empty)),
 Node (3, Empty, Empty));; (* removed the node 6 -> size is wrong *)
is_complete_binary_tree 6 my_other_tree;; (* false *)
is_complete_binary_tree 5 my_other_tree;; (* true *)

let my_other_tree_2 = Node (1, Node (2, Node (4, Empty, Empty), Node (5, Empty, Empty)),
 Node (3, Empty, Node(6, Empty, Empty)));;
is_complete_binary_tree 6 my_other_tree_2;; (* false, node 6 should be on the left *)

let my_other_tree_3 = Node (1, Node (2, Node (4, Empty, Empty), Node (5, Empty, Empty)),
 Node (3, Node(7, Empty, Empty), Empty));;
is_complete_binary_tree 6 my_other_tree_3;; (* false, it should be 6 instead of 7 *)

(********************************)
(* 55. Layout a Binary Tree (1) *)

let example_layout_tree =
  let leaf x = Node (x, Empty, Empty) in
  Node ('n', Node ('k', Node ('c', leaf 'a',
                           Node ('h', Node ('g', leaf 'e', Empty), Empty)),
                 leaf 'm'),
       Node ('u', Node ('p', Empty, Node ('s', leaf 'q', Empty)), Empty));;

let layout_binary_tree_1 tree =  
  let rec aux tree cnt =
    match tree with
      | Empty -> (Empty, cnt)
      | Node(label, left, right) -> 
             let (leftnode, cnt1) = aux left cnt in
             let (rightnode, cnt2) = aux right (cnt1+1) in
              (Node((label, cnt1+1, 0), leftnode, rightnode), cnt2)
  in aux tree 0
;;

layout_binary_tree_1 example_layout_tree;;
(* - : (char * int * int) binary_tree =
Node (('n', 8, 1),
 Node (('k', 6, 2),
  Node (('c', 2, 3), Node (('a', 1, 4), Empty, Empty),
   Node (('h', 5, 4),
    Node (('g', 4, 5), Node (('e', 3, 6), Empty, Empty), Empty), Empty)),
  Node (('m', 7, 3), Empty, Empty)),
 Node (('u', 12, 2),
  Node (('p', 9, 3), Empty,
   Node (('s', 11, 4), Node (('q', 10, 5), Empty, Empty), Empty)),
  Empty)) *)
