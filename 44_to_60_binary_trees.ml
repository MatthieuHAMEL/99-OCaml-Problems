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
  let rec aux tree cnt h =
    match tree with
      | Empty -> (Empty, cnt)
      | Node(label, left, right) -> 
             let (leftnode, cnt1) = aux left cnt (h+1) in
             let (rightnode, cnt2) = aux right (cnt1+1) (h+1) in
              (Node((label, cnt1+1, h), leftnode, rightnode), cnt2)
  in aux tree 0 1
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

(********************************)
(* 56. Layout a Binary Tree (2) *)

let layout_binary_tree_2 tree =
  (* First get the height of the tree *)
  let rec height tree =
    match tree with
      | Empty -> 0
      | Node(_, left, right) -> 1 + (max (height left) (height right))
  in
  let tree_height = height tree in
  (* get the leftmost node height to determine the x coordinate of the root node *)
  let rec aux_leftmost_height tree =
    match tree with
      | Empty -> 0
      | Node(_, left, _) -> 1 + (aux_leftmost_height left)
  in
  let leftmost_height = aux_leftmost_height tree
  in
  (* the x distance between a child node and its root, the child node is at height h
     the x distance between two child nodes is 2*x_step *)
  let x_step h = 1 lsl (tree_height-h) in
  (* now add the x_steps from the leftmost height to the root to get the x coordinate of the root *)
  let rec aux_x_root acc cur_height =
    if cur_height <= 1 then acc (* root reached *)
    else aux_x_root (acc + (x_step cur_height)) (cur_height-1)
  in
  let x_root = aux_x_root 1 leftmost_height
  in
  (* now I can build up the tree with its layout *)
  let rec aux tree cur_height x =
    match tree with
      | Empty -> Empty
      | Node(label, left, right) -> 
              Node((label, x, cur_height), aux left (cur_height+1) (x - (x_step (cur_height+1))), 
                                            aux right (cur_height+1) (x + (x_step (cur_height+1))))
  in aux tree 1 x_root
;;

let example_layout_tree_2 =
  let leaf x = Node (x, Empty, Empty) in
  Node ('n', Node ('k', Node ('c', leaf 'a',
                           Node ('e', leaf 'd', leaf 'g')),
                 leaf 'm'),
       Node ('u', Node ('p', Empty, leaf 'q'), Empty));;

layout_binary_tree_2 example_layout_tree_2 ;;
(*- : (char * int * int) binary_tree =
Node (('n', 15, 1),
 Node (('k', 7, 2),
  Node (('c', 3, 3), Node (('a', 1, 4), Empty, Empty),
   Node (('e', 5, 4), Node (('d', 4, 5), Empty, Empty),
    Node (('g', 6, 5), Empty, Empty))),
  Node (('m', 11, 3), Empty, Empty)),
 Node (('u', 23, 2),
  Node (('p', 19, 3), Empty, Node (('q', 21, 4), Empty, Empty)), Empty))*)


(********************************)
(* 57. Layout a Binary Tree (3) *)

(* There is quite a bit of code but I didn't find the problem "difficult" 
   as the assignment says, ... compared to 48. or 44.
   (though it may be difficult to get an optimal solution!) *)

let layout_binary_tree_3 tree =
  let rec collect_left_or_rightmosts acc cur_h tree is_left =
    match tree with
      | Empty -> acc
      | Node((label, x, y), left, right) ->
          let new_acc =
            let opt_left_or_rightmost_h = List.assoc_opt cur_h acc in
            match opt_left_or_rightmost_h with
              | None -> ((cur_h, x)::acc)
              | Some min_or_max -> if (is_left && x < min_or_max) || ((not is_left) && x > min_or_max) then
                               ((cur_h, x)::(List.remove_assoc cur_h acc))
                             else
                               acc
          in
          collect_left_or_rightmosts (collect_left_or_rightmosts new_acc (cur_h+1) right is_left) (cur_h+1) left is_left
  in
  (* are the rightmost coordinates (height_i, rightX_i) colliding with (height_i, leftX_i) for each height i ? *)
  let conflict rightmosts_x leftmosts_x =
    let sorted_rights = List.sort (fun (h1, x1) (h2, x2) -> compare h1 h2) rightmosts_x in
    let sorted_lefts = List.sort (fun (h1, x1) (h2, x2) -> compare h1 h2) leftmosts_x in
    let rec aux rights lefts =
      match (rights, lefts) with
        | ([], []) | ([], _) | (_, []) -> false (* done, no conflict *)
        | ((curhr, rightX)::restrights, (curhl, leftX)::restlefts) ->
            if rightX = leftX then (* conflict *) true else aux restrights restlefts
    in aux sorted_rights sorted_lefts
  in
  (* Process and build the tree starting from x, y *)
  let rec aux cur_step x y tree =
    match tree with
      | Empty -> Empty
      | Node(label, left, right) -> 
        let left_subtree = aux 1 (x-cur_step) (y+1) left in
        let rightmosts = collect_left_or_rightmosts [] (y+1) left_subtree false in
        let right_subtree = aux 1 (x+cur_step) (y+1) right in
        let leftmosts = collect_left_or_rightmosts [] (y+1) right_subtree true in
       if conflict rightmosts leftmosts then
         aux (cur_step+1) x y tree (* try again and put some more space between the two subtrees. *)
       else
         Node((label, x, y), left_subtree, right_subtree)
  in
  (* I build the correct tree but with the root node at x = 0 first.
     It doesn't seem possible to get the leftmost coordinate before building the whole tree 
     (since the right branches can "overlap" with branches from the left, as long as they don't collide) *)
  let res_0_centered = aux 1 0 1 tree 
  in
  (* Last step: shift the whole tree to the right so that the lowest x is 1. *)
  let rec min_x tree =
    match tree with
      | Empty -> 0
      | Node((_, x, _), left, right) -> min (min x (min_x left)) (min_x right)
  in
  let rec shift_x tree step_x =
    match tree with
      | Empty -> Empty
      | Node((label, x, y), left, right) -> Node((label, x+step_x, y), shift_x left step_x, shift_x right step_x)
  in
  shift_x res_0_centered (- (min_x res_0_centered) + 1)
;;

let example_tree_from_the_assignment_drawing =
  let leaf x = Node (x, Empty, Empty) in
  Node('n', Node('k', Node('c', leaf 'a',
                                Node('e', leaf 'd', leaf 'g')), 
                      leaf 'm'), 
            Node('u', Node('p', Empty, leaf 'q'), Empty));;
    
layout_binary_tree_3 example_tree_from_the_assignment_drawing;;
(* Node (('n', 5, 1),
 Node (('k', 3, 2),
  Node (('c', 2, 3), Node (('a', 1, 4), Empty, Empty),
   Node (('e', 3, 4), Node (('d', 2, 5), Empty, Empty),
    Node (('g', 4, 5), Empty, Empty))),
  Node (('m', 4, 3), Empty, Empty)),
 Node (('u', 7, 2),
  Node (('p', 6, 3), Empty, Node (('q', 7, 4), Empty, Empty)), Empty))
*)

let example_layout_tree_3 =
  let leaf x = Node (x, Empty, Empty) in
  Node ('n', Node ('k', Node ('c', leaf 'a',
                           Node ('h', Node ('g', leaf 'e', Empty), Empty)),
                 leaf 'm'),
       Node ('u', Node ('p', Empty, Node ('s', leaf 'q', Empty)), Empty));;

layout_binary_tree_3 example_layout_tree_3;;
(* - : (char * int * int) binary_tree =
Node (('n', 5, 1),
 Node (('k', 3, 2),
  Node (('c', 2, 3), Node (('a', 1, 4), Empty, Empty),
   Node (('h', 3, 4),
    Node (('g', 2, 5), Node (('e', 1, 6), Empty, Empty), Empty), Empty)),
  Node (('m', 4, 3), Empty, Empty)),
 Node (('u', 7, 2),
  Node (('p', 6, 3), Empty,
   Node (('s', 7, 4), Node (('q', 6, 5), Empty, Empty), Empty)),
  Empty)) *)

(***********************************************)
(* 58. A String Representation of Binary Trees *)

let example_layout_tree =
  let leaf x = Node (x, Empty, Empty) in
    (Node ('a', Node ('b', leaf 'd', leaf 'e'),
     Node ('c', Empty, Node ('f', leaf 'g', Empty))))
;;

let rec string_of_tree tree =
  match tree with
    | Empty -> ""
    | Node(label, Empty, Empty) -> (String.make 1 label)
    | Node(label, left, right) -> (String.make 1 label) ^ "(" ^ (string_of_tree left) ^ "," ^ (string_of_tree right) ^ ")"
;;

string_of_tree example_layout_tree;; (* a(b(d,e),c(,f(g,))) *)


let rec tree_of_string str =
  let rec aux_parse_groups paren_cnt str first_paren_idx comma_idx cur_idx =
     (* returns (a, z, b) where a is the index of the 1st parenthesis, b the matching closing parenthesis and z the index of the comma *)
    if str = "" then (0, 0, 0) (* there was no parentheses pattern so it was probably a leaf *) 
    else 
    let ci = str.[0] in
    let rest = String.sub str 1 ((String.length str) - 1) in
    if ci = '(' then
      let my_first_paren_idx = if paren_cnt = 0 then cur_idx else first_paren_idx in
      aux_parse_groups  (paren_cnt + 1) rest my_first_paren_idx comma_idx (cur_idx+1)
    else if ci = ',' then
      let my_comma_idx = if paren_cnt = 1 then cur_idx else comma_idx in
      aux_parse_groups  paren_cnt rest first_paren_idx my_comma_idx (cur_idx+1)
    else if ci = ')' then
      let new_paren_count = paren_cnt - 1 in
      if new_paren_count = -1 then (0, 0, 0) (* we're not in a valid pattern *)
      else if new_paren_count = 0 then (* done *) (first_paren_idx, comma_idx, cur_idx)
      else aux_parse_groups new_paren_count rest first_paren_idx comma_idx (cur_idx+1)
    else
      aux_parse_groups paren_cnt rest first_paren_idx comma_idx (cur_idx+1)
  in
  let parse_groups str = aux_parse_groups 0 str 0 0 0
  in  
  if str = "" then Empty
  else 
    let (fp, c, lp) = parse_groups str in
    let subleft = if c - fp - 1 = -1 then "" else String.sub str (fp + 1) (c - fp - 1) in
    let subright = if lp - c - 1 = -1 then "" else String.sub str (c + 1) (lp - c - 1) in
    Node(str.[0], tree_of_string subleft, tree_of_string subright)
;; 

tree_of_string "a(b(d,e),c(,f(g,)))";; 
(* - : char binary_tree =
Node ('a', Node ('b', Node ('d', Empty, Empty), Node ('e', Empty, Empty)),
 Node ('c', Empty, Node ('f', Node ('g', Empty, Empty), Empty)))
 *)


(******************************************************)
(* 59. Preorder and Inorder Sequences of Binary Trees *)

let rec preorder tree =
  match tree with
    | Empty -> []
    | Node(label, left, right) -> label :: (preorder left) @ (preorder right)
;;

preorder (Node (1, Node (2, Empty, Empty), Empty));;
(* - : int list = [1; 2] *)

preorder example_layout_tree;;
(* - : char list = ['a'; 'b'; 'd'; 'e'; 'c'; 'f'; 'g'] *)


let rec inorder tree =
  match tree with
    | Empty -> []
    | Node(label, left, right) -> (inorder left) @ [label] @ (inorder right)
;;

inorder (Node (1, Node (2, Empty, Empty), Empty));;
(* - : int list = [2; 1] *)

inorder example_layout_tree;;
(* - : char list = ['d'; 'b'; 'e'; 'a'; 'c'; 'g'; 'f'] *)

(* Question 2: no, I can't start from a preorder sequence as shown above to build the corresponding tree (it's ambiguous!) *)
(* Question 3: *)

let pre_in_tree preorder_seq inorder_seq =
  let rec aux preord inord parent =
   match preord with (* also returns the "right subtree" part of preord once it has been recursively processed *)
     | [] -> (Empty, [])
     | [head] -> (Node(head, Empty, Empty), [])
     | ph::succ1::preord_rest -> (* ph is the current node label. What I need to know is whether succ1 and succ2 are its children or not *)
         match inord with
           | ih::isuc::inord_rest -> if ih = ph then (* the head node didn't have a left child since it is first in the inorder sequence *)
                             let opt_isuc = Some isuc in
                             if opt_isuc = parent then (* ph represents a leaf *)
                               (Node(ph, Empty, Empty), (succ1::preord_rest))
                             else if isuc = succ1 then (* maybe this case is covered by the next one. TODO *)
                               (Node(ph, Empty, Node(succ1, Empty, Empty)), preord_rest)
                             else (* The inorder sequence has something else than succ1 after ph -> recursion *)
                               let (right_branch, _) = aux (succ1::preord_rest) (isuc::inord_rest) (Some ph) in
                               (Node(ph, Empty, right_branch), [])
                           else (* There is something before ph in the inorder seq.
                                   I must be able to find ph somewhere in the inorder list to build the right child... *)
                              let (left, right_preorder) = aux (succ1::preord_rest) inord (Some ph) in
                              let inorder_after_ph = List.drop_while (fun x -> x != ph) inord in
                              let (right, _) = match preord_rest with
                                | _::_ -> aux right_preorder (List.tl inorder_after_ph) (Some ph)
                                | [] -> (Empty, []) in
                              (Node(ph, left, right), [])
           | [e] -> if e = ph then (Node(ph, Empty, Empty),[]) else raise (Failure "inorder cannot contain a single element different from ph")
           | [] -> raise (Failure "inorder seq cannot be empty")
  in
  aux preorder_seq inorder_seq None
;;                           

let newtree = pre_in_tree ['a'; 'b'; 'd'; 'e'; 'c'; 'f'; 'g'] ['d'; 'b'; 'e'; 'a'; 'c'; 'g'; 'f'];;

