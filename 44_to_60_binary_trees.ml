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
             let trees = (List.map (fun (t1, u1) -> Node('x', t1, u1)) all_combis) @ (List.map (fun (t1, u1) -> Node('x', u1, t1)) all_combis) in
             List.sort_uniq (fun x1 x2 -> if x1 = x2 then 0 else -1) trees (* eliminate duplicates *)
;;

cbal_tree 0;;
cbal_tree 1;;
cbal_tree 2;;
cbal_tree 3;;
cbal_tree 4;;

