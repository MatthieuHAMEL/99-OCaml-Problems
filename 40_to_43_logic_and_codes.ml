(*********************** LOGIC AND CODES ***************************)

(* 40. Truth Tables for Logical Expressions (2 Variables) *)

type bool_expr =
  | Var of string
  | Not of bool_expr
  | And of bool_expr * bool_expr
  | Or of bool_expr * bool_expr;;

(* this is a bool_expr : *)
And (Or (Var "a", Var "b"), And (Var "a", Var "b"));;

let table2 var1 var2 expr =
  let rec evaluate iExpr val1 val2 =
    match iExpr with
      | Var v -> if v = var1 then val1 else val2
      | Not expr -> not (evaluate expr val1 val2)
      | And (expr1, expr2) -> (evaluate expr1 val1 val2) && (evaluate expr2 val1 val2)
      | Or (expr1, expr2) ->  (evaluate expr1 val1 val2) || (evaluate expr2 val1 val2)
  in (* there are 4 possible states; since problem 41 generalizes anyway, let's just enumerate them here *)
    [(true, true,   evaluate expr true true);
     (true, false,  evaluate expr true false);
     (false, true,  evaluate expr false true);
     (false, false, evaluate expr false false)]
  ;;

table2 "a" "b" (And (Var "a", Or (Var "a", Var "b")));;
(* - : (bool * bool * bool) list =
[(true, true, true); (true, false, true); (false, true, false);
 (false, false, false)] *)

(********************************************)
(* 41. Truth Tables for Logical Expressions *)

let table iVars expr =
  let rec value_of var varlist =
    match varlist with
      | [] -> raise (Failure "variable not found")
      | (varname, varval)::restvars -> if varname = var then varval else value_of var restvars
  in
  let rec evaluate iExpr varlist =
    match iExpr with
      | Var v -> value_of v varlist
      | Not expr -> not (evaluate expr varlist)
      | And (expr1, expr2) -> (evaluate expr1 varlist) && (evaluate expr2 varlist)
      | Or (expr1, expr2) ->  (evaluate expr1 varlist) || (evaluate expr2 varlist)
  in

  let rec aux_all_possibilities acc_res seed vars =  (* [[(var1, true); (var2, true); (var3, true);]; [etc ; etc] *) 
    if vars = [] then seed
    else match seed with (* put true and false in front of every element of the seed *)
      | [] -> aux_all_possibilities [] acc_res (List.tl vars)  (* acc_res becomes the seed *)
      | headseed::restseeds -> aux_all_possibilities ([(List.hd vars, true)::headseed ; (List.hd vars, false)::headseed] @ acc_res) restseeds vars
  in
  let all_possibilities vars =
    let tmpvars = List.rev vars in (* to get the result in the right order *)
    aux_all_possibilities [] [[(List.hd tmpvars, true)];[(List.hd tmpvars, false)]] (List.tl tmpvars)
  in

  let rec aux acc varlist = (* [[("var1", true), ("var2"), false  etc ]; for all possibilities ...*)
    match varlist with
      | [] -> acc
      | headvarlist::restlists -> aux ((headvarlist, evaluate expr headvarlist)::acc) restlists
  in
  aux [] (all_possibilities iVars)
 ;;


table ["a"; "b"] (And (Var "a", Or (Var "a", Var "b")));;
(* - : ((string * bool) list * bool) list =
[([("a", true); ("b", true)], true); ([("a", true); ("b", false)], true);
 ([("a", false); ("b", true)], false); ([("a", false); ("b", false)], false)] *)

(*****************)
(* 42. Gray Code *)

let gray n =
  let rec append_0_or_1 acc strlist strdigit = (*appends 0 or 1 to every element *)
    match strlist with
      | [] -> List.rev acc
      | headstr::reststrs -> append_0_or_1 ((strdigit^headstr)::acc) reststrs strdigit
  in
  let rec aux acc cnt =
    if cnt >= n then acc
    else
      let first_half = (append_0_or_1 [] acc "0") in
      let second_half = (append_0_or_1 [] (List.rev acc) "1") in
      aux (first_half @ second_half) (cnt+1)
  in
  aux ["0";"1"] 1
;;

gray 1;;
(* - : string list = ["0"; "1"] *)
gray 2;;
(* - : string list = ["00"; "01"; "11"; "10"] *)
gray 3;;
(* - : string list = ["000"; "001"; "011"; "010"; "110"; "111"; "101"; "100"] *)

(********************)
(* 43. Huffman Code *)
