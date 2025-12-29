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

