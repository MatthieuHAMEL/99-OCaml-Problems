(*********************** ARITHMETICS ***************************)

(* 29. Determine Whether a Given Integer Number Is Prime *)

let is_prime n = 
  if n = 1 then false else
  let rec aux d =
    if (Float.of_int d) > Float.sqrt (Float.of_int n) then true
    else
      let rem = n mod d in
      if rem = 0 then false
      else aux (d+1)
  in aux 2
;;
      
not (is_prime 1);;
(*- : bool = true*)
is_prime 7;;
(*- : bool = true*)
not (is_prime 12);;
(*- : bool = true*)

(*****************************************************************************)
(* 30. Determine the Greatest Common Divisor of Two Positive Integer Numbers *)

let rec gcd a b = (* Euclid's Algorithm *)
  if a < b then gcd b a else
  let r = a mod b in
    if r = 0 then b
    else gcd b r
;;
gcd 13 27;;
(* - : int = 1 *)
gcd 20536 7826;;
(* - : int = 2 *)

(******************************************************************)
(* 31. Determine Whether Two Positive Integer Numbers Are Coprime *)

let coprime a b = (gcd a b = 1);;

coprime 13 27;;
(* - : bool = true *)
not (coprime 20536 7826);;
(* - : bool = true *)
coprime 13 26;;
(* - : bool = false *)

(***********************************************)
(* 32. Calculate Euler's Totient Function Φ(m) *)
(* "using the most primitive method possible *)

let phi m =
  let rec aux cnt k =
    if k >= m then cnt (* done *)
    else
      if coprime m k then aux (cnt+1) (k+1)
      else                aux  cnt    (k+1) 
  in aux 0 1
;;

phi 10;;
(* - : int = 4 *)

(***************************************************************)
(* 33. Determine the Prime Factors of a Given Positive Integer *)

let factors number =
  if is_prime number then [number] else
  let rec aux acc num k =
    if k = 0 then acc else
    if (not (is_prime k)) then aux acc num (k-1) else
    let r = num mod k in 
      if r != 0 then aux acc num (k-1)
      else (* found a prime factor! *) aux (k::acc) (num/k) k
  in
  aux [] number ((number/2) + 1)
;;

factors 315;;
(*- : int list = [3; 3; 5; 7]*)

(*******************************************************************)
(* 34. Determine the Prime Factors of a Given Positive Integer (2) *)
(* Construct a list containing the prime factors and their multiplicity. *)

(* Reusing my solution to problem 10 : *)
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


let factors_bis number = (* here the format is (elt, freq), which is the opposite of problem 10 *)
  List.map (fun (a, b) -> (b, a)) (encode (factors number));;

factors_bis 315;;
(*- : (int * int) list = [(3, 2); (5, 1); (7, 1)]*)

(**********************************************************)
(* 35. Calculate Euler's Totient Function Φ(m) (Improved) *)

(* There is no integer power function in OCaml *)
let pow number exponent =
  let rec aux pow_acc exponent =
    if exponent = 0 then pow_acc
    else aux (pow_acc*number) (exponent-1)
  in aux 1 exponent
;;

pow 5 0;;
pow 3 1;;
pow 7 2;;

let phi_improved m = 
  let factors_multi = factors_bis m in (* [(factor1, freq1); ...] *)
  let rec aux phi_acc factors =
    match factors with
      | [] -> phi_acc
      | (p1, m1)::restfactors -> aux ((p1-1) * (pow p1 (m1-1))) restfactors
  in
  aux 1 factors_multi
;;

phi_improved 10;;
(*- : int = 4*)
phi_improved 13;;
(*- : int = 12*)

(***********************************************************************)
(* 36. Compare the Two Methods of Calculating Euler's Totient Function *)

let timeit func param =
  let t1 = Sys.time() in
  let _ = func param in Sys.time() -. t1
;;

timeit phi 10090;;
timeit phi_improved 10090;;

(*******************************)
(* 37. A List of Prime Numbers *)

let all_primes low high = (* bounds included *)
  let rec aux acc num =
    if num > high then acc
    else if is_prime num then aux (num::acc) (num+1) (* as an optimization I could iterate on odd numbers only ... *)
    else aux acc (num+1)
  in aux [] low
;;

List.length (all_primes 2 7920);;
(* - : int = 1000 *)

(*****************************)
(* 38. Goldbach's Conjecture *)

let goldbach num =
  if (num mod 2) != 0 then raise (Failure "even number expected") else
  let rec aux cnt =
    if cnt < 2 then (* I didn't find the 2 numbers... *)
      raise (Failure "there is an error in my program or the goldbach conjecture is wrong") (* most likely the first explanation *)
    else if (is_prime cnt) && (is_prime (num - cnt)) then (* done *)
      (num - cnt, cnt)
    else 
      aux (cnt-1)
  in aux (num - 2) (* 0 and 1 cannot be part of the result so I start at num-2. *)
;;

goldbach 28;; (* - : int * int = (5, 23) *)
goldbach 1578;; (* (7, 1571) *)
goldbach 4;;    (* (2, 2) *)

(***************************************)
(* 39. A List of Goldbach Compositions *)

let goldbach_list low high =
  let rec aux acc num =
    if num > high then acc (* done *)
    else aux ((num, goldbach num)::acc) (num+2) (* iterate on odd numbers only *)
  in
  let start_low = if (low mod 2 = 0) then low else (low + 1) in (* skip low if it's odd *)
  List.rev (aux [] start_low)
;;

goldbach_list 9 20;;
(* - : (int * (int * int)) list =
[(10, (3, 7)); (12, (5, 7)); (14, (3, 11)); (16, (3, 13)); (18, (5, 13));
 (20, (3, 17))] *)
