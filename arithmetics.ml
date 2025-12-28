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



