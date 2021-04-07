open List
open Sets

(*********)
(* Types *)
(*********)

type ('q, 's) transition = 'q * 's option * 'q

type ('q, 's) nfa_t = {
  sigma: 's list; (* list of alphabet*)
  qs: 'q list; (* list of states like 0, 1, etc*)  (* [1;2;3;4] *)
  q0: 'q; (* initial state *)
  fs: 'q list; (* final states*)
  delta: ('q, 's) transition list; (* list of tuples (1, "a", 2) *)
}

(***********)
(* Utility *)
(***********)

(* explode converts a string to a character list *)
let explode (s: string) : char list =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l)
  in
  exp (String.length s - 1) []

(****************)
(* Part 1: NFAs *)
(****************)

let to_option x = Some x;;

let option_list lst = map (fun x-> to_option x) lst;;

(* let movehelp lst qs s a = 
  fold_left (fun acc q -> if (q = a && s = b) && (List.mem a qs) && ((List.mem c acc) != true) then c::acc else acc) lst qs;;
 ;;  *)


let move (nfa: ('q,'s) nfa_t) (qs: 'q list) (s: 's option) : 'q list =
  fold_left (fun acc tup -> match tup with 
  | (a,b,c) -> if (b = s && List.mem a qs) && ((List.mem c acc) != true) then c::acc else acc

) [] nfa.delta
;;   
       (*return a list of possible next states you can move to*)



let fold_help tup lst qs =
  match tup with 
  |(a,b,c) -> fold_left (fun acc x -> if (x=a && b = None) && (List.mem a qs) && (List.mem c acc != true) then c::acc else acc) lst qs

;;

let sort_lst lst = 
  let compares x y = if x < y then -1 else if x > y then 1 else 0 in
  sort compares lst;;
;;

let rec e_help nfa qs = 
if (sort_lst (union (move nfa qs None) qs)) = (sort_lst qs) then ( union (move nfa qs None) qs) else e_help nfa (union (move nfa qs None) qs)
;;

let e_closure (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list =   
  e_help nfa qs
;; 




let rec is_final finals currstates = (* Checks if anything in currstates is a final state *)
  match finals with 
  |[] -> false
  |h::t-> if (List.mem h currstates) = true then true else is_final t currstates        
;;


let to_option x = Some x;;

let s_option_list s = map (fun x -> to_option x) (explode s);;



(*curr is the current set of states *)
(* ho can i go through every final state*)
let rec helper_accept nfa charlst curr = (* list of chars *)
  match charlst with
  |[]-> if (is_final nfa.fs curr) then true else false
  |h::t ->  if e_closure nfa ( move nfa curr h ) = [] then false else helper_accept nfa t (e_closure nfa (move nfa curr h))
;;



  let accept (nfa: ('q,char) nfa_t) (s: string) : bool = 
 if s = "" && (List.mem nfa.q0 nfa.fs) then true else if s = "" then false else
   (helper_accept nfa (s_option_list s) (e_closure nfa [nfa.q0])) 
  ;;   
   (* passing in the first possible set of start positions and 
   the helper will continue from here
   *)

  

(*******************************)
(* Part 2: Subset Construction *)
(*******************************)

let compare a b = if a < b then -1 else if a > b then 1 else 0;;

let new_states_helper nfa qs sigmalist =
  fold_left (fun acc x -> (e_closure nfa (move nfa (e_closure nfa qs) x ) :: acc) ) [] sigmalist
;; 



let new_states (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list =
 new_states_helper nfa qs (option_list nfa.sigma)
;;



let rec transhelper nfa qslst options qs =
  fold_left (fun acc x -> (qs, x, e_closure nfa (move nfa (e_closure nfa qs) x)) :: acc)  [] options
;;

let new_trans (nfa: ('q,'s) nfa_t) (qs: 'q list) : ('q list, 's) transition list =
  transhelper nfa (new_states nfa qs) (option_list nfa.sigma) qs

;;


let new_finals (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list =
   if is_final nfa.fs qs then [qs] else [];;

;;

let step_help nfa dfa w = (*DFA*)
  {  
    
    qs = union dfa.qs [w];
    sigma = nfa.sigma;
    delta = union dfa.delta (new_trans nfa w);
    q0 = e_closure nfa [nfa.q0];
    fs = union dfa.fs (new_finals nfa w);
  }

 ;; 

let rec nfa_to_dfa_step (nfa: ('q,'s) nfa_t) (dfa: ('q list, 's) nfa_t)
    (work: 'q list list) : ('q list, 's) nfa_t =

    match work with
    |[]-> dfa
    |h::t -> fold_left (fun acc x-> if (elem (e_closure nfa x) (step_help nfa acc h).qs)  
    then (step_help nfa acc h) else nfa_to_dfa_step nfa (step_help nfa acc h) [e_closure nfa x]) 
  dfa (*accumulator*)
  (new_states nfa h) (*list*)

;; 

let the_dfa nfa = 
  let new_q0 = e_closure nfa [nfa.q0] in
  {
    sigma= nfa.sigma; (* list of alphabet*)
    qs= []; (* list of states like 0, 1, etc*)
    q0= new_q0; (* initial state *)
    fs= []; (* final states*)
    delta= []; (* list of tuples (1, "a", 2) *)
  }
;;

let nfa_to_dfa (nfa: ('q,'s) nfa_t) : ('q list, 's) nfa_t =
  let new_q0 = e_closure nfa [nfa.q0] in

  nfa_to_dfa_step nfa (the_dfa nfa) [new_q0]

  
;;

