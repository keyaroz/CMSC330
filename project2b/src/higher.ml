open Funs

(********************************)
(* Part 1: High Order Functions *)
(********************************)

(* Returns how many elements in lst are equal to target. *)
let count_occ lst target = 
  match lst with
  |[]-> 0
  |x::xs ->
    fold (fun a x -> if x = target then a+1 else a) 0 lst
;;

(* Given a list, returns a list with all duplicate elements removed. Order does not matter. *)
let uniq lst = 
  match lst with
  |[]-> []
  |[a]-> [a]
  |h::t-> fold (fun alst x -> if (count_occ alst x) = 0 then x::alst else alst) [] lst
;;



let assoc_list lst =  
  match lst with
    |[]-> []
    (* |[a]-> [(a,a)] *)
    |h::t-> 
    fold (fun alst x -> if (count_occ lst x) = 0 then ((x,1)::alst) 
    else (x, count_occ lst x)::alst) [] (uniq lst)
;;


let ap fns args = 
 fold_right (fun fn alst -> (map fn args) @ alst) fns []
;;
