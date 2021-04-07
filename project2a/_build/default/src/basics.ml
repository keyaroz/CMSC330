(***********************************)
(* Part 1: Non-Recursive Functions *)
(***********************************)

let rev_tup tup =           (* DONE *)
    match tup with
   
    |(a,b,c)->(c,b,a)
;;

let abs x =                  (* DONE *)
   if x < 0 then x*(-1)
   else x
;;

let area x y =               (* DONE *)
    match x with
    |(a,b)->
        match y with
            |(d,e)-> abs((a-d) * (b-e))
;;


let volume x y =             (* DONE *)
    match x,y with 
    |(a,b,c), (d,e,f) -> abs((a-d) * (b-e) * (c-f))
;;

(*******************************)
(* Part 2: Recursive Functions *)
(*******************************)

let rec factorial x =            (* DONE *)
    if x > 1 then x * factorial(x-1) else 1 ;;

let rec pow x y =                (* DONE *)
        if y=0 then 1 
        else if y=1 then x
        else x * pow x (y-1)

let rec log x y =  
    if  (x-1 >= y) then 0 
    else (log x (truncate(float_of_int y /. float_of_int x))) + 1
;;


(* let rec is_prime x = (*ASK TA*)
    match x with 
    |1-> false
    |_->
;; *)

    (* let rec divisor d =
     if ((x mod d) <> 0) && (divisor (d+1)) then true 
     in divisor 2
;; *)

let rec is_prime x =                (* DONE *)
    if x = 1 then false else
    let rec divisor d =
        (d * d) > x || (x mod d <> 0 && divisor (d+1)) in
        divisor 2
;;

let rec next_prime x = 
    if (is_prime x) then x
    else next_prime(x+1)
;;




(*****************)
(* Part 3: Lists *)
(*****************)


 let rec get idx lst =       (*get idx index of list*)      (* DONE *)
            match lst with
            |[] -> failwith "Out of bounds"
            |h::t-> if idx = 0 then h
                else get (idx-1) t
;; 


let larger lst1 lst2 =                  (*DONE*)
    let rec length lst =
        match lst with
        |[]-> 0
        |h::t-> 1 + length t in
    
        if (length lst1) > (length lst2) then lst1
            else if (length lst2) > (length lst1) then lst2
            else [] (* if same length*)
;;


let reverse lst =                            (* DONE *)
    let rec helper l newlst = 
        match l with
        |[] -> newlst
        |h::t -> helper t (h::newlst) in helper lst []
;;

let rec combine lst1 lst2 =                  (* DONE *)
    match lst1 with
    |[]-> lst2
    |h::t-> h::combine t lst2
;;



let rec rotate shift lst = 
    
    let rec helper l n =           
        match l with
        |[] -> []
        |h::t-> if (n < shift) then helper (combine t [h]) (n+1)
        else l
        in helper lst 0 

;;
