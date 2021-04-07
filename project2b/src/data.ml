open Funs

(************************)
(* Part 2: Integer BSTs *)
(************************)

type int_tree =
  | IntLeaf
  | IntNode of int * int_tree * int_tree

let empty_int_tree = IntLeaf

let rec int_insert x t =
  match t with
  | IntLeaf -> IntNode(x, IntLeaf, IntLeaf)
  | IntNode (y, l, r) when x > y -> IntNode (y, l, int_insert x r)
  | IntNode (y, l, r) when x = y -> t
  | IntNode (y, l, r) -> IntNode (y, int_insert x l, r)

let rec int_mem x t =
  match t with
  | IntLeaf -> false
  | IntNode (y, l, r) when x > y -> int_mem x r
  | IntNode (y, l, r) when x = y -> true
  | IntNode (y, l, r) -> int_mem x l

(* Implement the functions below. *)

let rec int_size t = 
  match t with
  | IntLeaf -> 0
  | IntNode (y, l, r) ->  1 + (int_size l) + (int_size r)
;;

let rec int_max t = 
  match t with
  | IntLeaf -> raise (Invalid_argument("int_max"))
  | IntNode (y, l, r) when r = empty_int_tree -> y
  | IntNode (y, l, r) -> int_max r

;;

(****************************)
(* Part 3: Polymorphic BSTs *)
(****************************)

type 'a atree =
  | Leaf
  | Node of 'a * 'a atree * 'a atree

type 'a compfn = 'a -> 'a -> int
type 'a ptree = 'a compfn * 'a atree

let empty_ptree f : 'a ptree = (f,Leaf)

let rec fold f a xs = match xs with
  | [] -> a
  | x :: xt -> fold f (f a x) xt
;;
(* Implement the functions below. *)


(* *)

(*-------------------------------------------*)

(* helper function that takes fn and atree  -> ptree*)




let pinsert x t = (* a -> ptree -> ptree *)
match t with 
|(fn, atree) -> 
let rec helper x atree =
  match atree with
|Leaf -> Node(x,Leaf, Leaf)
|Node(y, l, r) when ((fn x y) > 0) -> Node(y, l, (helper x r) )(* (fn, ) *)
|Node(y, l, r) when ((fn x y) = 0) ->  atree
|Node(y, l, r) ->  Node(y, helper x l, r)
in (fn, (helper x atree))
;;

let pmem x t = (*'a -> 'a ptree -> bool*)
 match t with
 | (fn, atree) -> let rec search x node = 
    match node with
    |Leaf -> false
    | Node (y, l, r) when (fn x y) = 0 -> true
    | Node (y, l, r) when (fn x y) < 0 -> search x l
    | Node (y, l, r) -> search x r
    in search x atree
  ;;



let pinsert_all lst t = 
  fold (fun acc el -> pinsert el acc) t lst
;;

 (* let reverse_help xs = fold (fun a x -> x :: a) [] xs;;  *)

 let rec p_as_list t = (*'a ptree ->   'a list *) (*left side, root, right side*)
 match t with
 |(fn, node)-> let rec list_func tree = 
               match tree with
               | Leaf -> []
               | Node(y, l, r) -> (list_func l) @ (y::list_func r)
         
    in list_func node;;

(* let rec p_as_list t = 
  match t with
  |(fn, atree)-> 
    match atree with
    |Leaf-> []
    |Node(y, l, r)-> reverse_help (y:: (p_as_list l) @ p_as_list r)

;;  *)

let pmap f t = (* ('a -> 'a) -> 'a ptree -> 'a ptree *)
 match t with 
 |(fn, atree)->
  let newlist = map f (p_as_list t) in
 pinsert_all newlist (fn, Leaf)
;;

(***************************)
(* Part 4: Variable Lookup *)
(***************************)

(* Modify the next line to your intended type *)


type lookup_table = (string * int) list list

let empty_table () : lookup_table = [];;

let push_scope (table: lookup_table) : lookup_table = 
  [] :: table;;

let pop_scope (table: lookup_table) : lookup_table = 
  match table with
  |h::t -> t
  |_ -> failwith "No scopes remain!"
;;

let add_var name value (table: lookup_table) : lookup_table =
  match table with
  |scope_h::scope_t -> ((name, value)::scope_h)::scope_t
  |_ -> failwith "There are no scopes to add a variable to!"
;;

(* let rec lookup_helper scpe name  = 
  match scpe with 
  |[] -> false
  |h::t -> match h with 
        |(n, v) -> if n = name then true else lookup_helper t name
;;

let rec get_value scpe name = 
  match scpe with 
  |[] -> 0
  |h::t -> match h with
          |(n,v) -> if n = name then v else get_value t name
;; *)

let rec lookup name (table: lookup_table) = match table with
  [] -> failwith "Variable not found!" (* h is a list of tuples*) 
  | h::t -> let rec helper lst name = 
	match lst with 
            | h2::t2 -> match h2 with (* h2 is a tuple *)
		|(n, v) -> if n = name then v else helper t name
		
in helper h name ;;

(* 

          |scopelst : Tup(n,v) -> if (lookup_helper scopelst name) = false 
                                  then failwith "Variable not found!"
                                  else  *)



(*******************************)
(* Part 5: Shapes with Records *)
(*******************************)

type pt = { x: int; y: int };;
type shape =
  | Circ of { radius: float; center: pt }
  | Square of { length: float; upper: pt }
  | Rect of { width: float; height: float; upper: pt }
;;

(* Implement the functions below. *)

let area s = match s with
|Circ { radius = radius; center = center } ->  radius *. radius *. 3.14
|Square { length = length; upper = u } -> length *. length
|Rect {width = width; height = height; upper = upper} -> width *. height
;;

let rec filter f lst = match lst with
 |[] -> []
 |h::t -> if (f h) = true then h::(filter f t) else filter f t
;;







