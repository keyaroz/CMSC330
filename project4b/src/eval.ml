open SmallCTypes
open EvalUtils
open TokenTypes


exception TypeError of string
exception DeclareError of string
exception DivByZeroError

let rec pow x y =
  if y > 0 then 
  x * (pow x (y-1)) else 1
 ;; 



 let rec find x env =
  match env with
  |[]-> raise(DeclareError "failed to find")
  |(id, v)::t -> if id = x then v else find x t ;;

let rem target ev =
List.remove_assoc target ev;; 


let rec eval_expr env t =
  match t with 
  
  | Int i -> Int_Val i

  | Bool b -> Bool_Val b

  | ID id -> find id env

  | Add (exp1, exp2) -> (let ev1 = eval_expr env exp1 in
                        let ev2 = eval_expr env exp2 in
                        match ev1 with
                        |Int_Val x1 -> (match ev2 with
                                      |Int_Val x2 -> Int_Val (x1+x2)
                                      |_ -> raise (TypeError "Invalid type"))
                        |_ -> raise (TypeError "Invalid type Add")

  )

  | Sub (exp1, exp2) -> (let ev1 = eval_expr env exp1 in
                        let ev2 = eval_expr env exp2 in
                        match ev1 with
                        |Int_Val x1 -> (match ev2 with
                                      |Int_Val x2 -> Int_Val (x1-x2)
                                      |_ -> raise (TypeError "Invalid type"))
                        |_ -> raise (TypeError "Invalid type sub")
  )

  | Mult (exp1, exp2) -> (let ev1 = eval_expr env exp1 in
                        let ev2 = eval_expr env exp2 in
                        match ev1 with
                        |Int_Val x1 -> (match ev2 with
                                      |Int_Val x2 -> Int_Val (x1*x2)
                                      |_ -> raise (TypeError "Invalid type"))
                        |_ -> raise (TypeError "Invalid type mult")
                        )

  | Div (exp1, exp2) -> (let ev1 = eval_expr env exp1 in
                        let ev2 = eval_expr env exp2 in
                        match ev1 with
                        |Int_Val x1 -> (match ev2 with
                                        |Int_Val x2 -> (if (x2 = 0) then raise (DivByZeroError)
                                          else Int_Val (x1/x2))
                                      |_ -> raise (TypeError "Invalid type")
                                      )
                        |_ -> raise (TypeError "Invalid type div")
)

  | Pow (exp1, exp2) ->( let ev1 = eval_expr env exp1 in
                        let ev2 = eval_expr env exp2 in
                        match ev1 with
                        |Int_Val x1 -> (match ev2 with
                                      |Int_Val x2 -> Int_Val (pow x1 x2)
                                      |_ -> raise (TypeError "Invalid type"))
                        |_ -> raise (TypeError "Invalid type pow")
  )

  | Greater (exp1, exp2) -> (let ev1 = eval_expr env exp1 in
                            let ev2 = eval_expr env exp2 in
                            match ev1 with
                            |Int_Val x1 -> (match ev2 with
                                      |Int_Val x2 -> if x1 > x2 then Bool_Val true else Bool_Val false
                                      |_ -> raise (TypeError "Invalid type"))
                            |_ -> raise (TypeError "Invalid type greater")
  )
  

  | Less (exp1, exp2) -> (let ev1 = eval_expr env exp1 in
                            let ev2 = eval_expr env exp2 in
                            match ev1 with
                            |Int_Val x1 -> (match ev2 with
                                      |Int_Val x2 -> if x1 < x2 then Bool_Val true else Bool_Val false
                                      |_ -> raise (TypeError "Invalid type"))
                            |_ -> raise (TypeError "Invalid type less")
  )

  | GreaterEqual (exp1, exp2) -> (let ev1 = eval_expr env exp1 in
                            let ev2 = eval_expr env exp2 in
                            match ev1 with
                            |Int_Val x1 -> (match ev2 with
                                      |Int_Val x2 -> if x1 >= x2 then Bool_Val true else Bool_Val false
                                      |_ -> raise (TypeError "Invalid type"))
                            |_ -> raise (TypeError "Invalid type greaterequal")
  )


  | LessEqual (exp1, exp2) -> (let ev1 = eval_expr env exp1 in
                            let ev2 = eval_expr env exp2 in
                            match ev1 with
                            |Int_Val x1 -> (match ev2 with
                                      |Int_Val x2 -> if x1 <= x2 then Bool_Val true else Bool_Val false
                                      |_ -> raise (TypeError "Invalid type"))
                            |_ -> raise (TypeError "Invalid type lesseq")
                            )

  | Equal (exp1, exp2) -> (let ev1 = eval_expr env exp1 in
                            let ev2 = eval_expr env exp2 in
                            match ev1 with
                            |Int_Val x1 -> (match ev2 with
                                      |Int_Val x2 -> if (x1 = x2) then Bool_Val true else Bool_Val false
                                      |_ -> raise (TypeError "Invalid type"))
                            |Bool_Val x1 -> (match ev2 with
                                      |Bool_Val x2 -> if (x1 = x2) then Bool_Val true else Bool_Val false
                                      |_ -> raise (TypeError "Invalid type"))
                        
  )

  | NotEqual (exp1, exp2)  -> (let ev1 = eval_expr env exp1 in
                            let ev2 = eval_expr env exp2 in
                            match ev1 with
                            |Int_Val x1 -> (match ev2 with
                                      |Int_Val x2 -> if x1 <> x2 then Bool_Val true else Bool_Val false
                                      |_ -> raise (TypeError "Invalid type"))


                            |Bool_Val x1 -> (match ev2 with
                                      |Bool_Val x2 -> if (x1 <> x2) then Bool_Val true else Bool_Val false
                                      |_ -> raise (TypeError "Invalid type"))
  )

  | Or (exp1, exp2)  -> (let ev1 = eval_expr env exp1 in
                            let ev2 = eval_expr env exp2 in
                            match ev1 with
                            |Bool_Val x1 -> (match ev2 with
                                      |Bool_Val x2 -> if (x1 || x2) then Bool_Val true else Bool_Val false
                                      |_ -> raise (TypeError "Invalid type"))
                            |_ -> raise (TypeError "Invalid type or")
  )

  
  | And (exp1, exp2) -> (let ev1 = eval_expr env exp1 in
                            let ev2 = eval_expr env exp2 in
                            match ev1 with
                            |Bool_Val x1 -> (match ev2 with
                                      |Bool_Val x2 -> if x1 && x2 then Bool_Val true else Bool_Val false
                                      |_ -> raise (TypeError "Invalid type"))
                            |_ -> raise (TypeError "Invalid type and")
  )

  | Not exp1 -> (let ev1 = eval_expr env exp1 in
                            match ev1 with
                            |Bool_Val x1 -> if (x1 = true) then Bool_Val false else Bool_Val true
                            |_ -> raise (TypeError "Invalid type not")
  )

    

;;

let getType x =
match x with
|Int_Val i -> Int_Type
|Bool_Val b -> Bool_Type
;;





let rec eval_stmt env s =

  match s with 

  |NoOp -> (env)

  |Seq (s1, s2) -> (eval_stmt (eval_stmt env s1) s2)

  |Declare (typ, str) -> (
                       if (List.mem_assoc str env) then raise (DeclareError "Declaree Error")  else 
                        (match typ with
                        |Int_Type -> (str, (Int_Val (0)))::env                       
                        |Bool_Type -> (str, Bool_Val false)::env
                        )
                    )
  
|Assign (id, exp) ->(
                    if List.mem_assoc id env then 
                      (let eq = eval_expr env exp in
                      (match ((find id env) , eq) with
                      |(Int_Val a, Int_Val b) -> (id, eq)
                      |(Bool_Val a, Bool_Val b) -> (id, eq)
                      |_-> raise (TypeError "Invalid type ASSIGN")
                      )::(List.remove_assoc id env)
) 
else raise (DeclareError "error")
)


              

  |If (exp, s1, s2) ->  (
                        let x = eval_expr env exp in 
                        if (getType(x)) = Bool_Type then 
                        (match x with
                        |Bool_Val b -> if (b=true) then (eval_stmt env s1)
                                       else (eval_stmt env s2)
                        |_-> raise (TypeError "Invalid type for IF"))

                        else raise (TypeError "Invalid type for IF")
                        )
                       


  |While (exp, s1) -> (
                        let ev = eval_expr env exp in
                        match ev with
                        |Bool_Val b -> if b = true then let ev1 = eval_stmt env s1 in eval_stmt ev1 (While (exp, s1)) else env
                        |_-> raise (TypeError "Invalid type")
                      )

                      
  |For (id, ex1, ex2, stmt) -> ( if (List.mem_assoc id env = false) then raise (TypeError "err") else
    match (eval_expr env ex1), (eval_expr env ex2) with
      |Int_Val x1, Int_Val x2 ->   
        if x1 <= x2 then
      let elp = (str, Int_x1) in let enn = elp::env in eval_stmt enn stmt
       
  )


 
  |Print p -> (
    match eval_expr env p with
        | Int_Val i -> let _ = print_output_int i in
                        let _ = print_output_newline () in env

        |Bool_Val b -> let _ = print_output_bool b in
        let _ = print_output_newline () in env
  )
;;









// v = number of  vertices
// G is the graph
Arr = [v][v]

for i=0 to n do
  for j=0 to n do
    if (G[i, j] == 1)
      Arr[i] ← Arr[i] ∪ {Arr[j]}
    end if
  end for
end for


// v = number of  vertices
// G is the graph
Arr = [v][v]    // initialize all elements to 0

for i=0 to n do
  for j=0 to n do
    if (G[i, j] == 1)
      Arr[i][j] ← 1
    end if
  end for
end for


for i in ( V - {a,b})
    if (D[i][a] + s) < D[i][b]
      D[i][b] <-- (D[i][a] + s)
    end if
  end for

for i in V
    for j in V
      if (D[i][b] + D[b][j]) < (D[b,j])
        D[i][j] = D[i][b] + D[b][j]
      end if
    end for
end for










