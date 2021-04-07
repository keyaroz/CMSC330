
open SmallCTypes
open Utils
open TokenTypes

(* Parsing helpers (you don't need to modify these) *)

(* Return types for parse_stmt and parse_expr *)
type stmt_result = token list * stmt
type expr_result = token list * expr

(* Return the next token in the token list, throwing an error if the list is empty *)
let lookahead (toks : token list) : token =
  match toks with
  | [] -> raise (InvalidInputException "No more tokens")
  | h::_ -> h

(* Matches the next token in the list, throwing an error if it doesn't match the given token *)
let match_token (toks : token list) (tok : token) : token list =
  match toks with
  | [] -> raise (InvalidInputException(string_of_token tok))
  | h::t when h = tok -> t
  | h::_ -> raise (InvalidInputException(
      Printf.sprintf "Expected %s from input %s, got %s"
        (string_of_token tok)
        (string_of_list string_of_token toks)
        (string_of_token h)
    ))

(* Parsing (TODO: implement your code below) *)

let rec parse_expr toks : expr_result =
  
  parse_Or toks

  and parse_Or toks =
    let (toks1, andExp) = parse_And toks in
    match lookahead toks1 with
    |Tok_Or -> let toks2 = (match_token toks1 Tok_Or) in
               let (toks3, x1) = parse_Or toks2 in 
                (toks3, Or(andExp, x1))
    |_ -> (toks1, andExp)

  (* ----------------------*)

  and parse_And toks =
  let (toks1, eqexp) = parse_Equality toks in
  match lookahead toks1 with
  |Tok_And -> let toks2 = (match_token toks1 Tok_And) in
              let (toks3, andExp) = parse_And toks2 in 
              (toks3, And(eqexp, andExp))
  |_ -> (toks1, eqexp)

  (*------------------------------*)
and parse_Equality toks = 
let (toks1, relationalExp) = parse_Relational toks in
match lookahead toks1 with
|Tok_Equal -> let toks2 = (match_token toks1 Tok_Equal) in
            let (toks3, eqexp) = parse_Equality toks2 in 
            (toks3, Equal(relationalExp, eqexp))

|Tok_NotEqual -> let toks2 = (match_token toks1 Tok_NotEqual) in
            let (toks3, eqexp) = parse_Equality toks2 in 
            (toks3, NotEqual(relationalExp, eqexp))
|_ -> (toks1, relationalExp)


(*------------------------------*)
(*     < | > | <= | >=     *)
and parse_Relational toks = 
let (toks1, addExp) = parse_Additive toks in
  match lookahead toks1 with
  |Tok_Less -> let toks2 = (match_token toks1 Tok_Less) in
            let (toks3, relationalExp) = parse_Relational toks2 in 
            (toks3, Less(addExp, relationalExp))

  |Tok_LessEqual -> let toks2 = (match_token toks1 Tok_LessEqual) in
            let (toks3, relationalExp) = parse_Relational toks2 in 
            (toks3, LessEqual(addExp, relationalExp))

  |Tok_Greater -> let toks2 = (match_token toks1 Tok_Greater) in
            let (toks3, relationalExp) = parse_Relational toks2 in 
            (toks3, Greater(addExp, relationalExp))

  |Tok_GreaterEqual -> let toks2 = (match_token toks1 Tok_GreaterEqual) in
            let (toks3, relationalExp) = parse_Relational toks2 in 
            (toks3, GreaterEqual(addExp, relationalExp))
  |_ -> (toks1, addExp)

(*------------------------------*)
and parse_Additive toks = 
  let (toks1, multExp) = parse_Multiplicative toks in
    match lookahead toks1 with
      |Tok_Add -> let toks2 = (match_token toks1 Tok_Add) in
            let (toks3, addExp) = parse_Additive toks2 in 
              (toks3, Add(multExp, addExp))

      |Tok_Sub -> let toks2 = (match_token toks1 Tok_Sub) in
            let (toks3, addExp) = parse_Additive toks2 in 
              (toks3, Sub(multExp, addExp))

      |_ -> (toks1, multExp)

(*------------------------------*)
and parse_Multiplicative toks = 
  let (toks1, powerExp) = parse_Power toks in
    match lookahead toks1 with
    |Tok_Mult -> let toks2 = (match_token toks1 Tok_Mult) in
                  let (toks3, multExp) = parse_Multiplicative toks2 in 
                    (toks3, Mult(powerExp, multExp))

    |Tok_Div -> let toks2 = (match_token toks1 Tok_Div) in
                  let (toks3, multExp) = parse_Multiplicative toks2 in 
                    (toks3, Div(powerExp, multExp))

    |_ -> (toks1, powerExp)

(*------------------------------*)
and parse_Power toks = 
  let (toks1, unaryExp) = parse_Unary toks in
    match (lookahead toks1) with
    |Tok_Pow -> let toks2 = (match_token toks1 Tok_Pow) in
                let (toks3, powerExp) = parse_Power toks2 in 
                  (toks3, Pow(unaryExp, powerExp))
    |_ -> (toks1, unaryExp)

(*------------------------------*)
and parse_Unary toks = 
  
    match (lookahead toks) with
    |Tok_Not -> let toks1 = (match_token toks Tok_Not) in
                let (toks2, ex) = parse_Primary toks1 in 
                  (toks2, Not(ex))
    |_ -> parse_Primary toks

(*------------------------------*)
and parse_Primary toks = 
    match lookahead toks with


    |Tok_Int i -> (let toks1 = (match_token toks (Tok_Int i)) in
                  (toks1, Int(i)))

    |Tok_Bool b -> (let toks1 = (match_token toks (Tok_Bool b)) in           
                  (toks1, (Bool(b))))


    |Tok_ID id -> (let toks1 = (match_token toks (Tok_ID id)) in           
                  (toks1, (ID(id))))

    |Tok_LParen -> (let toks1 = (match_token toks Tok_LParen) in
                    let (toks2, ex) = parse_expr toks1 in 
                    let toks3 = (match_token toks2 Tok_RParen) in
                    (toks3, ex))
    
    |_ -> raise (InvalidInputException "parse_expr failed")
;;
  
let id_helper tok = 
  match tok with
  |(Tok_ID id)::t -> (t , id)
  |_ -> raise (InvalidInputException "id fail")
;;


(*--------------------------------*)
let rec parse_stmt toks : stmt_result =
  match lookahead toks with

  |EOF

  (*Declare*)
  |Tok_Int_Type -> (
    let toks1 = (match_token toks Tok_Int_Type) in
      match toks1 with
      |(Tok_ID id)::t -> let toks2 = (match_token toks1 (Tok_ID id)) in
        let toks3 = match_token toks2 Tok_Semi in
        let (lst, stmt) = parse_stmt toks3 in (lst, Seq(Declare(Int_Type, id), stmt))
      |_ -> raise (InvalidInputException "Invalid Int_Type")
      )

  |Tok_Bool_Type -> (
    let toks1 = (match_token toks Tok_Bool_Type) in
      match toks1 with
      |(Tok_ID id)::t -> let toks2 = (match_token toks1 (Tok_ID id)) in
        let toks3 = match_token toks2 Tok_Semi in
        let (lst, stmt) = parse_stmt toks3 in (lst, Seq(Declare(Bool_Type, id), stmt))
      |_ -> raise (InvalidInputException "Invalid Bool_Type")
      )



  |Tok_ID id -> (
                 let t4 = (match_token toks (Tok_ID id)) in
                let t5 = (match_token t4 Tok_Assign) in
                let (lst, ex) = parse_expr t5 in
                let t6 = (match_token lst Tok_Semi) in
                let (lst2, stmt2) = parse_stmt t6 in
                (lst2, Seq(Assign(id, ex), stmt2)) 
                (* raise (InvalidInputException "Invalid iddd") *)

                )

  

  |Tok_If ->   ( let t4 = (match_token toks Tok_If) in
                let t5 = (match_token t4 Tok_LParen) in
                let (lst, ex) = parse_expr t5 in
                let t6 = (match_token lst Tok_RParen) in
                let t7 = (match_token t6 Tok_LBrace) in
                let (lst2, stmt2) = parse_stmt t7 in
                let toks8 = match_token lst2 Tok_RBrace in
                match toks8 with 
                |Tok_Else::t -> (let tk1 = (match_token t Tok_LBrace) in
                              let (lst3, stmt3) = parse_stmt tk1 in
                              let tk2 = (match_token lst3 Tok_RBrace) in
                              let (lst4, stmt4) = parse_stmt tk2 in
                              (lst4, Seq(If (ex, stmt2, stmt3), stmt4))
                )
                |_ -> let (lst5, stmt5) = parse_stmt toks8 in (lst5, Seq(If (ex, stmt2, NoOp), stmt5))
  )   


  |Tok_Print -> (
                let t4 = (match_token toks Tok_Print) in
                let t5 = (match_token t4 Tok_LParen) in
                let (lst, stmt) = parse_expr t5 in
                let t6 = (match_token lst Tok_RParen) in
                let t7 = (match_token t6 Tok_Semi) in
                let (lst2, stmt2) = parse_stmt t7 in
                (lst2, Seq(Print(stmt), stmt2))
  )

    |Tok_For -> (
                let t4 = (match_token toks Tok_For) in   
                let t5 = (match_token t4 Tok_LParen) in
                let (t6, id) = id_helper t5 in  
                let t7 = (match_token t6 Tok_From) in
                let (lst, stmt) = parse_expr t7 in
                let toks8 = (match_token lst Tok_To) in
                let (lst2, stmt2) = parse_expr toks8 in
                let toks9 = (match_token lst2 Tok_RParen) in
                let tk1 = (match_token toks9 Tok_LBrace) in
                let (lst3, stmt3) = parse_stmt tk1 in
                let tk2 = (match_token lst3 Tok_RBrace) in
                let (lst4, stmt4) = parse_stmt tk2 in
                (lst4, Seq (For (id, stmt, stmt2, stmt3), stmt4))
                )

    |Tok_While -> (
                let t4 = (match_token toks Tok_While) in
                let t5 = (match_token t4 Tok_LParen) in
                let (lst, stmt) = parse_expr t5 in
                let t6 = (match_token lst Tok_RParen) in
                let t7 = (match_token t6 Tok_LBrace) in
               
                let (lst2, stmt2) = parse_stmt t7 in
                let toks8 = (match_token lst2 Tok_RBrace) in
                let (lst3, stmt3) = parse_stmt toks8 in
                (lst3, Seq (While (stmt, stmt2), stmt3))
                )

      |Tok_RBrace -> (toks, NoOp)

      

      |_-> raise (InvalidInputException "parse stmt fail") 
;;


let parse_main toks : stmt =
 match toks with
  |Tok_Int_Type::Tok_Main::Tok_LParen::Tok_RParen::Tok_LBrace::t ->
    (
      let (lst, stmt) = (parse_stmt t) in match lst with
                            |Tok_RBrace::[EOF] -> stmt
                            |_-> raise (InvalidInputException "parse_main fail")


    )
  |_-> raise (InvalidInputException "parse_main fail")
  
 ;; 

 

