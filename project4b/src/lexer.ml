open TokenTypes

let tokenize input =
  
  let rec helper pos remain =
    if (pos >= String.length remain) then [EOF]


      else if (Str.string_match (Str.regexp "for") remain pos) then 
      Tok_For::(helper (pos+(String.length (Str.matched_string remain))) remain)
      
      else if (Str.string_match (Str.regexp "from") remain pos) then 
      Tok_From::(helper (pos+(String.length (Str.matched_string remain))) remain)

      else if (Str.string_match (Str.regexp "to") remain pos) then 
      Tok_To::(helper (pos+(String.length (Str.matched_string remain))) remain)

      else if (Str.string_match (Str.regexp "while") remain pos) then 
      Tok_While::(helper (pos+(String.length (Str.matched_string remain))) remain)

      else if (Str.string_match (Str.regexp "int") remain pos) then 
      Tok_Int_Type::(helper (pos+(String.length (Str.matched_string remain))) remain)
  
      else if (Str.string_match (Str.regexp "bool") remain pos) then 
      Tok_Bool_Type::(helper (pos+(String.length (Str.matched_string remain))) remain)

      else if (Str.string_match (Str.regexp "true") remain pos) then  
      Tok_Bool (true)::(helper (pos+(String.length (Str.matched_string remain))+1) remain)

      else if (Str.string_match (Str.regexp "false") remain pos) then 
      Tok_Bool (false)::(helper (pos+(String.length (Str.matched_string remain))+1) remain)

      else if (Str.string_match (Str.regexp "\\-") remain pos) then 
      Tok_Sub::(helper (pos+1) remain)

      else if (Str.string_match (Str.regexp ";") remain pos) then 
      Tok_Semi::(helper (pos+1) remain)

      else if (Str.string_match (Str.regexp ")") remain pos) then 
      Tok_RParen::(helper (pos+1) remain)

      else if (Str.string_match (Str.regexp "}") remain pos) then 
      Tok_RBrace::(helper (pos+1) remain)

      
      else if (Str.string_match (Str.regexp "printf") remain pos) then 
      Tok_Print::(helper (pos+(String.length (Str.matched_string remain))) remain)

      
      else if (Str.string_match (Str.regexp "\\^") remain pos) then 
      Tok_Pow::(helper (pos+1) remain)


      else if (Str.string_match (Str.regexp "\\+") remain pos) then 
      Tok_Add::(helper (pos+1) remain)

      else if (Str.string_match (Str.regexp "||") remain pos) then 
      Tok_Or::(helper (pos+1) remain)

      else if (Str.string_match (Str.regexp "!=") remain pos) then 
      Tok_NotEqual::(helper (pos+2) remain)

      else if (Str.string_match (Str.regexp "!") remain pos) then 
      Tok_Not::(helper (pos+1) remain)

      else if (Str.string_match (Str.regexp "\\*") remain pos) then 
      Tok_Mult::(helper (pos+1) remain)

      
      else if (Str.string_match (Str.regexp "main") remain pos) then 
      Tok_Main::(helper (pos+(String.length (Str.matched_string remain))) remain)

      

      


      else if (Str.string_match (Str.regexp ">") remain pos) then 
      Tok_Greater::(helper (pos+1) remain)

      else if (Str.string_match (Str.regexp "==") remain pos) then 
      Tok_Equal::(helper (pos+2) remain)

      

      else if (Str.string_match (Str.regexp ">=") remain pos) then 
      Tok_GreaterEqual::(helper (pos+1) remain)

      else if (Str.string_match (Str.regexp "<=") remain pos) then 
      Tok_LessEqual::(helper (pos+1) remain)

      else if (Str.string_match (Str.regexp "<") remain pos) then 
      Tok_Less::(helper (pos+1) remain)

      else if (Str.string_match (Str.regexp "(") remain pos) then 
      Tok_LParen::(helper (pos+1) remain)

      else if (Str.string_match (Str.regexp "{") remain pos) then 
      Tok_LBrace::(helper (pos+1) remain)


      else if (Str.string_match (Str.regexp "=") remain pos) then 
      Tok_Assign::(helper (pos+1) remain)

      else if (Str.string_match (Str.regexp "&&") remain pos) then 
      Tok_And::(helper (pos+2) remain)

      else if (Str.string_match (Str.regexp "-?[0-9]+" ) remain pos) then 
      let matched_int  = Str.matched_string remain in
      Tok_Int (int_of_string matched_int)::(helper (pos + (String.length matched_int)) remain )

      else if (Str.string_match (Str.regexp "if") remain pos) then 
      Tok_If::(helper (pos+(String.length (Str.matched_string remain))) remain)

      else if (Str.string_match (Str.regexp "else") remain pos) then 
      Tok_Else::(helper (pos+(String.length (Str.matched_string remain))) remain)

      else if (Str.string_match (Str.regexp "/") remain pos) then 
      Tok_Div::(helper (pos+1) remain)

      else if (Str.string_match (Str.regexp "[a-zA-Z][a-zA-Z0-9]*") remain pos) then 
      let matchy = Str.matched_string remain in  
      Tok_ID (matchy)::(helper (pos + (String.length (Str.matched_string remain))) remain)

      else helper (pos+1) remain

      in helper 0 input
;;
