open Types
include Str

let tokenize input =
  let rec helper input tokens =
    if input = "" then
      List.rev tokens  
    else
      let input = if Str.string_match (Str.regexp "^[ \n\t]+") input 0 then
                    Str.replace_matched "" input  
                  else
                    input
      in
      if Str.string_match (Str.regexp "^\\(-?[0-9]+\\)") input 0 then
        let matched = Str.matched_group 1 input in
        let n = if matched.[0] = '(' then
                    
                    int_of_string (String.sub matched 1 ((String.length matched) - 2))
                  else
                    int_of_string matched
        in
        helper (Str.string_after input (String.length matched)) (Tok_Int n :: tokens)
      else if Str.string_match (Str.regexp "^\\\"[^\"]*\\\"") input 0 then
        let matched = Str.matched_string input in
        let str_value = String.sub matched 1 ((String.length matched) - 2) in 
        helper (Str.string_after input (String.length matched)) (Tok_String str_value :: tokens)
      else if Str.string_match (Str.regexp "^[a-zA-Z][a-zA-Z0-9]*") input 0 then
        let matched = Str.matched_string input in
        helper (Str.string_after input (String.length matched)) (Tok_ID matched :: tokens)
      else if Str.string_match (Str.regexp "^(true|false)") input 0 then
        let bool = Str.matched_string input in
        let token = if bool = "true" then Tok_Bool true else Tok_Bool false in
        helper (Str.string_after input (String.length bool)) (token :: tokens)
      else match input.[0] with
        | '(' -> helper (Str.string_after input 1) (Tok_LParen :: tokens)
        | ')' -> helper (Str.string_after input 1) (Tok_RParen :: tokens)
        | '{' -> helper (Str.string_after input 1) (Tok_LCurly :: tokens)
        | '}' -> helper (Str.string_after input 1) (Tok_RCurly :: tokens)
        | '.' -> helper (Str.string_after input 1) (Tok_Dot :: tokens)
        | '+' -> helper (Str.string_after input 1) (Tok_Add :: tokens)
        | '-' -> 
            if Str.string_startswith input "->" then
              helper (Str.string_after input 2) (Tok_Arrow :: tokens)
            else
              helper (Str.string_after input 1) (Tok_Sub :: tokens)
        | '*' -> helper (Str.string_after input 1) (Tok_Mult :: tokens)
        | '/' -> helper (Str.string_after input 1) (Tok_Div :: tokens)
        | '^' -> helper (Str.string_after input 1) (Tok_Concat :: tokens)
        | ';' -> 
            if Str.string_startswith input ";;" then
              helper (Str.string_after input 2) (Tok_DoubleSemi :: tokens)
            else
              helper (Str.string_after input 1) (Tok_Semi :: tokens)
        | _ -> 
           if Str.string_match (Str.regexp "^==") input 0 then
        helper (Str.string_after input 2) (Tok_Equal :: tokens)
      else if Str.string_match (Str.regexp "^<>") input 0 then
        helper (Str.string_after input 2) (Tok_NotEqual :: tokens)
      else if Str.string_match (Str.regexp "^>=") input 0 then
        helper (Str.string_after input 2) (Tok_GreaterEqual :: tokens)
      else if Str.string_match (Str.regexp "^<=") input 0 then
        helper (Str.string_after input 2) (Tok_LessEqual :: tokens)
      else if Str.string_match (Str.regexp "^>") input 0 then
        helper (Str.string_after input 1) (Tok_Greater :: tokens)
      else if Str.string_match (Str.regexp "^<") input 0 then
        helper (Str.string_after input 1) (Tok_Less :: tokens)
      else if Str.string_startswith input "||" then
        helper (Str.string_after input 2) (Tok_Or :: tokens)
      else if Str.string_startswith input "&&" then
        helper (Str.string_after input 2) (Tok_And :: tokens)
      else if Str.string_match (Str.regexp "^not\\([ \n\t;.,(){}]\\|$\\)") input 0 then
        helper (Str.string_after input 3) (Tok_Not :: tokens)
      else if Str.string_match (Str.regexp "^if\\([ \n\t;.,(){}]\\|$\\)") input 0 then
        helper (Str.string_after input 2) (Tok_If :: tokens)
      else if Str.string_match (Str.regexp "^then\\([ \n\t;.,(){}]\\|$\\)") input 0 then
        helper (Str.string_after input 4) (Tok_Then :: tokens)
      else if Str.string_match (Str.regexp "^else\\([ \n\t;.,(){}]\\|$\\)") input 0 then
        helper (Str.string_after input 4) (Tok_Else :: tokens)
      else if Str.string_match (Str.regexp "^let\\([ \n\t;.,(){}]\\|$\\)") input 0 then
        helper (Str.string_after input 3) (Tok_Let :: tokens)
      else if Str.string_match (Str.regexp "^def\\([ \n\t;.,(){}]\\|$\\)") input 0 then
        helper (Str.string_after input 3) (Tok_Def :: tokens)
      else if Str.string_match (Str.regexp "^in\\([ \n\t;.,(){}]\\|$\\)") input 0 then
        helper (Str.string_after input 2) (Tok_In :: tokens)
      else if Str.string_match (Str.regexp "^rec\\([ \n\t;.,(){}]\\|$\\)") input 0 then
        helper (Str.string_after input 3) (Tok_Rec :: tokens)
      else if Str.string_match (Str.regexp "^fun\\([ \n\t;.,(){}]\\|$\\)") input 0 then
        helper (Str.string_after input 3) (Tok_Fun :: tokens)
      else
          failwith "Unknown token"
  in
  helper input []
