open Types
open Str

let tokenize input =
  let rec helper input tokens =
    let string_startswith s prefix =
      let len = String.length prefix in
      if String.length s < len then false
      else String.sub s 0 len = prefix
    in
    
    if input = "" then
      List.rev tokens  
    else
      let input = if string_match (regexp "^[ \n\t]+") input 0 then
                    replace_matched "" input  
                  else
                    input
      in
      if string_match (regexp "^\\(-?[0-9]+\\)") input 0 then
        let matched = matched_group 1 input in
        let n = if matched.[0] = '(' then int_of_string (String.sub matched 1 ((String.length matched) - 2)) else int_of_string matched
        in
        helper (string_after input (String.length matched)) (Tok_Int n :: tokens)
      else if string_match (regexp "^\\\"[^\"]*\\\"") input 0 then
        let matched = matched_string input in
        let str_value = String.sub matched 1 ((String.length matched) - 2) in 
        helper (string_after input (String.length matched)) (Tok_String str_value :: tokens)
      else if string_match (regexp "^[a-zA-Z][a-zA-Z0-9]*") input 0 then
        let matched = matched_string input in
        helper (string_after input (String.length matched)) (Tok_ID matched :: tokens)
      else if string_match (regexp "^(true|false)") input 0 then
        let bool = matched_string input in
        let token = if bool = "true" then Tok_Bool true else Tok_Bool false in
        helper (string_after input (String.length bool)) (token :: tokens)
      else match input.[0] with
        | '(' -> helper (string_after input 1) (Tok_LParen :: tokens)
        | ')' -> helper (string_after input 1) (Tok_RParen :: tokens)
        | '{' -> helper (string_after input 1) (Tok_LCurly :: tokens)
        | '}' -> helper (string_after input 1) (Tok_RCurly :: tokens)
        | '.' -> helper (string_after input 1) (Tok_Dot :: tokens)
        | '+' -> helper (string_after input 1) (Tok_Add :: tokens)
        | '-' -> 
            if string_startswith input "->" then
              helper (string_after input 2) (Tok_Arrow :: tokens)
            else
              helper (string_after input 1) (Tok_Sub :: tokens)
        | '*' -> helper (string_after input 1) (Tok_Mult :: tokens)
        | '/' -> helper (string_after input 1) (Tok_Div :: tokens)
        | '^' -> helper (string_after input 1) (Tok_Concat :: tokens)
        | ';' -> 
            if string_startswith input ";;" then
              helper (string_after input 2) (Tok_DoubleSemi :: tokens)
            else
              helper (string_after input 1) (Tok_Semi :: tokens)
        | _ -> 
           if string_match (regexp "^==") input 0 then
        helper (string_after input 2) (Tok_Equal :: tokens)
      else if string_match (regexp "^<>") input 0 then
        helper (string_after input 2) (Tok_NotEqual :: tokens)
      else if string_match (regexp "^>=") input 0 then
        helper (string_after input 2) (Tok_GreaterEqual :: tokens)
      else if string_match (regexp "^<=") input 0 then
        helper (string_after input 2) (Tok_LessEqual :: tokens)
      else if string_match (regexp "^>") input 0 then
        helper (string_after input 1) (Tok_Greater :: tokens)
      else if string_match (regexp "^<") input 0 then
        helper (string_after input 1) (Tok_Less :: tokens)
      else if string_startswith input "||" then
        helper (string_after input 2) (Tok_Or :: tokens)
      else if string_startswith input "&&" then
        helper (string_after input 2) (Tok_And :: tokens)
      else if string_match (regexp "^not\\([ \n\t;.,(){}]\\|$\\)") input 0 then
        helper (string_after input 3) (Tok_Not :: tokens)
      else if string_match (regexp "^if\\([ \n\t;.,(){}]\\|$\\)") input 0 then
        helper (string_after input 2) (Tok_If :: tokens)
      else if string_match (regexp "^then\\([ \n\t;.,(){}]\\|$\\)") input 0 then
        helper (string_after input 4) (Tok_Then :: tokens)
      else if string_match (regexp "^else\\([ \n\t;.,(){}]\\|$\\)") input 0 then
        helper (string_after input 4) (Tok_Else :: tokens)
      else if string_match (regexp "^let\\([ \n\t;.,(){}]\\|$\\)") input 0 then
        helper (string_after input 3) (Tok_Let :: tokens)
      else if string_match (regexp "^def\\([ \n\t;.,(){}]\\|$\\)") input 0 then
        helper (string_after input 3) (Tok_Def :: tokens)
      else if string_match (regexp "^in\\([ \n\t;.,(){}]\\|$\\)") input 0 then
        helper (string_after input 2) (Tok_In :: tokens)
      else if string_match (regexp "^rec\\([ \n\t;.,(){}]\\|$\\)") input 0 then
        helper (string_after input 3) (Tok_Rec :: tokens)
      else if string_match (regexp "^fun\\([ \n\t;.,(){}]\\|$\\)") input 0 then
        helper (string_after input 3) (Tok_Fun :: tokens)
      else
          failwith "Unknown token"
  in
  helper input []
