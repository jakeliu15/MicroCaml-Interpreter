open Types
open Str


let tokenize pos =
  let rec helper pos tokens =
  
    let ignore_spaces pos =
      if string_match (regexp "^[ \n\t]+") pos 0 then
        string_after pos (match_end ())
      else
        pos
    in
    if pos = "" || string_match (regexp "^[ \n\t]+$") pos 0 then
      List.rev tokens
    else
      let pos = ignore_spaces pos in  

      if string_match (regexp "^\\([0-9]+-\\)") pos 0 then
        let matched = String.sub (matched_group 1 pos) 0 1 in
        let n = int_of_string matched in
        helper (ignore_spaces (string_after pos (String.length matched + 1))) (Tok_Sub :: Tok_Int n :: tokens) 

     

      else if string_match (regexp "^\\(-?[0-9]+\\)") pos 0 then
        let matched = matched_group 1 pos in
        let n = int_of_string matched in
        helper (ignore_spaces (string_after pos (String.length matched))) (Tok_Int n :: tokens)
      else if string_match (regexp "^\\((-?[0-9]+)\\)") pos 0 then
        let matched = matched_group 1 pos in
        let n = 
          if String.length matched > 0 && matched.[0] = '(' then
            int_of_string (String.sub matched 1 (String.length matched - 2))
          else
            int_of_string matched
        in
        helper (ignore_spaces (string_after pos (String.length matched))) (Tok_Int n :: tokens)
      else if string_match (regexp "^\\\"[^\"]*\\\"") pos 0 then
        let matched = matched_string pos in
        let str_value = String.sub matched 1 ((String.length matched) - 2) in
        helper (ignore_spaces (string_after pos (String.length matched))) (Tok_String str_value :: tokens)
      else if string_match (regexp "^true") pos 0 then
        let bool = matched_string pos in
        let token = Tok_Bool true in
        helper (ignore_spaces (string_after pos (String.length bool))) (token :: tokens)
      else if string_match (regexp "^false") pos 0 then
        let bool = matched_string pos in
        let token = Tok_Bool false in
        helper (ignore_spaces (string_after pos (String.length bool))) (token :: tokens)
      else if string_match (regexp "^not\\b") pos 0 then
        helper (ignore_spaces (string_after pos 3)) (Tok_Not :: tokens)
      else if string_match (regexp "^if\\b") pos 0 then
        helper (ignore_spaces (string_after pos 2)) (Tok_If :: tokens)
      else if string_match (regexp "^then\\b") pos 0 then
        helper (ignore_spaces (string_after pos 4)) (Tok_Then :: tokens)
      else if string_match (regexp "^else\\b") pos 0 then
        helper (ignore_spaces (string_after pos 4)) (Tok_Else :: tokens)
      else if string_match (regexp "^let\\b") pos 0 then
        helper (ignore_spaces (string_after pos 3)) (Tok_Let :: tokens)
      else if string_match (regexp "^def\\b") pos 0 then
        helper (ignore_spaces (string_after pos 3)) (Tok_Def :: tokens)
      else if string_match (regexp "^in\\b") pos 0 then
        helper (ignore_spaces (string_after pos 2)) (Tok_In :: tokens)
      else if string_match (regexp "^rec\\b") pos 0 then
        helper (ignore_spaces (string_after pos 3)) (Tok_Rec :: tokens)
      else if string_match (regexp "^fun\\b") pos 0 then
        helper (ignore_spaces (string_after pos 3)) (Tok_Fun :: tokens)
      else if string_match (regexp "^[a-zA-Z][a-zA-Z0-9]*") pos 0 then
        let matched = matched_string pos in
        helper (ignore_spaces (string_after pos (String.length matched))) (Tok_ID matched :: tokens)
      
      else match pos.[0] with
        | '(' -> helper (ignore_spaces (string_after pos 1)) (Tok_LParen :: tokens)
        | ')' -> helper (ignore_spaces (string_after pos 1)) (Tok_RParen :: tokens)
        | '{' -> helper (ignore_spaces (string_after pos 1)) (Tok_LCurly :: tokens)
        | '}' -> helper (ignore_spaces (string_after pos 1)) (Tok_RCurly :: tokens)
        | '.' -> helper (ignore_spaces (string_after pos 1)) (Tok_Dot :: tokens)
        | '+' -> helper (ignore_spaces (string_after pos 1)) (Tok_Add :: tokens)
        | '-' ->
            if Str.string_match (Str.regexp "^->") pos 0 then
              helper (ignore_spaces (string_after pos 2)) (Tok_Arrow :: tokens)
            else
              helper (ignore_spaces (string_after pos 1)) (Tok_Sub :: tokens)
        | '*' -> helper (ignore_spaces (string_after pos 1)) (Tok_Mult :: tokens)
        | '/' -> helper (ignore_spaces (string_after pos 1)) (Tok_Div :: tokens)
        | '^' -> helper (ignore_spaces (string_after pos 1)) (Tok_Concat :: tokens)
        | ';' ->
            if Str.string_match (Str.regexp "^;;") pos 0 then
              helper (ignore_spaces (string_after pos 2)) (Tok_DoubleSemi :: tokens)
            else
              helper (ignore_spaces (string_after pos 1)) (Tok_Semi :: tokens)
        | '=' ->
            if Str.string_match (Str.regexp "^=") pos 0 then
              helper (ignore_spaces (string_after pos 1)) (Tok_Equal :: tokens)
            else
              raise (InvalidInputException("Invalid Input"))
        | '<' ->
            if Str.string_match (Str.regexp "^<=") pos 0 then
              helper (ignore_spaces (string_after pos 2)) (Tok_LessEqual :: tokens)
            else if Str.string_match (Str.regexp "^<>") pos 0 then
              helper (ignore_spaces (string_after pos 2)) (Tok_NotEqual :: tokens)
            else
              helper (ignore_spaces (string_after pos 1)) (Tok_Less :: tokens)
        | '>' ->
            if Str.string_match (Str.regexp "^>=") pos 0 then
              helper (ignore_spaces (string_after pos 2)) (Tok_GreaterEqual :: tokens)
            else
              helper (ignore_spaces (string_after pos 1)) (Tok_Greater :: tokens)
        | '&' ->
            if Str.string_match (Str.regexp "^&&") pos 0 then
              helper (ignore_spaces (string_after pos 2)) (Tok_And :: tokens)
            else
              raise (InvalidInputException ("1"))
        | '|' ->
            if Str.string_match (Str.regexp "^||") pos 0 then
              helper (ignore_spaces (string_after pos 2)) (Tok_Or :: tokens)
            else
              raise (InvalidInputException ("2"))
        | _ -> raise (InvalidInputException ("3"))
  in
  helper pos []