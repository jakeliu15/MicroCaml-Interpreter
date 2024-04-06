open Types
open Str

exception InvalidInputException

let tokenize input =
  let rec helper input tokens =
  
    let ignore_spaces input =
      if string_match (regexp "^[ \n\t]+") input 0 then
        string_after input (match_end ())
      else
        input
    in
    if input = "" || string_match (regexp "^[ \n\t]+$") input 0 then
      List.rev tokens
    else
      let input = ignore_spaces input in  

      if string_match (regexp "^\\(-?[0-9]+\\)") input 0 then
        let matched = matched_group 1 input in
        let n = int_of_string matched in
        helper (ignore_spaces (string_after input (String.length matched))) (Tok_Int n :: tokens)
      else if string_match (regexp "^\\((-?[0-9]+)\\)") input 0 then
        let matched = matched_group 1 input in
        let n = 
          if String.length matched > 0 && matched.[0] = '(' then
            int_of_string (String.sub matched 1 (String.length matched - 2))
          else
            int_of_string matched
        in
        helper (ignore_spaces (string_after input (String.length matched))) (Tok_Int n :: tokens)
      else if string_match (regexp "^\\\"[^\"]*\\\"") input 0 then
        let matched = matched_string input in
        let str_value = String.sub matched 1 ((String.length matched) - 2) in
        helper (ignore_spaces (string_after input (String.length matched))) (Tok_String str_value :: tokens)
      else if string_match (regexp "^true") input 0 then
        let bool = matched_string input in
        let token = Tok_Bool true in
        helper (ignore_spaces (string_after input (String.length bool))) (token :: tokens)
      else if string_match (regexp "^false") input 0 then
        let bool = matched_string input in
        let token = Tok_Bool false in
        helper (ignore_spaces (string_after input (String.length bool))) (token :: tokens)
      else if string_match (regexp "^not\\b") input 0 then
        helper (ignore_spaces (string_after input 3)) (Tok_Not :: tokens)
      else if string_match (regexp "^if\\b") input 0 then
        helper (ignore_spaces (string_after input 2)) (Tok_If :: tokens)
      else if string_match (regexp "^then\\b") input 0 then
        helper (ignore_spaces (string_after input 4)) (Tok_Then :: tokens)
      else if string_match (regexp "^else\\b") input 0 then
        helper (ignore_spaces (string_after input 4)) (Tok_Else :: tokens)
      else if string_match (regexp "^let\\b") input 0 then
        helper (ignore_spaces (string_after input 3)) (Tok_Let :: tokens)
      else if string_match (regexp "^def\\b") input 0 then
        helper (ignore_spaces (string_after input 3)) (Tok_Def :: tokens)
      else if string_match (regexp "^in\\b") input 0 then
        helper (ignore_spaces (string_after input 2)) (Tok_In :: tokens)
      else if string_match (regexp "^rec\\b") input 0 then
        helper (ignore_spaces (string_after input 3)) (Tok_Rec :: tokens)
      else if string_match (regexp "^fun\\b") input 0 then
        helper (ignore_spaces (string_after input 3)) (Tok_Fun :: tokens)
      else if string_match (regexp "^[a-zA-Z][a-zA-Z0-9]*") input 0 then
        let matched = matched_string input in
        helper (ignore_spaces (string_after input (String.length matched))) (Tok_ID matched :: tokens)
      
      else match input.[0] with
        | '(' -> helper (ignore_spaces (string_after input 1)) (Tok_LParen :: tokens)
        | ')' -> helper (ignore_spaces (string_after input 1)) (Tok_RParen :: tokens)
        | '{' -> helper (ignore_spaces (string_after input 1)) (Tok_LCurly :: tokens)
        | '}' -> helper (ignore_spaces (string_after input 1)) (Tok_RCurly :: tokens)
        | '.' -> helper (ignore_spaces (string_after input 1)) (Tok_Dot :: tokens)
        | '+' -> helper (ignore_spaces (string_after input 1)) (Tok_Add :: tokens)
        | '-' ->
            if Str.string_match (Str.regexp "^->") input 0 then
              helper (ignore_spaces (string_after input 2)) (Tok_Arrow :: tokens)
            else
              helper (ignore_spaces (string_after input 1)) (Tok_Sub :: tokens)
        | '*' -> helper (ignore_spaces (string_after input 1)) (Tok_Mult :: tokens)
        | '/' -> helper (ignore_spaces (string_after input 1)) (Tok_Div :: tokens)
        | '^' -> helper (ignore_spaces (string_after input 1)) (Tok_Concat :: tokens)
        | ';' ->
            if Str.string_match (Str.regexp "^;;") input 0 then
              helper (ignore_spaces (string_after input 2)) (Tok_DoubleSemi :: tokens)
            else
              helper (ignore_spaces (string_after input 1)) (Tok_Semi :: tokens)
        | '=' ->
            if Str.string_match (Str.regexp "^=") input 0 then
              helper (ignore_spaces (string_after input 2)) (Tok_Equal :: tokens)
            else
              raise InvalidInputException
        | '<' ->
            if Str.string_match (Str.regexp "^<=") input 0 then
              helper (ignore_spaces (string_after input 2)) (Tok_LessEqual :: tokens)
            else if Str.string_match (Str.regexp "^<>") input 0 then
              helper (ignore_spaces (string_after input 2)) (Tok_NotEqual :: tokens)
            else
              helper (ignore_spaces (string_after input 1)) (Tok_Less :: tokens)
        | '>' ->
            if Str.string_match (Str.regexp "^>=") input 0 then
              helper (ignore_spaces (string_after input 2)) (Tok_GreaterEqual :: tokens)
            else
              helper (ignore_spaces (string_after input 1)) (Tok_Greater :: tokens)
        | '&' ->
            if Str.string_match (Str.regexp "^&&") input 0 then
              helper (ignore_spaces (string_after input 2)) (Tok_And :: tokens)
            else
              raise InvalidInputException
        | '|' ->
            if Str.string_match (Str.regexp "^||") input 0 then
              helper (ignore_spaces (string_after input 2)) (Tok_Or :: tokens)
            else
              raise InvalidInputException
        | _ -> raise InvalidInputException
  in
  helper input []