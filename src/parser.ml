open Types
open Utils

(* Provided functions - DO NOT MODIFY *)

(* Matches the next token in the list, throwing an error if it doesn't match the given token *)
let match_token (toks : token list) (tok : token) =
  match toks with
  | [] -> raise (InvalidInputException (string_of_token tok))
  | h :: t when h = tok -> t
  | h :: _ ->
      raise
        (InvalidInputException
           (Printf.sprintf "Expected %s from input %s, got %s"
              (string_of_token tok)
              (string_of_list string_of_token toks)
              (string_of_token h)))

(* Matches a sequence of tokens given as the second list in the order in which they appear, throwing an error if they don't match *)
let match_many (toks : token list) (to_match : token list) =
  List.fold_left match_token toks to_match

(* Return the next token in the token list as an option *)
let lookahead (toks : token list) =
  match toks with [] -> None | h :: t -> Some h

(* Return the token at the nth index in the token list as an option*)
let rec lookahead_many (toks : token list) (n : int) =
  match (toks, n) with
  | h :: _, 0 -> Some h
  | _ :: t, n when n > 0 -> lookahead_many t (n - 1)
  | _ -> None

(* Part 2: Parsing expressions *)


let rec parse_expr toks =
  match lookahead toks with
  | None -> raise (InvalidInputException ("parse_expr"))
  | Some tok ->
    match tok with
    | Tok_Let ->
      let toks = match_token toks Tok_Let in
      let toks, recursive = parse_recursion toks in
      let toks, var = parse_id toks in
      let toks = match_token toks Tok_Equal in
      let toks, expr1 = parse_expr toks in
      (match lookahead toks with
       | Some Tok_In ->
         let toks = match_token toks Tok_In in
         let toks, expr2 = parse_expr toks in
         toks, Let (var, recursive, expr1, expr2)
       | _ -> toks, Let (var, recursive, expr1, ID var))
    | Tok_If ->
      let toks = match_token toks Tok_If in
      let toks, cond = parse_expr toks in
      let toks = match_token toks Tok_Then in
      let toks, expr1 = parse_expr toks in
      let toks = match_token toks Tok_Else in
      let toks, expr2 = parse_expr toks in
      toks, If (cond, expr1, expr2)
    | Tok_Fun ->
      let toks = match_token toks Tok_Fun in
      let toks, var = parse_id toks in
      let toks = match_token toks Tok_Arrow in
      let toks, expr = parse_expr toks in
      toks, Fun (var, expr)
    | _ -> parse_or toks

and parse_recursion toks =
  match lookahead toks with
  | Some Tok_Rec ->
    let toks = match_token toks Tok_Rec in
    toks, true
  | _ -> toks, false

and parse_or toks =
  let toks, expr = parse_and toks in
  match lookahead toks with
  | Some Tok_Or ->
    let toks = match_token toks Tok_Or in
    let toks, expr2 = parse_or toks in
    toks, Binop (Or, expr, expr2)
  | _ -> toks, expr

and parse_and toks =
  let toks, expr = parse_equality toks in
  match lookahead toks with
  | Some Tok_And ->
    let toks = match_token toks Tok_And in
    let toks, expr2 = parse_and toks in
    toks, Binop (And, expr, expr2)
  | _ -> toks, expr

and parse_equality toks =
  let toks, expr = parse_relational toks in
  match lookahead toks with
  | Some Tok_Equal ->
    let toks = match_token toks Tok_Equal in
    let toks, expr2 = parse_equality toks in
    toks, Binop (Equal, expr, expr2)
  | Some Tok_NotEqual ->
    let toks = match_token toks Tok_NotEqual in
    let toks, expr2 = parse_equality toks in
    toks, Binop (NotEqual, expr, expr2)
  | _ -> toks, expr

and parse_relational toks =
  let toks, expr = parse_additive toks in
  match lookahead toks with
  | Some Tok_Less ->
    let toks = match_token toks Tok_Less in
    let toks, expr2 = parse_additive toks in
    toks, Binop (Less, expr, expr2)
  | Some Tok_Greater ->
    let toks = match_token toks Tok_Greater in
    let toks, expr2 = parse_additive toks in
    toks, Binop (Greater, expr, expr2)
  | Some Tok_LessEqual ->
    let toks = match_token toks Tok_LessEqual in
    let toks, expr2 = parse_additive toks in
    toks, Binop (LessEqual, expr, expr2)
  | Some Tok_GreaterEqual ->
    let toks = match_token toks Tok_GreaterEqual in
    let toks, expr2 = parse_additive toks in
    toks, Binop (GreaterEqual, expr, expr2)
  | _ -> toks, expr

and parse_additive toks =
  let toks, expr = parse_multiplicative toks in
  match lookahead toks with
  | Some Tok_Add ->
    let toks = match_token toks Tok_Add in
    let toks, expr2 = parse_additive toks in
    toks, Binop (Add, expr, expr2)
  | Some Tok_Sub ->
    let toks = match_token toks Tok_Sub in
    let toks, expr2 = parse_additive toks in
    toks, Binop (Sub, expr, expr2)
  | _ -> toks, expr

and parse_multiplicative toks =
  let toks, expr = parse_concat toks in
  match lookahead toks with
  | Some Tok_Mult ->
    let toks = match_token toks Tok_Mult in
    let toks, expr2 = parse_multiplicative toks in
    toks, Binop (Mult, expr, expr2)
  | Some Tok_Div ->
    let toks = match_token toks Tok_Div in
    let toks, expr2 = parse_multiplicative toks in
    toks, Binop (Div, expr, expr2)
  | _ -> toks, expr

and parse_concat toks =
  let toks, expr = parse_unary toks in
  match lookahead toks with
  | Some Tok_Concat ->
    let toks = match_token toks Tok_Concat in
    let toks, expr2 = parse_concat toks in
    toks, Binop (Concat, expr, expr2)
  | _ -> toks, expr

and parse_unary toks =
  match lookahead toks with
  | Some Tok_Not ->
    let toks = match_token toks Tok_Not in
    let toks, expr = parse_unary toks in
    toks, Not expr
  | _ -> parse_app toks


  and parse_app toks =
    let toks, func = parse_primary toks in  
    match lookahead toks with
    | Some Tok_ID _ | Some Tok_Int _ | Some Tok_Bool _ | Some Tok_String _ | Some Tok_LParen ->
     
      let toks, arg = parse_primary toks in
      toks, App (func, arg)
    | _ -> toks, func
  


and parse_primary toks =
  let toks, primary_expr = parse_partial_primary toks in
  match lookahead toks with
  | Some Tok_Dot ->
    let toks = match_token toks Tok_Dot in
    let toks, lab = parse_id toks in
    toks, Select (Lab lab, primary_expr)
  | _ -> toks, primary_expr


and parse_partial_primary toks =
  match lookahead toks with
  | Some Tok_Int i ->
    let toks = match_token toks (Tok_Int i) in
    toks, Int i
  | Some Tok_Bool b ->
    let toks = match_token toks (Tok_Bool b) in
    toks, Bool b
  | Some Tok_String s ->
    let toks = match_token toks (Tok_String s) in
    toks, String s
  | Some Tok_ID id ->
    let toks = match_token toks (Tok_ID id) in
    toks, ID id
  | Some Tok_LParen ->
    let toks = match_token toks Tok_LParen in
    let toks, expr = parse_expr toks in
    let toks = match_token toks Tok_RParen in
    toks, expr
  | Some Tok_LCurly ->
    parse_record toks
  | _ -> raise(InvalidInputException ("partial"))



and parse_record toks =
  let toks = match_token toks Tok_LCurly in
  match lookahead toks with
  | Some Tok_RCurly -> 
    let toks = match_token toks Tok_RCurly in
    toks, Record []
  | _ ->
    let toks, body = parse_record_body toks in
    let toks = match_token toks Tok_RCurly in
    toks, Record body

and parse_record_body toks =
  let toks, lab = parse_id toks in
  let toks = match_token toks Tok_Equal in
  let toks, expr = parse_expr toks in
  match lookahead toks with
  | Some Tok_Semi ->
    let toks = match_token toks Tok_Semi in
    let toks, body = parse_record_body toks in
    toks, (Lab lab, expr) :: body
  | _ -> toks, [(Lab lab, expr)]

and parse_id toks =
  match lookahead toks with
  | Some (Tok_ID id) ->
    let toks = match_token toks (Tok_ID id) in
    toks, id
  | _ -> raise (InvalidInputException ("parse_id"))

let rec parse_mutop toks =
  match lookahead toks with
  | None -> raise (InvalidInputException ("parse_mutop"))
  | Some Tok_Def -> parse_def_mutop toks
  | Some Tok_DoubleSemi -> match_token toks Tok_DoubleSemi, NoOp
  | _ -> parse_expr_mutop toks

and parse_def_mutop toks =
  let toks = match_token toks Tok_Def in
  let toks, var = parse_id toks in
  let toks = match_token toks Tok_Equal in
  let toks, expr = parse_expr toks in
  let toks = match_token toks Tok_DoubleSemi in
  toks, Def (var, expr)

and parse_expr_mutop toks =
  let toks, expr = parse_expr toks in
  let toks = match_token toks Tok_DoubleSemi in
  toks, Expr expr