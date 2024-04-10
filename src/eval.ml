
open Types

(* Provided functions - DO NOT MODIFY *)

(* Adds mapping [x:v] to environment [env] *)
let extend env x v = (x, ref v) :: env

(* Returns [v] if [x:v] is a mapping in [env]; uses the
   most recent if multiple mappings for [x] are present *)
let rec lookup env x =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value) :: t -> if x = var then !value else lookup t x

(* Creates a placeholder mapping for [x] in [env]; needed
   for handling recursive definitions *)
let extend_tmp env x = (x, ref (Int 0)) :: env

(* Updates the (most recent) mapping in [env] for [x] to [v] *)
let rec update env x v =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value) :: t -> if x = var then value := v else update t x v


(* Part 1: Evaluating expressions *)

(* Evaluates MicroCaml expression [e] in environment [env],
   returning an expression, or throwing an exception on error *)
   let rec eval_expr env e =
    match e with
    | Int _ | Bool _ | String _ -> e
    | ID x -> lookup env x
    | Not e1 ->
        (match eval_expr env e1 with
         | Bool b -> Bool (not b)
         | _ -> raise (TypeError "Expected boolean in Not expression"))
    | Binop (op, e1, e2) ->
        let v1 = eval_expr env e1 in
        let v2 = eval_expr env e2 in
        (match op, v1, v2 with
         | Add, Int x, Int y -> Int (x + y)
         | Add, _, _ -> raise (TypeError "Expected integers in addition")
         | Sub, Int x, Int y -> Int (x - y)
         | Sub, Int x, _ -> Int (-x)
         | Sub, _, _ -> raise (TypeError "Expected integers in subtraction")
         | Mult, Int x, Int y -> Int (x * y)
         | Mult, _, _ -> raise (TypeError "Expected integers in multiplication")
         | Div, Int x, Int y ->
             if y = 0 then raise DivByZeroError else Int (x / y)
         | Div, _, _ -> raise (TypeError "Expected integers in division")
         | Greater, Int x, Int y -> Bool (x > y)
         | Greater, _, _ -> raise (TypeError "Expected integers in Greater comparison")
         | Less, Int x, Int y -> Bool (x < y)
         | Less, _, _ -> raise (TypeError "Expected integers in Less comparison")
         | GreaterEqual, Int x, Int y -> Bool (x >= y)
         | GreaterEqual, _, _ -> raise (TypeError "Expected integers in GreaterEqual comparison")
         | LessEqual, Int x, Int y -> Bool (x <= y)
         | LessEqual, _, _ -> raise (TypeError "Expected integers in LessEqual comparison")
         | Concat, String x, String y -> String (x ^ y)
         | Concat, _, _ -> raise (TypeError "Expected strings in concatenation")
         | Equal, Int x, Int y -> Bool (x = y)
         | Equal, Bool x, Bool y -> Bool (x = y)
         | Equal, String x, String y -> Bool (x = y)
         | Equal, _, _ -> raise (TypeError "Cannot compare different types for equality")
         | NotEqual, Int x, Int y -> Bool (x <> y)
         | NotEqual, Bool x, Bool y -> Bool (x <> y)
         | NotEqual, String x, String y -> Bool (x <> y)
         | NotEqual, _, _ -> raise (TypeError "Cannot compare different types for inequality")
         | Or, Bool x, Bool y -> Bool (x || y)
         | Or, _, _ -> raise (TypeError "Expected booleans in Or operation")
         | And, Bool x, Bool y -> Bool (x && y)
         | And, _, _ -> raise (TypeError "Expected booleans in And operation"))
    | If (e1, e2, e3) ->
        (match eval_expr env e1 with
         | Bool true -> eval_expr env e2
         | Bool false -> eval_expr env e3
         | _ -> raise (TypeError "Expected boolean in If condition"))
    | Let (x, recursive, e1, e2) ->
        let new_env = if recursive then extend_tmp env x else env in
        let v1 = eval_expr new_env e1 in
        if recursive then update new_env x v1;
        eval_expr (extend new_env x v1) e2
    | Fun (x, e1) -> Closure (env, x, e1)
    | App (e1, e2) ->
        let v1 = eval_expr env e1 in
        let v2 = eval_expr env e2 in
        (match v1 with
         | Closure (closure, x, e) -> eval_expr (extend closure x v2) e
         | _ -> raise (TypeError "Expected closure in function application"))
    | Record field_list ->
        let eval_field (Lab l, e) = (Lab l, eval_expr env e) in
        Record (List.map eval_field field_list)
    | Select (Lab l, e1) ->
        let rec field fields =
          match fields with
          | [] -> raise (SelectError ("Label " ^ l ^ " not found in record"))
          | (Lab l2, v) :: t -> if l = l2 then v else field t
        in
        (match eval_expr env e1 with
         | Record fields -> field fields
         | _ -> raise (TypeError "Expected record in Select expression"))
    | Closure _ -> raise (TypeError "Cannot evaluate a closure")

(* Part 2: Evaluating mutop directive *)

(* Evaluates MicroCaml mutop directive [m] in environment [env],
   returning a possibly updated environment paired with
   a value option; throws an exception on error *)

   let eval_mutop env = function
   | NoOp -> (env, None)
   | Expr expr ->
       let result = eval_expr env expr in
       (env, Some result)
   | Def (var, expr) ->
       let evaluated_expr = eval_expr env expr in  
       let new_env = extend env var evaluated_expr in 
       (new_env, Some evaluated_expr)