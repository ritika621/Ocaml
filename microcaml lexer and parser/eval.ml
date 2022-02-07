open MicroCamlTypes
open Utils

exception TypeError of string
exception DeclareError of string
exception DivByZeroError 

(* Provided functions - DO NOT MODIFY *)

(* Adds mapping [x:v] to environment [env] *)
let extend env x v = (x, ref v)::env

(* Returns [v] if [x:v] is a mapping in [env]; uses the
   most recent if multiple mappings for [x] are present *)
let rec lookup env x =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value)::t -> if x = var then !value else lookup t x

(* Creates a placeholder mapping for [x] in [env]; needed
   for handling recursive definitions *)
let extend_tmp env x = (x, ref (Int 0))::env

(* Updates the (most recent) mapping in [env] for [x] to [v] *)
let rec update env x v =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value)::t -> if x = var then (value := v) else update t x v
        
(* Part 1: Evaluating expressions *)

(* Evaluates MicroCaml expression [e] in environment [env],
   returning a value, or throwing an exception on error *)
let rec eval_expr env e = 

  match e with
  | Value (x) -> x
  | ID x -> lookup env x
  | Not x -> (match eval_expr env x with
        |Bool x -> Bool (not x)
        |_ -> raise (TypeError("Expected type bool")))
  |Binop (opr, exp1, exp2) ->
       (match opr with
        | Add -> let arg1 = eval_expr env exp1 in
                let arg2 = (eval_expr env exp2) in
                (match arg1 with 
                |Int a -> (match arg2 with
                                | Int b -> Int (a + b)
                                | _ -> raise (TypeError("Expected type int")))
                | _ -> raise (TypeError("Expected type int")))
        | Sub -> let arg1 = eval_expr env exp1 in
                 let arg2 = (eval_expr env exp2) in
                  (match arg1 with
                  |Int a -> (match arg2 with                                                                    
                  | Int b -> Int (a - b)
                            | _ -> raise (TypeError("expected type int")))
                  |_ -> raise (TypeError("Expected type int")))
       | Mult -> let arg1 = eval_expr env exp1 in
                 let arg2 = (eval_expr env exp2) in
                 (match arg1 with 
                 |Int a -> (match arg2 with
                        |Int b -> Int (a*b)
                        | _ -> raise (TypeError("Expected type int")))
                 | _ -> raise (TypeError("Expected type int")))
       | Div -> let arg1 = eval_expr env exp1 in
                let arg2 = eval_expr env exp2 in
                (match arg1 with
                |Int a -> (match arg2 with
                        |Int b -> if b = 0 then raise (DivByZeroError) else Int (a/b)
                        | _ -> raise (TypeError("Expected type int")))
                | _ -> raise (TypeError("Expected type int")))
       
       | Greater -> let arg1 = eval_expr env exp1 in
                    let arg2 = eval_expr env exp2 in
                    (match arg1 with
                    |Int a -> (match arg2 with
                             |Int b -> Bool (a>b)
                             | _ -> raise (TypeError("Expected type int")))
                    | _ -> raise (TypeError("Expected type int")))

        | Less -> let arg1 = eval_expr env exp1 in
                  let arg2 = eval_expr env exp2 in
                (match arg1 with
                |Int a -> (match arg2 with
                        |Int b -> Bool (a<b)
                        | _ -> raise (TypeError("Expected type int")))
                | _ -> raise (TypeError("Expected type int")))

       | GreaterEqual -> let arg1 = eval_expr env exp1 in
       let arg2 = eval_expr env exp2 in
       (match arg1 with
       |Int a -> (match arg2 with
       |Int b -> Bool (a>=b)
       | _ -> raise (TypeError("Expected type int")))
       | _ -> raise (TypeError("Expected type int")))

       | LessEqual -> let arg1 = eval_expr env exp1 in
       let arg2 = eval_expr env exp2 in
       (match arg1 with
       |Int a -> (match arg2 with
       |Int b -> Bool (a<=b)
       | _ -> raise (TypeError("Expected type int")))
       | _ -> raise (TypeError("Expected type int")))


       | Concat -> let arg1 = eval_expr env exp1 in
       let arg2 = eval_expr env exp2 in
       (match arg1 with
       |String a -> (match arg2 with
       |String b -> let stri =  a^b in
                     String stri
       | _ -> raise (TypeError("Expected type string")))
       | _ -> raise (TypeError("Expected type string")))

       | Equal-> let arg1 = eval_expr env exp1 in
                let arg2 = eval_expr env exp2 in
                (match arg1 with
                |Int a -> (match arg2 with
                         |Int b -> Bool (a=b)
                         | _ -> raise (TypeError ("Cannot compare types")))
                |Bool a -> (match arg2 with
                        |Bool b -> Bool (a=b)
                        | _ -> raise (TypeError ("Cannot compare types")))
                |String a -> (match arg2 with
                        |String b -> Bool (a=b)
                        | _ -> raise (TypeError ("Cannot compare types")))
                
                | _ -> raise (TypeError ("Cannot compare types")))



         | NotEqual-> let arg1 = eval_expr env exp1 in
          let arg2 = eval_expr env exp2 in
              (match arg1 with
              |Int a -> (match arg2 with
              |Int b -> Bool (a!=b)
             | _ -> raise (TypeError ("Cannot compare types")))
               |Bool a -> (match arg2 with
               |Bool b -> Bool (a!=b)
               |_ -> raise (TypeError ("Cannot compare types")))
               |String a -> (match arg2 with
              |String b -> Bool (a!=b)
              | _ -> raise (TypeError ("Cannot compare types")))
           
           | _ -> raise (TypeError ("Cannot compare types")))

              |Or -> let arg1 = eval_expr env exp1 in
                        let arg2 = eval_expr env exp2 in
                     (match arg1 with
                     |Bool a -> (match arg2 with
                              |Bool b -> Bool (a || b)
                              | _ -> raise (TypeError("Expected type bool")))
                      |_ -> raise (TypeError("Expected type bool")))

                     |And -> let arg1 = eval_expr env exp1 in
                     let arg2 = eval_expr env exp2 in
      (match arg1 with
      |Bool a -> (match arg2 with
      |Bool b -> Bool (a && b)
      | _ -> raise (TypeError("Expected type bool")))
      |_ -> raise (TypeError("Expected type bool")))      
                     
                   
		     )
                   

       |If (guard, exp1, exp2) ->
       let guard1 = eval_expr env guard in
       (match guard1 with
	      | Bool a -> if a = true then eval_expr env exp1 else eval_expr env exp2
| _ -> raise (TypeError("Expected type bool")))

|Let (var, false, expr1, expr2) -> let value = eval_expr env expr1 in
		    let new_env = extend env var value in
		    eval_expr new_env expr2
		    
 |Let(var, true, expr1, expr2) ->  let ext_env = extend_tmp env var in
		     let eval = eval_expr ext_env expr1 in
		     update ext_env var eval;
		     eval_expr ext_env expr2

		     
		    
 |Fun(var, expr) -> Closure(env, var, expr)

 |FunctionCall(expr1, expr2) -> (match eval_expr env expr1 with
                                |Closure(env1, x, e) -> let v = eval_expr env expr2 in
                                                     let new_env = extend env1 x v in
                                                     eval_expr new_env e
                               |_ -> raise(TypeError("Not a function")))
 
 
              
(* Part 2: Evaluating mutop directive *)

(* Evaluates MicroCaml mutop directive [m] in environment [env],
   returning a possibly updated environment paired with
   a value option; throws an exception on error *)
   let eval_mutop env m = match m with

|Def (var, expr1) ->     let new_env = extend_tmp env var in
                         let v = eval_expr new_env expr1 in
                       update new_env var v;
                            (new_env, Some (v))
                         
|Expr (expr1) -> (env, Some (eval_expr env expr1))
   |NoOp -> (env, None)
