open MicroCamlTypes
open Utils
open TokenTypes

(* Provided functions - DO NOT MODIFY *)

(* Matches the next token in the list, throwing an error if it doesn't match the given token *)
let match_token (toks: token list) (tok: token) =
  match toks with
  | [] -> raise (InvalidInputException(string_of_token tok))
  | h::t when h = tok -> t
  | h::_ -> raise (InvalidInputException(
      Printf.sprintf "Expected %s from input %s, got %s"
        (string_of_token tok)
        (string_of_list string_of_token toks)
        (string_of_token h)))

(* Matches a sequence of tokens given as the second list in the order in which they appear, throwing an error if they don't match *)
let match_many (toks: token list) (to_match: token list) =
  List.fold_left match_token toks to_match

(* Return the next token in the token list as an option *)
let lookahead (toks: token list) = 
  match toks with
  | [] -> None
  | h::t -> Some h

(* Return the token at the nth index in the token list as an option*)
let rec lookahead_many (toks: token list) (n: int) = 
  match toks, n with
  | h::_, 0 -> Some h
  | _::t, n when n > 0 -> lookahead_many t (n-1)
  | _ -> None

(* Part 2: Parsing expressions *)

let rec parse_expr toks = 
        match lookahead toks with
Some Tok_If -> parseIf toks
|Some Tok_Let -> parseLet toks
|Some Tok_Fun -> parseFun toks
       | _ -> parseOr toks 
and parseOr toks = 
        let(v, exp1) = parseAnd toks in
        match lookahead v with  
        | Some Tok_Or -> let toks1 = (match_token v Tok_Or) in 
        let (v1, exp2) = (parseOr toks1) in (v1, Binop(Or, exp1,exp2))
        | _ -> (v, exp1)
and parseAnd toks = 
        let(v, exp1) = parseEqual toks in
        match lookahead v with
        Some Tok_And -> let toks1 = (match_token v Tok_And) in 
        let (v1, exp2) = (parseAnd toks1) in (v1, Binop(And, exp1,exp2))
        | _ -> (v, exp1)
and parseEqual toks = 
        let (v, exp1) = (parseRelational toks) in
        match lookahead v with
        Some Tok_Equal -> let toks1 = (match_token v Tok_Equal) in
        let (v1, exp2) = (parseEqual toks1) in (v1,Binop(Equal,exp1,exp2))
        | Some Tok_NotEqual -> let toks1 = (match_token v Tok_NotEqual) in
        let (v1, exp2) = (parseEqual toks1) in (v1, Binop(NotEqual,exp1,exp2))
        | _ -> (v, exp1)        
and parseRelational toks = 
        let (v, exp1) = (parseAdditive toks) in
        match lookahead v with
        Some Tok_Less -> let toks1 = (match_token v Tok_Less) in
        let (v1, exp2) = (parseRelational toks1) in (v1,Binop(Less,exp1,exp2))
        | Some Tok_Greater -> let toks1 = (match_token v Tok_Greater) in
        let (v1, exp2) = (parseRelational toks1) in (v1, Binop(Greater,exp1,exp2))
        |Some Tok_LessEqual -> let toks1 = (match_token v Tok_LessEqual) in
        let (v1, exp2) = (parseRelational toks1) in (v1,Binop(LessEqual,exp1,exp2))
        |Some Tok_GreaterEqual -> let toks1 = (match_token v Tok_GreaterEqual) in
        let (v1, exp2) = (parseRelational toks1) in (v1, Binop(GreaterEqual,exp1,exp2))
        | _ -> (v, exp1)         
and parseAdditive toks = 
        let (v, exp1) = (parseMulti toks) in
        match lookahead v with
        Some Tok_Add -> let toks1 = (match_token v Tok_Add) in
        let (v1, exp2) = (parseAdditive toks1) in (v1,Binop(Add,exp1,exp2))
        |Some Tok_Sub -> let toks1 = (match_token v Tok_Sub) in
        let (v1, exp2) = (parseAdditive toks1) in (v1, Binop(Sub,exp1,exp2))
        |_ -> (v, exp1)        
and parseMulti toks = 
        let (v, exp1) = (parseConcat toks) in
        match lookahead v with
        Some Tok_Mult -> let toks1 = (match_token v Tok_Mult) in
        let (v1, exp2) = (parseMulti toks1) in (v1,Binop(Mult,exp1,exp2))
        | Some Tok_Div -> let toks1 = (match_token v Tok_Div) in
        let (v1, exp2) = (parseMulti toks1) in (v1, Binop(Div,exp1,exp2))
        | _ -> (v, exp1)        
and parseConcat toks = 
        let (v, exp1) = (parseUnary toks) in
        match lookahead v with
        Some Tok_Concat -> let toks1 = (match_token v Tok_Concat) in
        let (v1, exp2) = (parseConcat toks1) in (v1,Binop(Concat,exp1,exp2))
        | _ -> (v, exp1)               
and parseUnary toks = 
        match lookahead toks with
        Some Tok_Not -> let toks1 = (match_token toks Tok_Not) in
        let (v1, exp2) = (parseUnary toks1) in (v1,Not(exp2))
        | _ -> parseFunctionCall toks

and parseFunctionCall toks =
        let(v, exp1) = (parsePrimary toks) in
        match lookahead v with
        Some Tok_ID (x) ->
                let (v1, exp2) = (parsePrimary v) in (v1, FunctionCall(exp1, exp2))
        |Some Tok_Int(b)->
                                        let (v1, exp2) = (parsePrimary v) in (v1, FunctionCall(exp1, exp2))
        |Some Tok_String(a) ->
                                        let (v1, exp2) = (parsePrimary v) in (v1, FunctionCall(exp1, exp2))
        |Some Tok_Bool(y) ->
                        let (v1, exp2) = (parsePrimary v) in (v1, FunctionCall(exp1, exp2))
        |Some Tok_LParen ->
                        let (v1, exp2) = (parsePrimary v) in (v1, FunctionCall(exp1, exp2))
        |_ -> (v, exp1)


and parsePrimary toks = 
        match lookahead toks with 
       Some (Tok_Int(x)) -> let toks1 = (match_token toks (Tok_Int(x))) in
        (toks1, Value(Int x))
        |Some (Tok_Bool(x)) -> let toks1 = (match_token toks (Tok_Bool(x))) in
        (toks1, Value(Bool x))
        |Some (Tok_String(x)) -> let toks1 = (match_token toks (Tok_String(x))) in
						 (toks1, Value(String x))
        |Some (Tok_ID(x)) -> let toks1 = (match_token toks (Tok_ID(x))) in
        (toks1, ID x)
        |Some Tok_LParen -> let toks1 = (match_token toks Tok_LParen) in
        let (v1, exp2) = (parse_expr toks1) in
        let v2 = match_token v1 Tok_RParen in (v2, exp2)
       |_ -> raise(InvalidInputException("x"))

and parseIf toks = 
        match lookahead toks with
        |Some Tok_If -> 
                        let toks1 = (match_token toks Tok_If) in
                        let (toks2, if_exp) = (parse_expr toks1) in
                        (match lookahead toks2 with
                        |Some Tok_Then -> let toks4 = (match_token toks2 Tok_Then) in
                        let (toks5, then_exp) = (parse_expr toks4) in
                        (match lookahead toks5 with
                        | Some Tok_Else -> let toks6 = (match_token toks5 Tok_Else) in
                        let (toks7, else_st) =  parse_expr toks6 in
                        (toks7, If(if_exp, then_exp, else_st))  
                        | _ -> raise (InvalidInputException("else error")))
                        | _ -> raise (InvalidInputException("then error")))
                        |_ -> raise (InvalidInputException("if error"))



and parseLet toks = 
        (match lookahead toks with
        |Some Tok_Let -> 
                        let toks1 = (match_token toks Tok_Let) in
                        (match lookahead toks1 with 
                        |Some Tok_Rec -> 
                                        let toks2 = (match_token toks1 Tok_Rec)  in
                                        (match lookahead toks2 with
                                        |Some Tok_ID(x) -> let toks3 = (match_token toks2 (Tok_ID(x))) in
                                        (match lookahead toks3 with
                                        |Some Tok_Equal -> let toks4 = (match_token toks3 Tok_Equal) in
                                        let (toks5, exp1) = (parse_expr toks4) in
                                        (match lookahead  toks5 with
                                        |Some Tok_In -> let toks6 = (match_token toks5 Tok_In) in
                                        let (toks7, body) = (parse_expr toks6) in
                                        (toks7, Let(x, true, exp1, body))
                                        |_-> raise (InvalidInputException("in error"))) 
                                        |_->  raise (InvalidInputException("equal error")))
                                        |_-> raise (InvalidInputException("id error")))
                                        |_ ->  
                                        (match lookahead toks1 with
                                        |Some Tok_ID(x) -> let toks3 = (match_token toks1 (Tok_ID(x))) in
                                        (match lookahead toks3 with
                                        |Some Tok_Equal -> let toks4 = (match_token toks3 Tok_Equal) in
                                        let (toks5, exp1) = (parse_expr toks4) in
                                        (match lookahead toks5 with
                                        |Some Tok_In -> let toks6 = (match_token toks5 Tok_In) in
                                        let (toks7, body) = (parse_expr toks6) in
                                        (toks7, Let(x, false, exp1, body))
                                        |_-> raise (InvalidInputException("in error"))) 
                                        |_->  raise (InvalidInputException("equal error")))
                                        |_-> raise (InvalidInputException("id error"))) )
                        |_ ->  raise (InvalidInputException("let error")))


                and parseFun toks = 
                        (match lookahead toks with
                        |Some Tok_Fun -> 
                                        let toks1 = (match_token toks Tok_Fun) in
                                        (match lookahead toks1 with 
                                        |Some Tok_ID(x) -> 
                                                        let toks2 = (match_token toks1 (Tok_ID(x)))  in
                                                        (match lookahead toks2 with
                                                        |Some Tok_Arrow -> let toks3 = (match_token toks2 Tok_Arrow) in
                                                        let (toks4, exp1) = (parse_expr toks3) in
                                                        (toks4, Fun(x, exp1))
                                                        |_ -> raise (InvalidInputException("arrow")))
                                                        |_ -> raise (InvalidInputException("ID error")))
                                        |_ -> raise (InvalidInputException("fun error")))
(* Part 3: Parsing mutop *)



let rec parse_mutop toks = 
        match lookahead toks with
        Some Tok_Def -> parse_defMutop toks
                                        |Some Tok_DoubleSemi -> parse_doublesemi 
                                        | _ -> parse_exprMutop toks

                                and parse_defMutop toks = 
                                        let toks1 = (match_token toks Tok_Def) in
                                        (match lookahead toks1 with
                                        |Some Tok_ID(x) ->
                                                        let toks2 = (match_token toks1 (Tok_ID(x))) in
                                                        (match lookahead toks2 with
                                                        |Some Tok_Equal -> let toks3=  match_token toks2 Tok_Equal in 
                                                        let(toks4, exp1) = parse_expr toks3 in
                                                        (match lookahead toks4 with
                                                        |Some Tok_DoubleSemi -> let toks5 = match_token toks4 Tok_DoubleSemi in
                                                        (toks5, Def(x,exp1))
                                                        |_ -> raise (InvalidInputException ("double semi error")))
                                                        |_ -> raise (InvalidInputException ("equal error")))
                                                        |_ -> raise (InvalidInputException ("id error")))

                                                and parse_exprMutop toks = 
                                                        let(toks1, expr1) = parse_expr toks in ([], Expr(expr1))

                                                        and parse_doublesemi =
                                                                ([], NoOp)


