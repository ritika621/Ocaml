open TokenTypes

(* Part 1: Lexer - IMPLEMENT YOUR CODE BELOW *)

let reg_Lparen = Str.regexp "("
let reg_Rparen = Str.regexp ")"
let reg_Equal = Str.regexp "="
let reg_NotEqual = Str.regexp "<>"
    
let reg_Greater = Str.regexp ">"
let reg_Less = Str.regexp "<"
    
let reg_GreaterEqual = Str.regexp ">="
let reg_LessEqual = Str.regexp "<="
let reg_Or = Str.regexp "||"
let reg_And = Str.regexp "&&"
let reg_Not = Str.regexp "not"
let reg_If = Str.regexp "if"
let reg_Then = Str.regexp "then"
let reg_Else = Str.regexp "else"
let reg_Add = Str.regexp "+"
let reg_Sub= Str.regexp "-"
let reg_Mult = Str.regexp "*"
let reg_Div = Str.regexp "/"
let reg_Concat = Str.regexp "\\^"
let reg_Let = Str.regexp "let"
let reg_Def = Str.regexp "def"
let reg_Rec = Str.regexp "rec"
let reg_In = Str.regexp "in"
let reg_Fun = Str.regexp "fun"
let reg_Arrow = Str.regexp "->"
let reg_DoubleSemi = Str.regexp ";;"
let reg_Bool =Str.regexp "true\\|false"
let reg_Int = Str.regexp "[0-9]+"

let reg_IntNeg= Str.regexp "(\\(-[0-9]+\\))"

let reg_String =Str.regexp "\"[^\"]*\""
    
let reg_Id=  Str.regexp "[a-zA-Z][a-zA-Z0-9]*"
let space = Str.regexp "\\( \\)\\|\\(\t\\)\\|\\(\n\\)"



exception InvalidInputException of string 

let rec word list = 
        match list with 
        []-> raise (InvalidInputException ("Wrong string"))
        |h::t -> if h = "" then word t else h




let tokenize input =
let rec tok pos s = 
        if pos >= String.length s then
                []


                 else if
                     (Str.string_match reg_Arrow s pos) then
                     (Tok_Arrow)::(tok (pos+2) s)

else if
                        (Str.string_match reg_Sub s pos) then
                                                        (Tok_Sub)::(tok (pos+1) s)
else if

 (Str.string_match reg_IntNeg s pos) then
         let token = Str.matched_string s in
         let list = String.split_on_char '(' token in
         let x = word list in
         let list2 =  String.split_on_char ')' x in
         let x1 = word list2 in
         (Tok_Int (int_of_string x1))::(tok (pos+(String.length token)) s)





        else if 
                (Str.string_match reg_Int s pos) then
                        let token = Str.matched_string s in
                        (Tok_Int (int_of_string token))::(tok (pos+(String.length token)) s)
        else if (Str.string_match reg_Lparen s pos) then
                (Tok_LParen)::(tok (pos+1) s)
        else if
                (Str.string_match reg_Rparen s pos) then
                        (Tok_RParen)::(tok (pos+1) s)
        else if
                (Str.string_match reg_NotEqual s pos) then
                        (Tok_NotEqual)::(tok (pos+2) s)
        else if
                (Str.string_match reg_Equal s pos) then
                   
                        (Tok_Equal)::(tok (pos+1) s)



        else if
                (Str.string_match reg_GreaterEqual s pos) then
                        (Tok_GreaterEqual)::(tok (pos+2) s)
        else if
                (Str.string_match reg_LessEqual s pos) then
                        (Tok_LessEqual)::(tok (pos+2) s)
        else if
                (Str.string_match reg_Less s pos) then
                        (Tok_Less)::(tok (pos+1) s)
        else if
                (Str.string_match reg_Greater s pos) then
                        (Tok_Greater)::(tok (pos+1) s)
        else if
                (Str.string_match reg_Or s pos) then
                        (Tok_Or)::(tok (pos+2) s)
        else if
                (Str.string_match reg_And s pos) then
                        (Tok_And)::(tok (pos+2) s)
        else if
                (Str.string_match reg_Not s pos) then
                        (Tok_Not)::(tok (pos+3) s)
        else if
                (Str.string_match reg_If s pos) then
                        (Tok_If)::(tok (pos+2) s)
        else if
                (Str.string_match reg_Then s pos) then
                        (Tok_Then)::(tok (pos+4) s)
        else if
                (Str.string_match reg_Else s pos) then
                        (Tok_Else)::(tok (pos+4) s)
        else if
                (Str.string_match reg_Add s pos) then
                        (Tok_Add)::(tok (pos+1) s)
      
        else if
                (Str.string_match reg_Mult s pos) then
                        (Tok_Mult)::(tok (pos+1) s)
        else if
                (Str.string_match reg_Div s pos) then
                        (Tok_Div)::(tok (pos+1) s)
        else if
                (Str.string_match reg_Concat s pos) then
                        (Tok_Concat)::(tok (pos+1) s)
        else if
                (Str.string_match reg_Let s pos) then
                        (Tok_Let)::(tok (pos+3) s)
        else if
                (Str.string_match reg_Rec s pos) then
                        (Tok_Rec)::(tok (pos+3) s)
        else if
                (Str.string_match reg_In s pos) then
                        (Tok_In)::(tok (pos+2) s)
        else if
                (Str.string_match reg_Def s pos) then
                        (Tok_Def)::(tok (pos+3) s)
        else if
                (Str.string_match reg_Fun s pos) then
                        (Tok_Fun)::(tok (pos+3) s)
        
        else if
                (Str.string_match reg_DoubleSemi s pos) then
                        (Tok_DoubleSemi)::(tok (pos+2) s)
        else if
                (Str.string_match reg_String s pos) then
                        let token = Str.matched_string s in
 let list = String.split_on_char '"' token
 in let x = word list
 in  (Tok_String x)::(tok (pos+(String.length token)) s)


        else if (Str.string_match reg_Bool s pos) then
                let token = Str.matched_string s in
                (Tok_Bool (bool_of_string token))::(tok (pos+(String.length token)) s)
        else if
                (Str.string_match reg_Id s pos) then
                        let token = Str.matched_string s in
                        (Tok_ID token)::(tok (pos+(String.length token)) s)
        else if (Str.string_match space s pos) then
                let token = Str.matched_string s in
                tok (pos + (String.length token)) s
        else 
                raise (InvalidInputException("WrongInput"))
in tok 0 input




