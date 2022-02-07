(***********************************)
(* Part 1: Non-Recursive Functions *)
(***********************************)

let rev_tup tup =
let (a,b,c) = tup in (c, b, a) 

let is_odd x =
if x mod 2 = 0 then false
else true;;

let area (a1, b1) (a2, b2) =
(abs((a1 - a2) * (b1 - b2)));;

let volume (a1, b1, c1) (a2, b2, c2) =
(abs((a1 - a2) * (b1 - b2) * (c1 - c2)));;

(*******************************)
(* Part 2: Recursive Functions *)
(*******************************)

let rec fibonacci n =
if n = 0 then 0
else if n = 1 then 1
else fibonacci(n-1) + fibonacci(n-2)

let rec pow x y =
if y = 0 then 1
 else x * (pow x (y-1))
				
let rec log x y =
if y = 0 then 0
else if y = 1 then 0
else if (y/x) = 0 then 0
else if (y/x) = 1 then 1
else 1 + (log x (y/x))

let rec gcf x y =
if y = 0 then x
else if x = 0 then y
else (gcf y (x mod y))

let rec fold f a l =
match l with
| [] -> a
| h::t -> fold f (f a h) t

let rec is_prime x =
if x < 2 then false
else
let rec is_prime_aux num =
if (num < x) then
num :: (is_prime_aux (num + 1))
else
[]
in
fold (fun a h -> a && (x mod h) != 0) true (is_prime_aux 2)

(*****************)
(* Part 3: Lists *)
(*****************)

let rec get idx lst =
match lst with
[] -> failwith "Out of bounds"
| (h::t) -> if idx = 0 then h else get (idx-1) t;;


let rec length list = match list with
[] -> 0
| (_::t) -> 1 + length t

let larger lst1 lst2 =
if (length lst1) > (length lst2) then
lst1
else if (length lst1) = (length lst2) then
[]
else
lst2


let rec combine lst1 lst2 =
match lst1 with
[] -> lst2
| (x :: xs) -> x :: (combine xs lst2)

let rec reverse lst = match lst with
[] -> []
| (x::xs) -> combine (reverse(xs)) [x]

let rec merge lst1 lst2 = match lst1, lst2 with
| [],_ -> lst2
| _, [] -> lst1
| x :: a, y :: b ->
if x < y then x :: merge a lst2 else y :: merge lst1 b

let rot_to_front list =
match list with
|[] -> []
|[x] -> [x]
|initial::remaining -> (remaining@(initial::[]));;

let rec rotate shift lst =
match shift with
| 0 -> lst
| _ -> rotate (shift - 1) (rot_to_front lst);;

let rec is_palindrome lst = lst = reverse lst 
