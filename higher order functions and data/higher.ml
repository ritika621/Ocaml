open Funs

(********************************)
(* Part 1: High Order Functions *)
(********************************)

let contains_elem lst e =
if (fold (fun a x -> if x = e then a + 1 else a) 0 lst) >= 1 then true else false

let is_present lst x =
map(fun a -> if a = x then 1 else 0) lst

let count_occ lst target =
fold (fun a x -> if x = target then a+1 else a) 0 lst

let uniq lst = fold_right(fun x a -> if contains_elem a x = true then a else x::a) lst []

let assoc_list lst =
let list = uniq lst in
map(fun x -> (x, count_occ lst x)) list

let ap fns args =
fold_right (fun x a -> (map x args) @ a) fns []
