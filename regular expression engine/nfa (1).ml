open List
open Sets

(*********)
(* Types *)
(*********)

type ('q, 's) transition = 'q * 's option * 'q

type ('q, 's) nfa_t = {
  sigma: 's list;
  qs: 'q list;
  q0: 'q;
  fs: 'q list;
  delta: ('q, 's) transition list;
}

(***********)
(* Utility *)
(***********)

(* explode converts a string to a character list *)
let explode (s: string) : char list =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l)
  in
  exp (String.length s - 1) []

(****************)
(* Part 1: NFAs *)
(****************)

let rec remove_duplicate lst new_lst =
match lst with
[] -> new_lst
|h::t -> if List.mem h new_lst = true then remove_duplicate t new_lst else remove_duplicate t (h::new_lst)

let move (nfa: ('q,'s) nfa_t) (qs: 'q list) (s: 's option) : 'q list =
  match nfa with
{sigma =_;qs =_;q0 = _;fs =_; delta = delta_func} ->
let lst =
List.fold_right(fun x a -> match x with (i,j,k) -> if List.mem i qs && j = s then k::a else a) delta_func [] in remove_duplicate lst []

let e_closure (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list =
  match nfa with
    {sigma =_;qs=_;q0=_;fs=_;delta=delta_func} ->
      let lst = List.fold_left(fun a x -> match x with (i,j,k) -> if List.mem i qs && j = None then [i;k]@a else if List.mem i a && j = None then [i;k]@a else a) [] delta_func in remove_duplicate (qs@lst) []

let rec make alphabet nfa lst qs =
match alphabet with
[] -> if List.length lst < List.length nfa.sigma then let lst1 = [[]]@lst in remove_duplicate lst1 [] else remove_duplicate lst []
|h::t -> let lst1 =
(List.fold_left(fun a x -> let lst2 = (move nfa [x] (Some h) @ e_closure nfa (move nfa [x] (Some h))) in (remove_duplicate lst2 []) @ a) [] qs) in if lst1 != [] then make t nfa ([lst1]@lst) qs else make t nfa lst qs

(*******************************)
(* Part 2: Subset Construction *)
(*******************************)

let new_states (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list =
if qs = [] then [[];[]]
else
match nfa with
{sigma = alphabet; qs=_;q0=_;fs=_;delta=_} -> let a = alphabet in make a nfa [] qs

let transhelper nfa symbol qs =
List.fold_left(fun a x -> let lst2 = (move nfa [x] (Some symbol) @ e_closure nfa (move nfa [x] (Some symbol))) in (remove_duplicate lst2 []) @ a) [] qs

let new_trans (nfa: ('q,'s) nfa_t) (qs: 'q list) : ('q list, 's) transition list =
let lst = List.fold_left (fun a x -> (qs, Some x, transhelper nfa x qs)::a) [] nfa.sigma in remove_duplicate lst []

                    let rec final nfa lst_finals qs =
                    match lst_finals with
                    [] ->[]
                    |h::t -> if List.mem h qs then [qs] else
final nfa t qs

let new_finals (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list =
 match nfa with
{sigma =_;qs=_;q0=_;fs=finals;delta = _} ->
let f = finals in
final nfa f qs

let rec check lst_finals qs =
match qs with
[] -> false
|h::t -> if List.mem h lst_finals = true then true else check lst_finals t

let rec remove_empty ls new_list =
match ls with
[] -> new_list
|h::t -> if h = [] then remove_empty t new_list else remove_empty t (h::new_list)

 let rec accepthelper nfa states_from charlst =
match nfa with
{sigma =_;qs=_;q0=_;fs=final;delta=_} ->
match charlst with
[] -> if check final states_from = false then false else true
|h::t -> 
let movelist = move nfa (e_closure nfa states_from) (Some h) in
          let states_reachable = e_closure nfa movelist in
          if states_reachable = [] then false else accepthelper nfa states_reachable t


let accept (nfa: ('q,char) nfa_t) (s: string) : bool =
match nfa with {sigma =_;qs=_;q0=startstate;fs=final;delta=_} ->
let charlist = explode s in
if (s = "") then
let e_closure_states = e_closure nfa [startstate] in
check final e_closure_states
else
accepthelper nfa [startstate] charlist


let rec dfa_states nfa lst qs =
let list1 = let list =
(List.fold_right(fun x a -> (List.fold_right(fun y b -> [(transhelper nfa x y)]@b) qs [])@a) nfa.sigma [])
in remove_empty list [] in
let list2 = remove_duplicate list1 [] in
if list2 != [] then
dfa_states nfa (list1@lst) list1
else lst

let rec final_states nfa lst list =
match lst with
[] -> list
|h::t -> if new_finals nfa h != [] then final_states nfa t ([h]@list) else final_states nfa t list

let new_delta nfa states_list =
List.fold_left(fun a x -> new_trans nfa x @ a) [] states_list

let rec nfa_to_dfa_step (nfa: ('q,'s) nfa_t) (dfa: ('q list, 's) nfa_t)
    (work: 'q list list) : ('q list, 's) nfa_t =
  failwith "unimplemented"


let nfa_to_dfa (nfa: ('q,'s) nfa_t) : ('q list, 's) nfa_t =
match nfa with
{sigma = alphabet; qs=_;q0=nfa_startstate;fs=_;delta=transitions} ->
let dfa_startstate = e_closure nfa [nfa_startstate] in
let qs1 = dfa_states nfa [[nfa_startstate]] [[nfa_startstate]] in
let new_final_list = final_states nfa qs1 [] in
let delta_new = new_delta nfa qs1 in
{sigma = alphabet; qs =qs1;q0=dfa_startstate; fs = new_final_list; delta = delta_new}
