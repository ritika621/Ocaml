open Funs

(*************************************)
(* Part 2: Three-Way Search Tree *)
(*************************************)

type int_tree =
  | IntLeaf
  | IntNode of int * int option * int_tree * int_tree * int_tree 

let empty_int_tree = IntLeaf

let rec int_insert x t =
match t with
 IntLeaf -> IntNode(x, None, IntLeaf, IntLeaf, IntLeaf)
| IntNode(n, m, left, middle, right) ->
match m with
Some i -> if x < n then IntNode(n, m, (int_insert x left), middle, right)
else if x > i then IntNode(n, m, left, middle, (int_insert x right))
else IntNode(n, m, left, (int_insert x middle), right)
|None -> if x < n then IntNode(x, Some n, IntLeaf, IntLeaf, IntLeaf)
else if x > n then
IntNode(n, Some x, IntLeaf, IntLeaf, IntLeaf)
else
IntNode(x, None, IntLeaf, IntLeaf, IntLeaf)


let rec int_mem x t =
match t with
IntLeaf -> false
|IntNode(n,m,left,middle,right) -> match m with
Some i -> if x = n then true else if x = i then true
else if x < n then int_mem x left 
else if x > i then int_mem x right
else int_mem x middle
| None -> if x = n then true else false


let rec int_size t =
match t with
IntLeaf -> 0
|IntNode(n,m,left,middle,right) ->
match m with 
Some i -> 2 + int_size left + int_size middle + int_size right
|None -> 1 + int_size left + int_size middle + int_size right

let rec int_max t =
match t with
IntLeaf -> raise(Invalid_argument("int_max"))
|IntNode(n,m,left,middle,right) -> 
match m with
Some i -> if right != IntLeaf then int_max right else i
|None -> n

(*******************************)
(* Part 3: Three-Way Search Tree-Based Map *)
(*******************************)

type 'a tree_map =
  | MapLeaf
  | MapNode of (int * 'a) * (int * 'a) option * 'a tree_map * 'a tree_map * 'a tree_map

let empty_tree_map = MapLeaf

let rec map_put k v t = 
match t with
MapLeaf -> MapNode((k,v), None, MapLeaf, MapLeaf, MapLeaf)
| MapNode((a,b), m, left, middle, right) ->
if k = a then raise(Invalid_argument("map_put"))
else
match m with
Some (x,y) -> if k < a then MapNode((a,b), m, map_put k v left, middle, right)
else if k > x then MapNode((a,b), m, left, middle, map_put k v right)
else MapNode((a,b), m, left, map_put k v middle, right)
|None -> if k < a then MapNode((k,v), Some (a,b), MapLeaf, MapLeaf, MapLeaf)
else if k > a then MapNode((a,b), Some (k,v), MapLeaf, MapLeaf, MapLeaf)
else
MapNode((k,v), None, MapLeaf, MapLeaf, MapLeaf)

let rec map_contains k t = 
match t with
MapLeaf -> false
| MapNode((a,b), m, left, middle , right) ->
match m with 
Some i -> if k = a then true else if k < a then map_contains k left
else if k > a then if k = a then true else map_contains k right
else map_contains k middle
|None -> if k = a then true else false

let rec map_get k t =
match t with
MapLeaf -> raise(Invalid_argument("map_get"))
|MapNode((a,b), m, left, middle, right) ->
match m with
Some (c,d) -> if k = a then b 
else if k < a then map_get k left
else if k > a then if k = c then d else map_get k right
else map_get k middle
|None -> if k = a then b else map_get k MapLeaf

(***************************)
(* Part 4: Variable Lookup *)
(***************************)

(* Modify the next line to your intended type *)
type lookup_table = (string * int) list list

let empty_table : lookup_table = []

let push_scope (table : lookup_table) : lookup_table = 
  []::table

let pop_scope (table : lookup_table) : lookup_table =
match table with
h::t -> t
|empty_table -> failwith "No scopes remain!"

let helper lst name =
if(fold(fun a x -> match x with (n,m) -> if n = name then a + 1 else a) 0 lst) = 1 then true else false

let add_var name value (table : lookup_table) : lookup_table =
  match table with
h::t -> if helper h name = false then [(name,value)::h] @ t
else
failwith "Duplicate variable binding in scope!"
|[] -> failwith "There are no scopes to add a variable to!"

let rec helper_find lst name =
match lst with
[] -> failwith "Variable not found!"
|(a,b)::t -> if a = name then b else helper_find t name

let rec lookup name (table : lookup_table) =
  match table with
h::t -> helper_find h name
|[] -> failwith "Variable not found!"
