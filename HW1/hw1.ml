(* Question 1 - check if a is subset of b *)
let subset a b = 
  List.for_all(fun x -> List.exists (fun y -> x=y) b) a

(* Question 2 - check for equal sets *)
let equal_sets a b = 
  (subset a b) && (subset b a)

(* Question 3 - set union *)
let rec set_union a b =
  match a with
  | [] -> b
  | x::rest -> x::(set_union rest b)

(* Question 4 - set intersection *)
let set_intersection a b =
  List.filter(fun x -> List.exists (fun y -> x=y) b) a

(* Question 5 - set difference a not in b*)
let set_diff a b = 
  List.filter(fun x -> not(List.exists (fun y -> x=y) b)) a

(* Question 6 - computed fixed point *)
let rec computed_fixed_point eq f x =
  if (eq(f x) x) then x else computed_fixed_point eq f (f x)

(* Question 7 - filter unreachable grammar *)
type ('a, 'b) symbol = N of 'a | T of 'b

(* given list of symbols, return non-terminal symbols as a list *)
let rec filter_given_symbols_list lst = 
  match lst with
  | [] -> []
  | x::rest -> match x with
    | N a -> a::(filter_given_symbols_list rest)
    | _ -> filter_given_symbols_list rest

(* given list of rules, return list of non-terminal symbols corresponding to rules *)
let rec filter_given_rules_list lst = 
  match lst with
  | [] -> []
  | x::rest -> match x with
    | (a, b) -> (set_union (filter_given_symbols_list b) (filter_given_rules_list rest))

(* given grammar, return list of non-terminals reachable by a *)
let filter_given_grammar g = match g with
| (a, b) -> 
  a::(filter_given_rules_list (List.filter(fun x -> match x with 
    | (c, d) -> if c = a then true else false) b))

(* given list of non-terminal symbols, return non-terminal symbols reachable by given list *)
let rec get_nonterminal_sublist lst des = 
  match lst with
  | [] -> []
  | x::rest -> (set_union (filter_given_grammar (x, des)) (get_nonterminal_sublist rest des))

(* returns list of all reachable non-terminals *)
let rec get_nonterminal_full_list checked all des =
  let curr_list = (set_diff all checked) in 
    match curr_list with
    | [] -> all
    | _ -> get_nonterminal_full_list all (set_union (get_nonterminal_sublist curr_list des) all) des

(* compares a given lst to grammar g to remove unreachable grammar *)
let rec compare_list g lst = match g with
  | (a, b) -> match b with
    | [] -> [];
    | x::rest -> match x with 
      | (c, d) -> if (subset [c] lst) 
        then (c, d)::(compare_list (a, rest) lst)
        else compare_list (a, rest) lst

(* main function that returns new grammar with unreachable grammar removed *)
let filter_reachable g = match g with
| (a, b) -> let all = get_nonterminal_full_list [a] (filter_given_grammar g) b in
  (a, compare_list g all)