(* Question 1 Convert HW1 Grammar to HW2 Grammar *)

let convert_grammar old_grammar = 
  let rec get_related_symbols lst =
    match lst with
    | [] -> []
    | x::rest -> match x with
      | (a, b) -> b::(get_related_symbols rest) in   
  match old_grammar with
  | (expr, rules) -> (expr, (fun input ->
      get_related_symbols (List.filter(fun (a,b) -> a = input) rules)
    ))

(* Question 2 Parse Tree Leaves *)

type ('nonterminal, 'terminal) parse_tree =
  | Node of 'nonterminal * ('nonterminal, 'terminal) parse_tree list
  | Leaf of 'terminal

let parse_tree_leaves tree =
  let rec parse_tree_leaves_helper node_list = 
    match node_list with
    | [] -> []
    | x::rest -> match x with
      | Node (a, lst) -> (parse_tree_leaves_helper lst)@(parse_tree_leaves_helper rest)
      | Leaf leaf -> leaf::(parse_tree_leaves_helper rest) in
   parse_tree_leaves_helper [tree]

(* Question 3 Matcher *)

type ('a, 'b) symbol = N of 'a | T of 'b

(* helper function that takes care of matching terminals *)
let term_func frag_list t_element k = 
  match frag_list with
    | ind_frag::rest_frag -> if ind_frag = t_element 
      then k rest_frag
      else None (* when terminal fragment doesnt match terminal element *)
    | [] -> None (* happens when no more fragments, to match, but still rules, therefore doesnt match the rule *)

(* mutually recursive helper function *)
let rec check_match rule func k fragment = 
  match rule with
  | [] -> k fragment
  | ind_rule::rest_rule -> match ind_rule with
    | N element -> if fragment=[] 
      then if rule=[] then k [] else None
      else (non_term_func (func element) fragment func (check_match rest_rule func k)) 
    | T element -> (term_func fragment element (check_match rest_rule func k))
and non_term_func rules_list frag func k = 
  match rules_list with 
  | [] -> None
  | rule::rest_rule -> match (check_match rule func k frag) with
    | None -> (non_term_func rest_rule frag (func) (k))
    | Some a -> Some a

let make_matcher gram = 
  match gram with 
  | (expr, func) -> (
      fun acceptor fragment -> (non_term_func (func expr) fragment func acceptor)
)

(* Question 4 Parser *)

let parser_acceptor path fragment = match fragment with
  | [] -> Some path
  | _ -> None

(* helper function that takes care of matching terminals *)
let term_func_parser frag_list t_element k = 
  match frag_list with
    | ind_frag::rest_frag -> if ind_frag = t_element 
      then k rest_frag
      else None (* when terminal fragment doesnt match terminal element *)
    | [] -> None (* happens when no more fragments, to match, but still rules, therefore doesnt match the rule *)

(* mutually recursive helper function *)
let rec check_match_parser rule k func path fragment = 
  match rule with
  | [] -> k path fragment 
  | ind_rule::rest_rule -> match ind_rule with
    | N element -> if fragment=[] 
      then if rule=[] then (k path []) else None
      else (non_term_func_parser element func (func element) (check_match_parser rest_rule (k) func) fragment path) 
    | T element -> (term_func_parser fragment element (check_match_parser rest_rule (k) func path))
and non_term_func_parser start_symbol func rules_list k frag path = 
  match rules_list with 
  | [] -> None
  | rule::rest_rule -> match (check_match_parser rule (k) (func) ((start_symbol, rule)::path) frag) with
    | None -> (non_term_func_parser start_symbol (func) rest_rule (k) frag path)
    | Some a -> Some a

(* want to return pairs of start symbols and rules *)
let make_list gram = 
  match gram with 
  | (expr, func) -> (
    fun fragment -> (non_term_func_parser (expr) (func) (func expr) (parser_acceptor) (fragment) ([]))
  )

(* returns tuple of path remaining, and **node** *)
(* call make_tree_helper when you want to build tree from path beginning with NT *)
let rec make_tree_helper path = 
  match path with
  (* path is list of non_terminal and corresponding rule *)
  | x::rest -> (match make_children_helper rest (snd x) with
    | (path_remain, children) -> path_remain, Node((fst x), children))
  | [] -> invalid_arg "path" (* this case should not happen *)
(* should return tuple of remaining path and **list** of nodes/leafs that are children of the same node *)
and make_children_helper rest_path curr_rules = 
  (* curr_rules is the rule for current nonterminal *)
  match curr_rules with 
  | [] -> rest_path, [] (* you done, since you finished traversing the entire rule *)
  | curr::rest_rules -> match curr with (* get curr element and build it's tree *)
    (* because nonterminal its next node will def be its children, so we call make_tree_helper *)
    | N element -> (match (make_tree_helper rest_path) with (* build subtree for NT element and deal with siblings *)
      | (path_remain, node) -> (* path_remain is rest path for current level, node is node containing its subtree *)
        (match (make_children_helper path_remain rest_rules) with (* want to get node/leaf for rest of sym in current level *)
        (* path_remain_remain is rest of path that is completely unrelated to current NT parent *)
        (* sibling is list of node/leaf that are children of current NT parent*)
        | (path_remain_remain, sibling) -> path_remain_remain, node::sibling))
    (* because terminal, want to make current element into leaf and deal with rest node/leaf on curr level *)
    | T element -> (match (make_children_helper rest_path rest_rules) with 
      (* sibling is list of nodes/leaf that are also in this level of tree *)
      | (path_remain, sibling) -> path_remain, (Leaf element)::sibling)


let make_parser grammar fragment = 
  let path_list = (make_list grammar fragment) in 
    match path_list with
    | Some path -> Some (snd (make_tree_helper (List.rev path)))
    | None -> None