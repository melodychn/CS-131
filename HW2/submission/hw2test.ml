(* matcher test cases *)

(* sample acceptors *)
let accept_all string = Some string
let accept_empty_suffix = function
   | _::_ -> None
   | x -> Some x

type awksub_nonterminals =
  | Expr | Term | Lvalue | Incrop | Binop | Num 

let awkish_grammar =
  (Expr, function
     | Expr ->
        [[N Term; N Binop; N Expr];
        [N Term]]
     | Term ->
      [[N Num];
        [N Lvalue];
        [N Incrop; N Lvalue];
        [N Lvalue; N Incrop];
        [T"("; N Expr; T")"]]
     | Lvalue ->
	      [[T"$"; N Expr]]
     | Incrop ->
        [[T"++"];
          [T"--"]]
     | Binop ->
        [[T"+"];
          [T"-"]]
     | Num ->
        [[T"0"]; [T"1"]; [T"2"]; [T"3"]; [T"4"];
          [T"5"]; [T"6"]; [T"7"]; [T"8"]; [T"9"]])

(* sample test cases with awkish_grammar *)
let test0 = ((make_matcher awkish_grammar accept_all ["(";"$";"9";"++";")"]) = Some [])
let test1 = ((make_matcher awkish_grammar accept_all ["(";"$";"9";"++";")";"+";"5";")";"-"]) = Some [")"; "-"])
let test2 = ((make_matcher awkish_grammar accept_all ["$";"9";"++"]) = Some ["++"])
let test3 = ((make_matcher awkish_grammar accept_empty_suffix ["$";"9";"++"]) = Some [])
let test4 = ((make_matcher awkish_grammar accept_empty_suffix ["$";"9";"++";")"]) = None)
let test5 = ((make_matcher awkish_grammar accept_all ["$";"9";"++";")"]) = Some ["++";")"])
let test6 = ((make_matcher awkish_grammar accept_all ["++";"$";"9"]) = Some [])
let test7 = ((make_matcher awkish_grammar accept_all ["(";"++";"$";"9"]) = None)
let test8 = ((make_matcher awkish_grammar accept_all ["(";"++";"$";"9";")";")"]) = Some [")"])
let test9 = ((make_matcher awkish_grammar accept_all ["(";"$";"(";"++";"$";"9";")";")";")"]) = Some [")"])
let test10 = ((make_matcher awkish_grammar accept_empty_suffix ["(";"$";"(";"++";"$";"9";")";")";")"]) = None)
let test11 = ((make_matcher awkish_grammar accept_empty_suffix ["$";"(";"$";"(";"++";"$";"9";")";")";")"]) = None)
let test12 = ((make_matcher awkish_grammar accept_empty_suffix ["$";"(";"$";"(";"++";"$";"9";")";")";]) = Some [])

type test_nonterminals = | People | Joanna | Janis | Ellie | Food | Verb | Past

let my_grammar =
  (People, function
     | People ->
        [[N Joanna; N Janis];
        [N Joanna]]
     | Joanna ->
      [[N Verb; N Food];
        [N Food; N Verb];
        [N Verb];
        [T"#"; N Joanna; T"!"]]
     | Janis ->
     [[N Verb];
      [N Verb; T "Tea"];
      [N Food; N Ellie];
      [T"#"; N Janis];
      [T"{"; N People;T"}"]]
     | Food ->
	      [[T"Pudding"];[T"Boba"];[T"Mango";T"Ice"]]
     | Verb ->
        [[T"Eat"];[T"Likes"];[N Past]]
     | Ellie ->
        [[T"Bird"];[T"Makar"]]
     | Past ->
        [[T"ate"];[T"liked"]])

let test13 = ((make_matcher my_grammar accept_all ["#";"ate";"Pudding";"!";"#";"Mango";"Ice";"Bird"]) = Some [])
let test14 = ((make_matcher my_grammar accept_all ["#";"ate";"Pudding";"!";]) = Some [])
let test15 = ((make_matcher my_grammar accept_all ["#";"ate";"Pudding";"!";"{";"#";"Boba";"Likes";"!";"#";"Mango";"Ice";"Bird";"}"]) = Some [])
let test16 = ((make_matcher my_grammar accept_all ["{";"#";"ate";"Pudding";"!";"{";"#";"Boba";"Likes";"!";"#";"Mango";"Ice";"Bird";"}"]) = None)
let make_matcher_test = ((make_matcher my_grammar accept_all ["ate";"{";"#";"ate";"Pudding";"!";"{";"#";"Boba";"Likes";"!";"#";"Mango";"Ice";"Bird";"}";"}";"{";"p"]) = Some ["{";"p"])

let test17 = ((make_parser my_grammar ["#";"ate";"Pudding";"!";]) = Some(Node (People,
                                                                        [Node (Joanna,
                                                                           [Leaf "#";
                                                                           Node (Joanna,
                                                                              [Node (Verb, [Node (Past, [Leaf "ate"])]);
                                                                              Node (Food, [Leaf "Pudding"])]);
                                                                           Leaf "!"])])))
let test18 = ((make_parser my_grammar ["ate";"{";"#";"ate";"Pudding";"!";"{";"#";"Boba";"Likes";"!";"#";"Mango";"Ice";"Bird";"}";"}";"{";"p"]) = None)
let test19 = ((make_parser my_grammar ["{";"#";"ate";"Pudding";"!";"{";"#";"Boba";"Likes";"!";"#";"Mango";"Ice";"Bird";"}"]) = None)
let text20 = ((make_parser my_grammar ["#";"ate";"Pudding";"!";"{";"#";"Boba";"Likes";"!";"#";"Mango";"Ice";"Bird";"}"]) = Some (Node (People,
                                                         [Node (Joanna,
                                                            [Leaf "#";
                                                            Node (Joanna,
                                                               [Node (Verb, [Node (Past, [Leaf "ate"])]);
                                                               Node (Food, [Leaf "Pudding"])]);
                                                            Leaf "!"]);
                                                            Node (Janis,
                                                            [Leaf "{";
                                                            Node (People,
                                                               [Node (Joanna,
                                                               [Leaf "#";
                                                                  Node (Joanna,
                                                                  [Node (Food, [Leaf "Boba"]); Node (Verb, [Leaf "Likes"])]);
                                                                  Leaf "!"]);
                                                               Node (Janis,
                                                               [Leaf "#";
                                                                  Node (Janis,
                                                                  [Node (Food, [Leaf "Mango"; Leaf "Ice"]);
                                                                  Node (Ellie, [Leaf "Bird"])])])]);
                                                            Leaf "}"])])))
                                                            
let my_frag = ["#";"ate";"Pudding";"!";"{";"#";"Boba";"Likes";"!";"#";"Mango";"Ice";"Bird";"}"]
let make_parser_test =
   match make_parser my_grammar my_frag with
      | Some tree -> parse_tree_leaves tree = my_frag
      | _ -> false