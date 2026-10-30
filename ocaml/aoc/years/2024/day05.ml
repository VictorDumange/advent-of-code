open Tools

type page = int

type update = int list

module PageSet = Set.Make (Int)
module RuleMap = Map.Make (Int)

type rules = PageSet.t RuleMap.t

type safety_manual = {
  rules : rules;
  updates : update list
}

type rule = {
  before : page;
  after : page
}

let parse_individual_rules (line : string) =
  match Parsing.split "|" line with
    | [lhs; rhs] -> {before = int_of_string lhs; after = int_of_string rhs}
    | _ -> failwith ("invalid order " ^ line)

let add_rule (rules : rules) (individual_rule : rule) =

  let current_afters = match RuleMap.find_opt individual_rule.before rules with 
    | Some after -> after
    | None -> PageSet.empty
  in

  let updated_after = (PageSet.add individual_rule.after current_afters) in

  RuleMap.add individual_rule.before updated_after rules

let parse_rules (input : string) : rules = 
  let lines = Parsing.get_lines input in
  lines 
    |> List.map parse_individual_rules
    |> List.fold_left add_rule RuleMap.empty

let parse_updates (input : string) : update list = 
  let lines = Parsing.get_lines input in
  lines 
    |> List.map (fun l -> Parsing.split "," l |> List.map int_of_string)

let parse_manual (input : string) : safety_manual =
  match Parsing.split "\n\n" input with
    | [lhs ; rhs] -> {rules = parse_rules lhs; updates = parse_updates rhs}
    | _ -> failwith ("invalid input")

let is_correct_order before after (rules : rules) =
  match RuleMap.find_opt before rules with
  | Some afters -> PageSet.mem after afters
  | None -> false

let is_update_correct (update : update) (rules : rules) =
  let rec aux = function
    | [] -> true
    | page :: rest ->
        let violates_rule =
          List.exists
            (fun later_page ->
              match RuleMap.find_opt later_page rules with
              | Some afters -> PageSet.mem page afters
              | None -> false)
            rest
        in
        not violates_rule && aux rest
  in
  aux update

let middle_page (update : update) =
  let size = List.length update in
  List.nth update (size / 2) 


module PageGraph = Tools.Graph.Make (Int)

let make_update_graph (update : update) (rules : rules) =
  let update_set =
    List.fold_left
      (fun set page -> PageSet.add page set)
      PageSet.empty
      update
  in

  List.fold_left
    (fun graph page ->
      let afters =
        match RuleMap.find_opt page rules with
        | Some pages -> pages
        | None -> PageSet.empty
      in

      let neighbors = PageSet.inter afters update_set in

      PageSet.fold
        (fun neighbor g -> PageGraph.add_edge page neighbor g)
        neighbors
        graph)
    (PageGraph.make ())
    update

let correct_update (update : update) (rules : rules) =
  let graph = make_update_graph update rules in
  PageGraph.topological_sort graph

let part1 (input : string) =
  let safety_manual = parse_manual input in
  List.fold_left
    (fun acc update -> acc +
      if is_update_correct update safety_manual.rules then middle_page update
      else 0)
    0
    safety_manual.updates

let part2 (input : string) = 
  let safety_manual = parse_manual input in
  List.fold_left
    (fun acc update -> acc +
      if is_update_correct update safety_manual.rules then 0
      else middle_page (correct_update update safety_manual.rules))
    0
    safety_manual.updates