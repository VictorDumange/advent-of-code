open Tools

type inter_connected_computers = {
  computer_a : string;
  computer_b : string;
  computer_c : string;
}

let to_list inter_connected_computers = 
  [
    inter_connected_computers.computer_a;
    inter_connected_computers.computer_b;
    inter_connected_computers.computer_c
  ]

module InterConnectedComputers = struct
  type t = inter_connected_computers

  let compare a b =
    let sorted_a = a
      |> to_list
      |> List.sort String.compare in
    let sorted_b = b
      |> to_list
      |> List.sort String.compare in

    List.compare String.compare sorted_a sorted_b
end

module ComputerSet = Set.Make (InterConnectedComputers)

module ComputerGraph = Graph.Make (String)

let parse_edges input =
  let lines = Parsing.get_lines input in
  List.fold_left
    (fun edges line -> (match Parsing.split "-" line with 
      | [lhs; rhs] -> (lhs, rhs)
      | _ -> failwith "wrong input") :: edges
    )
    []
    lines

let parse_graph input : ComputerGraph.t =
  let edges = parse_edges input in
  List.fold_left
    (fun graph (x, y) -> graph 
      |> ComputerGraph.add_edge x y 
      |> ComputerGraph.add_edge y x)
    (ComputerGraph.make ())
    edges

let are_connected a b graph =
  let neighbors_a = ComputerGraph.get_neighbors a graph in
  List.exists (fun n -> String.equal n b) neighbors_a


let pairs (computers : string array) =
  let nb_pairs = Array.length computers in
  let res = ref [] in
  for i = 0 to nb_pairs - 1 do 
    let a = Array.get computers i in
    for j = i + 1 to nb_pairs - 1 do 
      let b = Array.get computers j in
      res := (a, b) :: !res
    done 
  done;
  !res

let connected_computer computer graph (inter_connected : ComputerSet.t)=
  let connected = ComputerGraph.get_neighbors computer graph in
  let pairs = pairs (Array.of_list connected) in
  List.fold_left
    (fun inter_connected (a, b) -> 
      if (are_connected a b graph) then 
        ComputerSet.add 
          {computer_a = a; computer_b = b; computer_c = computer} 
          inter_connected
      else inter_connected
    )
    inter_connected
    pairs

let is_concerned inter_connected_computers =
inter_connected_computers
  |> to_list
  |> List.exists (fun computer -> 't' = String.get computer 0)

let part1 input =
  let graph = parse_graph input in

  let inter_connected = List.fold_left
    (fun res computer -> (connected_computer computer graph res))
    ComputerSet.empty
    (ComputerGraph.nodes graph)
  in

  inter_connected 
    |> ComputerSet.elements
    |> List.filter is_concerned
    |> List.length

let all_connected computers graph =
  let nb_computers = Array.length computers in
  let result = ref true in

  for i = 0 to nb_computers - 1 do
    for j = 0 to nb_computers - 1 do
      if i <> j then
        let a = Array.get computers i in
        let b = Array.get computers j in
        if not (are_connected a b graph) then
          result := false
    done
  done;

  !result

let rec combinations k computers : string list list =
  if (k = 0) then [[]]
  else match computers with 
    | [] -> []
    | x :: xs ->
        let with_x = List.map (fun sub -> x :: sub) (combinations (k - 1) xs) in
        let without_x = combinations k xs in
        List.concat [with_x ;without_x]

let largest_set computer graph =
  let neighbors = ComputerGraph.get_neighbors computer graph in
  let initial = [computer; List.hd neighbors] in
  let candidates = (computer :: neighbors) in

  let rec aux largest =
    let largest_size = List.length largest in
    if (largest_size = List.length candidates) then largest
    else 
      let all_comb : string list list = combinations (largest_size + 1) candidates in
      let connected = List.filter 
        (fun comb -> all_connected (Array.of_list comb) graph) 
        all_comb
      in
      if (0 = List.length connected) then largest
      else aux (List.hd connected)
    in
  
  aux initial

let part2 input = 
  let graph = parse_graph input in
  let computers = ComputerGraph.nodes graph in

  List.fold_left
    (fun largest computer -> 
      let candidate = largest_set computer graph in
      if (List.length candidate > List.length largest) then candidate
      else largest
    )
    []
    computers

    |> List.sort String.compare
    |> String.concat ","
