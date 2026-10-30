module type NODE = sig
  type t
  val compare : t -> t -> int
end

module FQueue = struct
  type 'a t = {
    front : 'a list;
    back : 'a list;
  }

  let empty = { front = []; back = [] }

  let is_empty q =
    q.front = [] && q.back = []

  let enqueue x q =
    { q with back = x :: q.back }

  let normalize q =
    match q.front with
    | [] -> { front = List.rev q.back; back = [] }
    | _ -> q

  let dequeue q =
    let q = normalize q in
    match q.front with
    | [] -> None
    | x :: xs -> Some (x, { q with front = xs })
end

module Make (Node : NODE) = struct
  module NodeSet = Set.Make (Node)
  module NodeMap = Map.Make (Node)

  type node = Node.t

  type t = NodeSet.t NodeMap.t

  let make () =
    NodeMap.empty

  let add_node node graph =
    if NodeMap.mem node graph then
      graph
    else
      NodeMap.add node NodeSet.empty graph

  let add_edge from_node to_node graph =
    let graph = add_node from_node graph in
    let graph = add_node to_node graph in

    let neighbors = NodeMap.find from_node graph in
    let neighbors = NodeSet.add to_node neighbors in

    NodeMap.add from_node neighbors graph

  let get_neighbors node graph =
    match NodeMap.find_opt node graph with
    | None -> []
    | Some neighbors -> NodeSet.elements neighbors

  let nodes graph =
    NodeMap.bindings graph
    |> List.map fst

  let in_degrees graph =
    let degrees =
      List.fold_left
        (fun acc node -> NodeMap.add node 0 acc)
        NodeMap.empty
        (nodes graph)
    in

    List.fold_left
      (fun acc node ->
        let neighbors = get_neighbors node graph in
        List.fold_left
          (fun acc neighbor ->
            let degree = NodeMap.find neighbor acc in
            NodeMap.add neighbor (degree + 1) acc)
          acc
          neighbors)
      degrees
      (nodes graph)

  let topological_sort (graph : t) =
    let in_degrees = in_degrees graph in

    let queue =
      NodeMap.fold
        (fun node degree acc ->
          if degree = 0 then FQueue.enqueue node acc
          else acc)
        in_degrees
        FQueue.empty
    in

    let update in_deg q node =
      let updated_deg = NodeMap.find node in_deg - 1 in
      let updated_deg_map = NodeMap.add node updated_deg in_deg in
      if updated_deg = 0 then
        (updated_deg_map, FQueue.enqueue node q)
      else
        (updated_deg_map, q)
    in

    let rec kahn queue xs deg =
      match FQueue.dequeue queue with
      | None -> List.rev xs
      | Some (node, dequeued) ->
          let updated_xs = node :: xs in
          let neighbors = get_neighbors node graph in
          let updated_deg, updated_queue =
            List.fold_left
              (fun (d, q) neighbor -> update d q neighbor)
              (deg, dequeued)
              neighbors
          in
          kahn updated_queue updated_xs updated_deg
    in

    kahn queue [] in_degrees


end