open Tools

type pos = {
  x : int;
  y : int;
}

module Pos = struct
  type t = pos 

  let compare a b =
    match Int.compare a.x b.x with
      | 0 -> Int.compare a.y b.y 
      | c -> c

end

module PosQueue = Fqueue.Make (Pos)
module PosMap = Map.Make (Pos)

type dir = N | E | S | W

let all_directions = [N ; E ; S ; W]

let offset dir = match dir with
  | N -> (-1, 0)
  | E -> (0, 1)
  | S -> (1, 0)
  | W -> (0, -1)

let translate pos dir =
  let dx, dy = offset dir in
  {x = pos.x + dx; y = pos.y + dy}

 let is_inside pos grid =
    let rows = Grid.rows grid in
    let cols = Grid.cols grid in
    (pos.x >= 0 && pos.y >= 0 
    && pos.x < rows && pos.y < cols)

let is_free pos grid =
  
  let is_obstructed pos grid =
    (Grid.get pos.x pos.y grid) = '#'
  in

  (is_inside pos grid) && not (is_obstructed pos grid)

let neighbors pos grid =
  all_directions
    |> List.map (fun d -> translate pos d)
    |> List.filter (fun p -> is_free p grid)

let find_first c grid =
  let rows = Grid.rows grid in
  let cols = Grid.cols grid in
  let res = ref {x = 0; y = 0} in
  for i = 1 to rows - 2 do
    for j = 1 to cols - 2 do 
      if ((Grid.get i j grid) = c) then
        res := {x = i ; y = j};
    done
  done;
  !res

let find_all c grid =
  let rows = Grid.rows grid in
  let cols = Grid.cols grid in
  let res = ref [] in
  for i = 1 to rows - 2 do
    for j = 1 to cols - 2 do 
      if ((Grid.get i j grid) = c) then
        res := {x = i ; y = j} :: !res;
    done
  done;
  !res

let free_positions grid =
  find_first 'E' grid :: 
  find_first 'S' grid :: 
  (find_all '.' grid)
  |> Array.of_list

let ps_to_end grid =

  let rec bfs (queue : PosQueue.t) (visited : int PosMap.t) =
    match PosQueue.dequeue queue with
      | None -> visited
      | Some (current_pos, dequeued) ->
        let non_visited_neighbors = (neighbors current_pos grid)
          |> List.filter (fun n -> not (PosMap.mem n visited))
        in
        let updated_queue, updated_visited = List.fold_left
          (fun (q, v) p -> 
            let pico = PosMap.find current_pos visited in
            (PosQueue.enqueue p q, PosMap.add p (pico + 1) v))
          (dequeued, visited)
          non_visited_neighbors
        in
        bfs updated_queue updated_visited
  in

  let end_pos = find_first 'E' grid in

  bfs 
    (PosQueue.enqueue end_pos PosQueue.empty)
    (PosMap.add end_pos 0 PosMap.empty)

let manhatan_distance a b =
  abs (a.x - b.x) + abs (a.y - b.y)

(* Count the cheats of length 2..max_cheat saving at least min_saving.
   Each pair of track cells is visited once; the cheat goes from the
   cell farther from the end to the closer one, hence the abs. *)
let solve grid min_saving max_cheat =
  let positions = free_positions grid in
  let pico_to_end = ps_to_end grid in
  let picos = Array.map (fun p -> PosMap.find p pico_to_end) positions in
  let nb_positions = Array.length positions in

  let count = ref 0 in

  for i = 0 to nb_positions - 1 do
    for j = i + 1 to nb_positions - 1 do
      let dist = manhatan_distance positions.(i) positions.(j) in
      if dist >= 2 && dist <= max_cheat then
        let saving = abs (picos.(i) - picos.(j)) - dist in
        if saving >= min_saving then incr count
    done
  done;

  !count

let part1 input threhsold =
  let grid = Grid.parse input Fun.id in
  solve grid threhsold 2

let part2 input threshold =
  let grid = Grid.parse input Fun.id in
  solve grid threshold 20

