open Tools

type pos = {
  x : int;
  y : int;
}

type dir = N | E | S | W

type location = {
  pos : pos ;
  dir : dir ;
}

type state = {
  loc : location;
  score : int
}

type rotation = L | R

module Pos = struct
  type t = pos

  let compare a b =
    match Int.compare a.x b.x with
    | 0 -> Int.compare a.y b.y
    | c -> c
end

module Location = struct
  type t = location
  let compare a b = match (Pos.compare a.pos b.pos) with 
    | 0 -> Stdlib.compare a.dir b.dir
    | c -> c

  let translate (l : t) = 
    let move l = 
      let position = l.pos in
      match l.dir with 
        | N -> {x = position.x - 1 ; y = position.y}
        | S -> {x = position.x + 1 ; y = position.y}
        | W -> {x = position.x ; y = position.y - 1}
        | E -> {x = position.x ; y = position.y + 1}
    in
    {pos = move l; dir = l.dir}

  let rotate (l : t) (rot : rotation) =
    let aux (dir : dir) (rot : rotation) =
      match rot with
        | R -> 
          (match dir with 
            | N -> E
            | E -> S
            | S -> W
            | W -> N)
        | L -> 
          (match dir with 
            | N -> W
            | E -> N
            | S -> E
            | W -> S)
    in
    {pos = l.pos; dir = aux l.dir rot}
end

module State = struct 
  type t = state

  let compare a b = 
    match (Int.compare a.score b.score) with
      | 0 -> Location.compare a.loc b.loc
      | c -> c
    
end

module StatePQueue = Pqueue.Make (State)

let find grid c = 
  let rows = Grid.rows grid in
  let cols = Grid.cols grid in

  let s = ref {x = 0; y = 0} in

  for i = 0 to rows - 1 do
    for j = 0 to cols - 1 do 
      if (Grid.get i j grid) = c then
        s := {x = i; y = j}
    done
  done;

  !s

let rotated_states (current_state : state) = 
  let loc = current_state.loc in
  let score = current_state.score in

  let left_rotated = {loc = Location.rotate loc L; score = score+1000} in
  let right_rotated = {loc = Location.rotate loc R; score = score+1000} in

  [left_rotated ; right_rotated]

let is_obstructed pos grid =
  (Grid.get pos.x pos.y grid) == '#'

let is_end pos grid =
  (Grid.get pos.x pos.y grid) == 'E'

let next_states (current_state : state) grid =
  let translated_location = Location.translate current_state.loc in
  if (is_obstructed translated_location.pos grid) then
    rotated_states current_state
  else 
    {loc = translated_location ; score = current_state.score + 1}
    :: rotated_states current_state


module LocMap = Map.Make (Location)

let rec minimum_score grid (pqueue : StatePQueue.t) (scores : int LocMap.t) =
  let mb_current_state = StatePQueue.find_min pqueue in
  match mb_current_state with 
    | None -> scores
    | Some (current_state) ->
      let dequeued = StatePQueue.remove_min pqueue in
      let available_states = next_states current_state grid in
      let updated_pqeue, updated_scores = List.fold_left
        (fun (pq, sc) st -> match LocMap.find_opt st.loc sc with
          | None -> (StatePQueue.insert st pq, LocMap.add st.loc st.score sc)
          | Some (score) -> 
            if (st.score >= score) then
              (pq, sc)
            else 
              (StatePQueue.insert st pq, LocMap.add st.loc st.score sc))
        (dequeued, scores)
        available_states in
      minimum_score grid updated_pqeue updated_scores

let intial_state grid =
  let st = find grid 'S' in
  let init_loc = {pos = st; dir = E} in
  {loc = init_loc ; score = 0}

let part1 (input : string) =
  let grid = Grid.parse input Fun.id in

  let intial_state = intial_state grid in
  let pqueue = StatePQueue.empty in

  let scores = minimum_score 
    grid 
    (StatePQueue.insert intial_state pqueue) 
    (LocMap.add intial_state.loc 0 LocMap.empty) 
  in

  let end_pos = find grid 'E' in
  [N; E; S; W]
  |> List.filter_map (fun dir -> LocMap.find_opt {pos = end_pos; dir} scores)
  |> List.fold_left min max_int



(*state = dir + ^pos*)
(*dijskra is priority q*)
(*available state : *)
(* for each available state, if visited*)