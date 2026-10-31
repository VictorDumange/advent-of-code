open Tools

type pos = {
  x : int ;
  y : int ;
}

type dir = W | N | E | S

type state = {
  pos : pos;
  dir : dir
}

module Pos = struct
  type t = pos

  let compare a b =
    match Int.compare a.x b.x with
    | 0 -> Int.compare a.y b.y
    | c -> c
end

module State = struct 
  type t = state

  let compare a b =
    if (a.dir == b.dir) then Pos.compare a.pos b.pos
    else 1
  
end

module PosSet = Set.Make (Pos)

module StateSet = Set.Make (State)

let move (state : state) = 
  let position = state.pos in
  match state.dir with 
    | N -> {x = position.x - 1 ; y = position.y}
    | S -> {x = position.x + 1 ; y = position.y}
    | W -> {x = position.x ; y = position.y - 1}
    | E -> {x = position.x ; y = position.y + 1}

let rotate (dir : dir) =
  match dir with 
    | N -> E
    | E -> S
    | S -> W
    | W -> N

let is_exit state grid = 

  let dir = state.dir in
  let pos = state.pos in

  let rows = Tools.Grid.rows grid in
  let cols = Tools.Grid.cols grid in

  match dir with 
    | N -> pos.x == 0
    | E -> pos.y == cols - 1
    | S -> pos.x == rows - 1
    | W -> pos.y == 0

let next_state state grid = 

  let pos = state.pos in
  let dir = state.dir in

  let next_pos = move state in

  (*obstacle -> rotation*)  
  if (Tools.Grid.get next_pos.x next_pos.y grid) == '#' then
    let next_dir = rotate dir in
    {pos = pos ; dir = next_dir}
  
  (*same direction*)
  else 
    {pos = next_pos ; dir = dir}

let count_pos visited = 
  PosSet.fold
      (fun _ acc -> acc + 1)
      visited
      0
let rec visit grid (state: state) (visited_pos : PosSet.t) =
  
  let updated_visited = PosSet.add state.pos visited_pos in

  if is_exit state grid then 
    updated_visited
      
  else
    let next_state = next_state state grid in
    visit grid next_state updated_visited

let initial_state grid =

  let find_start grid =
    let rows = Tools.Grid.rows grid in
    let cols = Tools.Grid.cols grid in

    let res : pos ref = ref {x = 0; y = 0} in

    for i = 0 to rows - 1 do
      for j = 0 to cols - 1 do
        if Tools.Grid.get i j grid = '^' then
          res := {x = i; y = j}
      done
    done;

    !res
  in

  {pos = find_start grid; dir = N}

let visit grid initial_state = 
  visit grid initial_state PosSet.empty

let add_obstruction pos grid =
  Tools.Grid.set pos.x pos.y '#' grid

let remove_obstruction pos grid =
  Tools.Grid.set pos.x pos.y '.' grid

let is_cycle obs_pos grid start =
  let _ = add_obstruction obs_pos grid in

  let rec aux grid state visited_state = 
      if StateSet.mem state visited_state then
      true
    else if is_exit state grid then
      false
    else
      let next_state = next_state state grid in
      let updated_visited = StateSet.add state visited_state in
      aux grid next_state updated_visited
  in

  let res = aux grid start StateSet.empty in
  let _ = remove_obstruction obs_pos grid in
  res

let visit_with_obstruction grid visited intial_state =
  PosSet.fold
    (fun pos acc -> acc + Bool.to_int (is_cycle pos grid intial_state))
    visited
    0

let part1 (input : string) =
  let grid = Tools.Grid.parse input Fun.id in
  let initial_state = initial_state grid in
  count_pos (visit grid initial_state)

let part2 (input : string) =
  let grid = Tools.Grid.parse input Fun.id in
  let intial_state = initial_state grid in
  let visited = visit grid intial_state in
  visit_with_obstruction grid visited intial_state
