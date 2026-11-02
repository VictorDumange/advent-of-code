open Tools

type button_type = A | B

type button = {
  x : int;
  y : int;
}

type prize = {
  x : int;
  y : int;
}

type claw_machine = {
  button_a : button;
  button_b : button;
  prize : prize;
}

let string_of_button button_type =
  match button_type with
    | A -> "A"
    | B -> "B"

let make_button x y : button =
  {x = x; y = y}

let parse_operand operand delimiter =
  match Parsing.split delimiter operand with
    | [_ ; rhs] -> int_of_string rhs
    | _ -> failwith("invalid translation " ^ operand )

let parse_x_y input delimiter =
  match Parsing.split "," input with
    | [lhs; rhs] -> (parse_operand lhs delimiter, parse_operand rhs delimiter)
    | _ -> failwith ("invalid button" ^ input)

let parse_button button_type input =
  let delimiter = 
    "Button " ^ 
    string_of_button button_type ^
    ":"
  in

  let marshalled_button = input
    |> Parsing.split delimiter
    |> List.hd  in

  let x, y = parse_x_y marshalled_button "+" in

  make_button x y

let parse_prize input =
  let marshalled_prize = input
    |> Parsing.split "Prize: "  
    |> List.hd 
  in

  let x,y = parse_x_y marshalled_prize "=" in
  {x = x; y = y}

let parse_claw_machine (input: string) =
  match Parsing.split "\n" input with
  | [left; middle; right] -> 
    {
      button_a = parse_button A left;
      button_b = parse_button B middle;
      prize = parse_prize right;
    }
  | _ -> failwith ("invalid input " ^ input)

let parse_machines input =
  input
    |> Parsing.split "\n\n"
    |> List.map parse_claw_machine

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

type state = {
  pos : pos;
  tokens : int
}

module State = struct
  type t = state

  let compare a b = 
    match Int.compare a.tokens b.tokens with
    | 0 -> Pos.compare a.pos b.pos
    | c -> c
end

module StateQueue = Pqueue.Make (State)

module Scores = Map.Make (Pos)

let tokens (machine : claw_machine) =

  let next_states (state : state) = 
    [
      {
        pos = 
          {
            x = state.pos.x + machine.button_a.x; 
            y = state.pos.y + machine.button_a.y
          } ; 
        tokens = state.tokens + 3
      };
      {
        pos =
          {
            x = state.pos.x + machine.button_b.x; 
            y = state.pos.y + machine.button_b.y
          };
        tokens = state.tokens + 1
      }
    ] 
  in

  let is_state_valid (state : state) =
    (state.pos.x <= machine.prize.x) &&
    (state.pos.y <= machine.prize.y)
  in

  let rec tokens (pqueue : StateQueue.t) (scores : int Scores.t) =
    match StateQueue.find_min pqueue with
      | None -> scores
      | Some (current_state) ->
        let dequeued = StateQueue.remove_min pqueue in
        let next_states = current_state
          |> next_states
          |> List.filter is_state_valid in
        let updated_queue, updated_scores = List.fold_left
          (fun (queue, it_score) state -> 
            match Scores.find_opt state.pos it_score with 
            | None -> (StateQueue.insert state queue, Scores.add state.pos state.tokens it_score) 
            | Some (registered_score) -> 
                if (state.tokens < registered_score) then
                  (StateQueue.insert state queue, Scores.add state.pos state.tokens it_score)
                else 
                  (queue, it_score)
          )
          (dequeued, scores)
          next_states
        in
        tokens updated_queue updated_scores
  in

  let initial_pos = {x=0; y=0} in
  let pqeue = StateQueue.empty in
  let scores = Scores.empty in
  tokens 
    (StateQueue.insert {pos = initial_pos; tokens = 0} pqeue) 
    (Scores.add initial_pos 0 scores)

let min_token machine =
  let prize_pos : pos = {x = machine.prize.x; y = machine.prize.y} in
  Scores.find_opt prize_pos (tokens machine)

let fewest_tokens (machines : claw_machine list) =
  List.fold_left
    (fun tokens machine -> 
      tokens + match min_token machine with
      | Some (t) -> t
      | None -> 0
    )
    0
    machines

let part1 (input: string) =
  fewest_tokens (parse_machines input)