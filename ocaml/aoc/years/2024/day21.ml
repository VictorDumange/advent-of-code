open Tools

type action = Idle | Push

type directional_keypad = A | N | E | S | W

type numeric_keypad = Validate | Zero | One | Two | Three | Four | Five | Six | Seven | Eight | Nine

let digit_of_nk nk : int = match nk with
  | Zero -> 0 | One -> 1 | Two -> 2 | Three -> 3 | Four -> 4
  | Five -> 5 | Six -> 6 | Seven -> 7 | Eight -> 8 | Nine -> 9
  | Validate -> failwith "digit cannot be extracted from Validate"

let nk_of_digit d : numeric_keypad = match d with 
  | 0 -> Zero
  | 1 -> One
  | 2 -> Two
  | 3 -> Three
  | 4 -> Four
  | 5 -> Five
  | 6 -> Six
  | 7 -> Seven
  | 8 -> Eight
  | 9 -> Nine
  | _ -> failwith ("cannot construct numerical keyboard touch with " ^ (string_of_int d))

let string_of_nk nk : string = match nk with
  | Validate -> "A"
  | _ as other -> string_of_int (digit_of_nk other)

let move_north nk =
  match nk with 
    | Validate -> Some (Idle, Three)
    | Zero -> Some (Idle, Two)
    | _ as other -> 
        let dg = digit_of_nk other in
        if (dg = 7 || dg = 8 || dg = 9) then None
        else Some (Idle, nk_of_digit (dg + 3))

let move_east nk =
  match nk with
    | Validate -> None
    | Zero -> Some (Idle, Validate)
    | _ as other -> 
      let dg = digit_of_nk other in
      if (dg = 9 || dg = 6 || dg = 3) then None
      else Some (Idle, nk_of_digit (dg + 1))

let move_west nk =
  match nk with
    | Validate -> Some (Idle, Zero)
    | Zero -> None
    | _ as other -> 
      let dg = digit_of_nk other in
      if (dg = 7 || dg = 4 || dg = 1) then None
      else Some (Idle, nk_of_digit (dg - 1))

let move_south nk = 
  match nk with 
    | Validate -> None
    | Zero -> None
    | One -> None
    | Two -> Some (Idle, Zero)
    | Three -> Some (Idle, Validate)
    | _ as other -> 
      let dg = digit_of_nk other in
      Some (Idle, nk_of_digit (dg - 3))

let push_directional (dk_a : directional_keypad) (dk_b : directional_keypad) =
  match (dk_a, dk_b) with 
    | (_ as st, A) -> Some (Push, st)
    | (A, W) -> Some (Idle, N)
    | (A, S) -> Some (Idle, E)
    | (N, E) -> Some (Idle, A)
    | (N, S) -> Some (Idle, S)
    | (S, N) -> Some (Idle, N)
    | (S, E) -> Some (Idle, E)
    | (S, W) -> Some (Idle, W)
    | (E, N) -> Some (Idle, A)
    | (E, W) -> Some (Idle, S)
    | (W, E) -> Some (Idle, S)
    | _ -> None

let push_numerical (nk : numeric_keypad) (dk : directional_keypad) =
  match (nk, dk) with
    | (_ as st, A) -> Some (Push, st)
    | (_ as st, W) -> move_west st
    | (_ as st, N) -> move_north st
    | (_ as st, E) -> move_east st
    | (_ as st, S) -> move_south st

type state = {
  robots : directional_keypad list;
  nk : numeric_keypad;

  sequence : directional_keypad list;
  typed_code : string
}

let push (manual_dir : directional_keypad) (robots : directional_keypad list) =
  List.fold_left
    (fun state robot -> match state with 
        | None -> None
        | Some (Idle, (_ as d) , updated_robots) -> Some(Idle, d , robot :: updated_robots)
        | Some (Push, (_ as button), updated_robots) ->
          match push_directional robot button with 
            | None -> None
            | Some ((_ as act), (_ as button)) -> Some (act, button, button :: updated_robots)
    )
    (Some (Push, manual_dir, []))
    robots

let push (manual_dir : directional_keypad) (state : state) =

  match push manual_dir state.robots with 
    | None -> None
    | Some (Idle, _,  (_ as robots)) -> Some {state with 
        robots = List.rev robots; 
        sequence = manual_dir :: state.sequence
      }
    | Some (Push, (_ as dir), (_ as robots)) ->
      match push_numerical state.nk dir with 
        | None -> None 
        | Some (Idle, nk) -> Some {state with
          robots = List.rev robots;
          sequence = manual_dir :: state.sequence;
          nk = nk
        }
        | Some (Push, nk) -> Some {
          robots = List.rev robots;
          sequence = manual_dir :: state.sequence;
          nk = nk;
          typed_code = state.typed_code ^ (string_of_nk nk)
        }


module State = struct
  type t = state

  let compare (a : t) (b : t) =

    match Stdlib.compare a.nk b.nk with
      | 0 -> (match List.compare Stdlib.compare a.robots b.robots with 
        | 0 -> String.compare a.typed_code b.typed_code
        | c -> c)
      | c -> c
      
end

module StateQueue = Fqueue.Make (State)

module StateSet = Set.Make (State)

let all_touches : directional_keypad list = [A; N; W; E; S]

let initial_state n : state = 
  let robots : directional_keypad list = 
    List.init n (fun _ ->  A) in
  {robots = robots; nk = Validate; sequence = []; typed_code = ""}

let shortest_sequence target_code nb_robot =

  let rec bfs queue visited = 
    match StateQueue.dequeue queue with 
      | None -> failwith ("did not succeed to type code")
      | Some (current_state, dequeued) ->
          let current_code = current_state.typed_code in
          if (String.equal target_code current_code) then
            List.rev (current_state.sequence)
          else if (not (String.equal (current_code) (String.sub target_code 0 (String.length current_code)))) then
            bfs dequeued visited
          else 
            let next_states = all_touches 
              |> List.filter_map (fun touch -> push touch current_state)
              |> List.filter (fun state -> not (StateSet.mem state visited))
            in
            let updated_queue, updated_visited = List.fold_left
              (fun (q, v) s -> (StateQueue.enqueue s q, StateSet.add s v))
              (dequeued, visited)
              next_states
            in
            bfs updated_queue updated_visited
  in

  let initial_state = initial_state nb_robot in

  bfs
    (StateQueue.enqueue initial_state StateQueue.empty)
    (StateSet.add initial_state StateSet.empty)

let numeric_part code =
  let size = String.length code in
  int_of_string (String.sub code 0 (size - 1))

let complexity target_code nb_robot =
  let shortest_sequence = shortest_sequence target_code nb_robot in
  let numeric_part = numeric_part target_code in
  (List.length shortest_sequence) * numeric_part

let solve codes nb_robots =
  List.fold_left 
    (fun compl code -> compl + complexity code nb_robots)
    0
  codes

let part1 input =
  let codes = Parsing.get_lines input in
  solve codes 2

let part2 input =
  let codes = Parsing.get_lines input in
  solve codes 25
