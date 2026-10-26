let split (delimiter: char) (input: string) =
  input
  |> String.trim
  |> String.split_on_char delimiter
  |> List.filter (fun x -> x <> "")

type multiplication = {
  lhs : int;
  rhs : int;
}

let parse_mul (expr: string) : multiplication =
  let size = String.length expr in
  let inner = String.sub expr 4 (size - 5) in
  match split ',' inner with
  | [left; right] ->
      { lhs = int_of_string left; rhs = int_of_string right }
  | _ ->
      failwith ("invalid expression " ^ expr)

let is_digit c =
  c >= '0' && c <= '9'

type state = Init | M | U | L | LeftBracket | LHS | Delimiter | RHS 
let rec match_mul (memory: string) (index: int) (current_state: state) = 
  let current_char = String.get memory index in
  match (current_char, current_state) with
    | ('m', Init) -> match_mul memory (index+1) M
    | ('u', M) -> match_mul memory (index+1) U
    | ('l', U) -> match_mul memory (index+1) L
    | ('(', L) -> match_mul memory (index+1) LeftBracket
    | (_ as c, LeftBracket) -> 
      if is_digit c then match_mul memory (index+1) LHS
      else -1
    | (_ as c, LHS) -> 
      if is_digit c then match_mul memory (index+1) LHS
      else if (c == ',') then match_mul memory (index+1) Delimiter
      else -1
    | (_ as c, Delimiter) ->
      if is_digit c then match_mul memory (index+1) RHS
      else -1
    | (_ as c, RHS) ->
      if is_digit c then match_mul memory (index+1) RHS
      else if (c == ')') then index
      else -1
    | _ -> -1

let get_index (memory: string) =
  List.init (String.length memory) (fun i -> i)

let find_mul_pattern (memory: string) =
  let index = get_index memory in
  index 
    |> List.map (fun nth -> (nth, match_mul memory nth Init))
    |> List.filter (fun (_, end_index) -> end_index != -1)

let solve (memory: string) = 
  let mul_index = find_mul_pattern memory in
  mul_index
    |> List.map (fun (left, right) -> 
      let size = right - left + 1 in
      let pattern = String.sub memory left size in
      parse_mul pattern)
    |> List.map (fun mul -> mul.lhs * mul.rhs)
    |> List.fold_left (+) 0


type statement = DO | DONT

type enabling_acc = {
  enabled : string list;
  current : string;
  state : statement;
}
let make_acc : enabling_acc =
  { enabled = []; current = ""; state = DO }

let is_sub (memory : string) (pattern : string) (nth : int) =
  let memory_size = String.length memory in
  let pattern_size = String.length pattern in
  nth >= 0
  && nth + pattern_size <= memory_size
  && String.sub memory nth pattern_size = pattern

let make_acc : enabling_acc =
  { enabled = []; current = ""; state = DO }

let find_enabling_intervals (memory : string) : string list =
  let index = get_index memory in

  let result =
    List.fold_left
      (fun acc nth ->
        if is_sub memory "don't()" nth then
          match acc.state with
          | DO ->
              { enabled = acc.current :: acc.enabled;
                current = "";
                state = DONT }
          | DONT ->
              acc
        else if is_sub memory "do()" nth then
          { acc with state = DO }
        else
          match acc.state with
          | DO ->
              { acc with current = acc.current ^ String.make 1 memory.[nth] }
          | DONT ->
              acc)
      make_acc
      index
  in

  let enabled =
    match result.state with
    | DO when result.current <> "" ->
        result.current :: result.enabled
    | _ ->
        result.enabled
  in

  List.rev enabled

let part1 (input: string) = 
  solve input

let part2 (input: string) = 
  let enabled_memory = find_enabling_intervals input in
  List.fold_left
      (fun acc chunck -> acc + (solve chunck))
      0
      enabled_memory