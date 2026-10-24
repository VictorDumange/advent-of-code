let parse_line line = 
  match String.split_on_char ' ' 
    (String.trim line)
    |> List.filter (fun s -> s <> "") with
    | [left; right] -> (int_of_string left, int_of_string right)
    | _ -> failwith ("Invalid input line: " ^ line)

let transpose rows =
  let left = List.map fst rows in
  let right = List.map snd rows in
  (left, right)

let zip left right =
  List.map2 (fun a b -> (a, b)) left right

let distance (a, b) =
  abs (a-b)

let solve input = 
  let rows =
    String.split_on_char '\n' input
    |> List.filter (fun line -> String.trim line <> "")
    |> List.map parse_line
  in
  let left, right = transpose rows in
  let sorted_left = List.sort compare left in
  let sorted_right = List.sort compare right in
  zip sorted_left sorted_right
    |> List.map distance
    |> List.fold_left (+) 0

let part1 (input : string) =
  solve input