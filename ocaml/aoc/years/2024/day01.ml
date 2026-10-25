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

let parse_input input =
  let rows =
    String.split_on_char '\n' input
    |> List.filter (fun line -> String.trim line <> "")
    |> List.map parse_line
  in
  transpose rows

let distance left right =
  let sorted_left = List.sort compare left in
  let sorted_right = List.sort compare right in
  zip sorted_left sorted_right
    |> List.map (fun (a, b) -> abs(a -b))
    |> List.fold_left (+) 0

let solve input similarity = 
  let left, right = parse_input input in
  similarity left right

module IntMap = Map.Make(Int)

let count_occurrences xs = 
  List.fold_left
    (fun counts x ->
      let previous =
        match IntMap.find_opt x counts with 
          | Some n -> n
          | None -> 0
      in
      IntMap.add x (previous + 1) counts)
    IntMap.empty
    xs

let similarity_score left right =
  let counts = count_occurrences right in
  let score x =
    match IntMap.find_opt x counts with
    | Some n -> n * x
    | None -> 0
  in
  left
  |> List.map score
  |> List.fold_left (+) 0

let part1 (input : string) =
  solve input distance

let part2 (input: string) =
  solve input similarity_score
