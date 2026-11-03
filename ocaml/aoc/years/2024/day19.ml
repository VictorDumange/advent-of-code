open Tools

let parse_patterns input =
  Parsing.split "," input
    |> List.map String.trim

let parse_designs input =
  Parsing.get_lines input

let parse_towels input = 
  match Parsing.split "\n\n" input with 
    | [lhs; rhs] -> (parse_patterns lhs, parse_designs rhs)
    | _ -> failwith ("invalid input " ^ input)

let fits design i pattern =
  let design_size = String.length design in
  let pattern_size = String.length pattern in
  if (i + pattern_size > design_size) then 
    false
  else 
    String.equal 
      (String.sub design i pattern_size) 
      pattern

let is_possible design patterns =
  
  let rec aux i = 
    if i = String.length design then true
    else patterns
      |> List.fold_left (fun res p -> 
          res ||
          ((fits design i p) && (aux (i + String.length p)))
        )
        false
  in

  aux 0

module StringMap = Map.Make (String)

(* number of ways to build a design, shared between designs *)
type cache = int StringMap.t

let remove_prefix prefix s =
  let n = String.length prefix in
  String.sub s n (String.length s - n)

let remember design ((count, cache) : int * cache) =
  (count, StringMap.add design count cache)

(* Count the arrangements of [design] and return the updated cache. *)
let rec count_arrangements patterns (cache : cache) design : int * cache =
  if design = "" then (1, cache)
  else
    match StringMap.find_opt design cache with
    | Some count -> (count, cache)
    | None ->
        sum_over_fitting_patterns patterns cache design
        |> remember design

(* Sum the arrangements of what remains after each pattern that fits. *)
and sum_over_fitting_patterns patterns cache design =
  patterns
  |> List.filter (fun prefix -> String.starts_with ~prefix design)
  |> List.fold_left
       (fun (total, cache) pattern ->
         let rest = remove_prefix pattern design in
         let count, cache = count_arrangements patterns cache rest in
         (total + count, cache))
       (0, cache)

(* DFS : stops when a solution is found*)
let part1 input =
  let patterns, designs = parse_towels input in
  List.fold_left
    (fun nb_possibles design ->
      nb_possibles + Bool.to_int (is_possible design patterns)
    )
    0
    designs

(*
* Dynamic Programming with memoization. The code
* has been refactored by an AI agent in order to
* make it more expressive. It is nice to extract
* sum_over_fitting_patterns. 
*)
let part2 input =
  
  let patterns, designs = parse_towels input in
  let total, _ =
    List.fold_left
      (fun (total, cache) design ->
        let count, cache = count_arrangements patterns cache design in
        (total + count, cache))
      (0, StringMap.empty)
      designs
  in
  total
