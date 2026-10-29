
let split (delimiter: char) (input: string) =
  input
  |> String.trim
  |> String.split_on_char delimiter
  |> List.filter (fun x -> x <> "")

let parse_stones (input : string) =
  input
    |> split ' '
    |> List.map int_of_string

let is_even n =
  (n mod 2) == 0

let rec shift_right base n =
  if n == 0 then base
  else shift_right (base / 10) (n - 1)

let rec pow10 n =
  if n < 0 then invalid_arg "pow10: negative exponent"
  else if n = 0 then 1
  else 10 * pow10 (n - 1)

let rec digit_count n =
  if n < 10 then 1
  else 1 + digit_count (n / 10)

let concatenate left right = 
  let offset = digit_count right in
  (left * (pow10 offset)) + right

let shift_left base n =
  let rec aux remaining acc i = 
    if (i == n) then acc
    else aux (remaining / 10) (acc + (remaining mod 10) * pow10 i) (i + 1)
  in
  aux base 0 0

let apply_rule stone =
  if stone == 0 then [1]
  else 
    let nb_digits = digit_count stone in
    if is_even nb_digits then 
      let half = nb_digits / 2 in
      [shift_right stone half; shift_left stone half]
    else
      [stone * 2024]

module IntMap = Map.Make(Int)

type stone_counts = int IntMap.t

let init_stone_frequencies (stones : int list) =
  List.fold_left
    (fun stone_map stone -> match IntMap.find_opt stone stone_map with 
      | Some (frequency) -> IntMap.add stone (frequency + 1) stone_map
      | None -> IntMap.add stone 1 stone_map
    )
    IntMap.empty
    stones

let update_counts stones initial_count counts =
  List.fold_left
    (fun acc stone ->
      match IntMap.find_opt stone acc with
      | Some c -> IntMap.add stone (c + initial_count) acc
      | None -> IntMap.add stone initial_count acc)
    counts
    stones

let blink (stone_counts : stone_counts) =
  IntMap.fold
    (fun stone count acc ->
      let stones = apply_rule stone in
      update_counts stones count acc)
    stone_counts
    IntMap.empty


let total (counts : stone_counts) =
  IntMap.fold
    (fun _ count acc -> acc + count)
    counts
    0

let count_stones (initial_counts : stone_counts) (nb_blink : int) =
  
  let rec aux (counts : stone_counts) i =
    if i == nb_blink then counts
    else aux (blink counts) (i + 1)
  in

  let res = aux initial_counts 0 in
  total res

let solve (input : string) (nb_blink : int) =
  let stones = parse_stones input in
  let initial_counts = init_stone_frequencies stones in
  count_stones initial_counts nb_blink

