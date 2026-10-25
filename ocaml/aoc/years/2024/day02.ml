let split (delimiter: char) (input: string) =
  input
  |> String.trim
  |> String.split_on_char delimiter
  |> List.filter (fun x -> x <> "")

let get_lines (input: string) =
  split '\n' input

let get_report (line: string) =
  List.map
    int_of_string
    (split ' ' line)

let get_reports (input: string) =
  List.map 
    get_report
    (get_lines input)

let rec is_increasingly_safe report =
  match report with
  | left :: (right :: _ as rest) ->
      left < right
      && right - left <= 3
      && is_increasingly_safe rest
  | _ -> true

let is_decreasingly_safe report =
  is_increasingly_safe (List.rev report)

let is_safe_default report =
   is_increasingly_safe report || is_decreasingly_safe report

let rec remove_nth n xs =
  match n, xs with
  | _, [] -> []
  | 0, _ :: rest -> rest
  | n, x :: rest -> x :: remove_nth (n - 1) rest

let rec is_safe_fallback report nth = 
  if (nth == List.length report) then false
  else 
    let corrected_report = remove_nth nth report in
    if (is_safe_default corrected_report) then true
    else is_safe_fallback report (nth + 1)

let is_safe_consolidated report = 
  if is_safe_default report 
    then true
  else is_safe_fallback report 0

let count_safe reports is_safe =
  List.fold_left
    (fun nb_safe report -> if (is_safe report) 
      then nb_safe + 1 
      else  nb_safe)
    0
    reports

let solve input is_safe = 
  let reports = get_reports input in
  count_safe reports is_safe

let part1 (input: string) =
  solve input is_safe_default

let part2 (input: string) =
  solve input is_safe_consolidated


