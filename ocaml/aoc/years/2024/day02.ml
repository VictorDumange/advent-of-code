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

let rec all_increasing report =
  match report with
  | left :: (right :: _ as rest) ->
      left < right && all_increasing rest
  | _ -> true
let all_decreasing report =
  all_increasing (List.rev report)

let are_consecutive_safe left right =
  (abs(right - left) >= 1) && (abs(right - left) <= 3)

let rec all_consecutive_safe report =
  match report with
  | left :: (right :: _ as rest) ->
      (are_consecutive_safe left right) && (all_consecutive_safe rest)
  | _ -> true

let is_safe report =
  (all_increasing report || all_decreasing report) 
  && all_consecutive_safe report

let count_safe reports =
  List.fold_left
    (fun nb_safe report -> if (is_safe report) 
      then nb_safe + 1 
      else  nb_safe)
    0
    reports

let part1 (input: string) =
  let reports = get_reports input in
  count_safe reports
