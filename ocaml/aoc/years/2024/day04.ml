open Tools

let is_same xs ref = 
  String.of_seq (List.to_seq xs) = ref

let count_xmas i j grid =
  let searches =
    List.map
      (fun dir -> Grid.get_line i j dir 4 grid)
      Grid.all_directions
  in

  List.fold_left
    (fun acc maybe_xs ->
      match maybe_xs with
      | None -> acc
      | Some xs -> acc + Bool.to_int (is_same xs "XMAS"))
    0
    searches

let is_x_mas i j grid =

  let nw_diag = Grid.get_line (i - 1) (j - 1) SE 3 grid in
  let ne_diag = Grid.get_line (i - 1) (j + 1) SW 3 grid in

  match nw_diag, ne_diag with
  | (Some nw, Some ne) ->
      (is_same nw "MAS" || is_same nw "SAM") &&
      (is_same ne "MAS" || is_same ne "SAM")
  | _ -> false


let part1 (input : string) =
  let grid = Grid.parse input Fun.id in
  let rows = Grid.rows grid in
  let cols = Grid.cols grid in
  let res = ref 0 in

  for i = 0 to rows - 1 do
    for j = 0 to cols - 1 do
      res := !res + count_xmas i j grid
    done
  done;

  !res

let part2 (input : string) =
  let grid = Grid.parse input Fun.id in
  let rows = Grid.rows grid in
  let cols = Grid.cols grid in
  let res = ref 0 in

  for i = 1 to rows - 2 do
    for j = 1 to cols - 2 do
      res := !res + Bool.to_int (is_x_mas i j grid) 
    done
  done;

  !res
