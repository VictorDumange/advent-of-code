type dir = N | E | S | W | NW | NE | SE | SW

let all_directions = [N; NE; E; SE; S; SW; W; NW]

type 'a t = {
  rows : int;
  cols : int;
  data : 'a array;
}

let parse input f =
  let lines = Parsing.get_lines input in
  match lines with
  | [] -> invalid_arg "Grid.parse: empty input"
  | first :: _ ->
      let rows = List.length lines in
      let cols = String.length first in

      if not (List.for_all (fun line -> String.length line = cols) lines) then
        invalid_arg "Grid.parse: inconsistent row lengths";

      let data =
        lines
        |> String.concat ""
        |> String.to_seq
        |> Seq.map f
        |> Array.of_seq
      in

      { rows; cols; data }

let make rows cols initial =
  if rows < 0 || cols < 0 then
    invalid_arg "Grid.make: negative dimensions";

  { rows; cols; data = Array.make (rows * cols) initial }

let rows grid = grid.rows
let cols grid = grid.cols

let get i j grid =
  if i < 0 || i >= grid.rows ||
     j < 0 || j >= grid.cols then
    invalid_arg "Grid.get: index out of bounds";

  grid.data.(i * grid.cols + j)

let set i j value grid =
  if i < 0 || i >= grid.rows ||
     j < 0 || j >= grid.cols then
    invalid_arg "Grid.set: index out of bounds";

  grid.data.(i * grid.cols + j) <- value

let direction_offset = function
  | N  -> (-1, 0)
  | S  -> (1, 0)
  | W  -> (0, -1)
  | E  -> (0, 1)
  | NW -> (-1, -1)
  | NE -> (-1, 1)
  | SE -> (1, 1)
  | SW -> (1, -1)

let get_line i j d length grid =
  let di, dj = direction_offset d in

  let rec collect k acc =
    if k = length then
      Some (List.rev acc)
    else
      let ni = i + k * di in
      let nj = j + k * dj in

      if ni < 0 || ni >= grid.rows ||
         nj < 0 || nj >= grid.cols then
        None
      else
        collect (k + 1) (get ni nj grid :: acc)
  in

  if length < 0 then
    invalid_arg "Grid.get_line: negative length"
  else
    collect 0 []