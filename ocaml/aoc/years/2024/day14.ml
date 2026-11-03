open Tools

type pos = {
  x : int;
  y : int;
}

module Pos = struct
  type t = pos
  let compare a b =
    match Int.compare a.x b.x with
      | 0 -> Int.compare a.y b.y
      | c -> c
end

module PosMap = Map.Make (Pos)

type velocity = {
  vx : int;
  vy : int;
}

type robot = {
  pos : pos;
  velocity : velocity
}

let parse_x_y delimiter input = 
  let marshalled = input 
    |> Parsing.split delimiter
    |> List.hd
    |> Parsing.split ","
  in
  match marshalled with 
    | [lhs; rhs] ->  (int_of_string lhs, int_of_string rhs)
    | _ -> failwith ("invalid pos " ^ input)

let parse_pos input =
  let x, y = parse_x_y "p=" input in
  {x = x; y = y}

let parse_velocity input =
  let vx, vy = parse_x_y "v=" input in
  {vx = vx; vy = vy}

let parse_robot line =
  match Parsing.split " " line with 
    | [lhs; rhs] -> {pos = parse_pos lhs; velocity = parse_velocity rhs}
    | _ -> failwith ("invalid robot " ^ line)

let parse_robots input =
  List.map parse_robot (Parsing.get_lines input)

let euclidean_rest a n =
  ((a mod n) + n) mod n

let compute_position robot w h t =
  let x = euclidean_rest (robot.pos.x + robot.velocity.vx * t) w in
  let y = euclidean_rest (robot.pos.y + robot.velocity.vy * t) h in 
  {x; y}

type quadrant = NW | NE | SE | SW

module Quadrant = struct
  type t = quadrant

  let compare a b =
    Stdlib.compare a b
end

module QuadrantMap = Map.Make (Quadrant)
let get_quadrant pos w h =
  let mw = w / 2 in
  let mh = h / 2 in

  if (pos.x < mw && pos.y < mh) then Some (NW)
  else if (pos.x < mw && pos.y > mh) then Some (SW)
  else if (pos.x > mw && pos.y < mh) then Some (NE)
  else if (pos.x > mw && pos.y > mh) then Some (SE)
  else None

type quadrants = pos list QuadrantMap.t

let insert pos quadrants w h =
  let maybe_quad = get_quadrant pos w h in
  match maybe_quad with 
    | Some (q) -> 
        let positions = QuadrantMap.find q quadrants in
        QuadrantMap.add q (pos :: positions) quadrants
    | None -> quadrants

let init_quadrant = 
  QuadrantMap.empty
    |> QuadrantMap.add NW []
    |> QuadrantMap.add NE []
    |> QuadrantMap.add SE []
    |> QuadrantMap.add SW []

let safety_factor (quadrants : quadrants) =
  QuadrantMap.fold
    (fun _ positions factor -> factor * (List.length positions))
    quadrants
    1

let same_pos p1 p2 =
  p1.x = p2.x && p1.y = p2.y

let cycle_length robot w h =
  let rec aux t = 
    let pos = compute_position robot w h t in
    if same_pos pos robot.pos then t
    else 
      aux (t+1)
  in
  aux 1

let rec gcd a b =
  if b = 0 then abs a
  else gcd b (a mod b)

let lcm a b =
  if a = 0 || b = 0 then 0
  else abs (a * b) / (gcd a b)

let lcm liste =
  match liste with
  | [] -> 1  (* L'élément neutre du PPCM est 1 *)
  | x :: xs -> List.fold_left lcm x xs

let nb_frames (robots : robot list) w h =
  let cycles = robots
    |> List.map (fun r -> cycle_length r w h) 
  in
  lcm cycles

let positions robots w h t = 
  robots
    |> List.map (fun r -> compute_position r w h t)
    |> List.fold_left 
      (fun mp pos -> 
        match PosMap.find_opt pos mp with
          | Some (frequency) -> PosMap.add pos (frequency + 1) mp
          | None -> PosMap.add pos 1 mp
      )
      PosMap.empty

let is_neighbors pos positions : bool = 
  if (PosMap.mem {x = pos.x - 1 ; y = pos.y } positions) then true
  else if (PosMap.mem {x = pos.x - 1 ; y = pos.y - 1 } positions) then true
  else if (PosMap.mem {x = pos.x ; y = pos.y - 1 } positions) then true
  else if (PosMap.mem {x = pos.x + 1 ; y = pos.y - 1 } positions) then true
  else if (PosMap.mem {x = pos.x + 1 ; y = pos.y} positions) then true
  else if (PosMap.mem {x = pos.x + 1 ; y = pos.y + 1 } positions) then true
  else if (PosMap.mem {x = pos.x ; y = pos.y + 1 } positions) then true
  else if (PosMap.mem {x = pos.x - 1 ; y = pos.y + 1 } positions) then true
  else false

let score positions =
  PosMap.fold
    (fun pos _ score -> score + Bool.to_int (is_neighbors pos positions))
    positions
    0

let marshall_row positions row w =

  let rec aux col acc =
    if col = w then acc
    else 
      let frequency = match PosMap.find_opt {x = col; y = row} positions with
        | Some (f) -> f
        | None -> 0
      in
      let elem = 
        if frequency = 0 then "."
        else string_of_int frequency
      in
      aux (col + 1) (acc ^ elem)
  in

  aux 0 ""

type frame = {
  positions : int PosMap.t;
  score : int;
  seconds : int;
}

let marshall_frame (frame : frame) w h =
  
  let map = (List.init h Fun.id)
    |> List.map (fun row -> marshall_row frame.positions row w)
    |> String.concat "\n"
  in

  "Map at " 
  ^ string_of_int frame.seconds
  ^ " seconds, "
  ^ "score: "
  ^ string_of_int frame.score
  ^ "\n\n"
  ^ map

let build_frame robots w h t =
  let positions = positions robots w h t in
  let score = score positions in
  {positions = positions; score = score; seconds = t}

let rec firstk k xs = match xs with
| [] -> failwith "firstk"
| x::xs -> if k=1 then [x] else x::firstk (k-1) xs;;

let all_frames robots w h =

  let nb_frames = (nb_frames robots w h) in

  let rec aux t acc = 
    if (t = nb_frames) then acc
    else aux (t+1) ((build_frame robots w h t) :: acc)
  in

  List.sort (fun a b -> (Int.compare b.score a.score)) (aux 0 []) 

let serialize_best (frames : frame list) w h k =
  frames
    |> firstk k
    |> List.map (fun frame -> marshall_frame frame w h)
    |> String.concat "\n\n\n"

let part1 (input : string) w h t =
  let robots = parse_robots input in
  let quadrants = robots
    |> List.map (fun r -> compute_position r w h t)
    |> List.fold_left (fun q p -> insert p q w h) init_quadrant
  in
  safety_factor quadrants

let part2 (input : string) w h filename =
  (* 
  * Part 2 was a little strange, because the “Christmas Three”
  * was not defined. This problem was therefore solved using 
  * the “Print-Debug” method.
  *
  * Since each robot moves in cycles, there are only a limited
  * number of different frames. For each frame, a score is 
  * calculated. Only the top 10 frames are serialized and saved
  * to a file.
  *
  * The score corresponds to the number of robots that have at 
  * least one other robot in their neighborhood. The image with
  * the highest score did indeed contain the “Christmas Three”!
  *)
  let robots = parse_robots input in
  let frames = all_frames robots w h in
  let serialized_maps = serialize_best frames w h 10 in
  Parsing.save_string filename serialized_maps;
