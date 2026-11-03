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

module PosSet = Set.Make (Pos)
module PosMap = Map.Make (Pos)
module PosQueue = Fqueue.Make (Pos)

type dir = N | E | S | W

let is_inside pos width =
  (pos.x >= 0 && pos.y >= 0 && pos.x <= width && pos.y <= width)

let is_corrupted pos corrupted =
  PosSet.mem pos corrupted

let offset dir = match dir with
  | N -> (0, -1)
  | E -> (1, 0)
  | S -> (0, 1)
  | W -> (-1, 0)

let translate pos dir =
  let dx, dy = offset dir in
  {x = pos.x + dx; y = pos.y + dy}

let all_directions = [N;E;S;W]

let find_safe pos width corrupted =
  all_directions
    |> List.map (fun d -> translate pos d)
    |> List.filter (fun p -> 
      (is_inside p width) && 
      (not (is_corrupted p corrupted)))

let is_exit pos width =
  pos.x = width && pos.y = width

let rec bfs (queue : PosQueue.t) (visited : int PosMap.t) (corrupted : PosSet.t) (width : int) =
  match PosQueue.dequeue queue with
    | None -> None
    | Some (pos, dequeued) -> 
        let nb_step = PosMap.find pos visited in
        if is_exit pos width then Some(nb_step)
        else 
          let available_positions = find_safe pos width corrupted in
          let updated_queue, updated_visited = List.fold_left
            (fun (q, v) p -> 
              if PosMap.mem p v then (q, v)
              else ((PosQueue.enqueue p q), (PosMap.add p (nb_step + 1) v))
            )
            (dequeued, visited)
            available_positions
          in
          bfs updated_queue updated_visited corrupted width

let rec firstk k xs = match xs with
  | [] -> failwith "firstk"
  | x::xs -> if k=1 then [x] else x::firstk (k-1) xs;;

let parse_corrupted (input : string) =
  input
    |> Parsing.get_lines
    |> List.map (fun line -> 
        match Parsing.split "," line with
          | [lhs; rhs] -> {x = int_of_string lhs; y = int_of_string rhs}
          | _ -> failwith ("invalid line " ^ line))

let set_of_list corrupted =
  List.fold_left 
    (fun set pos -> PosSet.add pos set)
    PosSet.empty
    corrupted

let solve corrupted width nb_bytes =

  let corrupted_set = corrupted
    |> firstk nb_bytes 
    |> set_of_list
  in

  let initial_pos = {x = 0; y = 0} in
  bfs 
    (PosQueue.enqueue initial_pos PosQueue.empty)
    (PosMap.add initial_pos 0 PosMap.empty)
    corrupted_set
    width

let part1 (input : string) (width : int) (nb_bytes : int) =
  
  let corrupted = parse_corrupted input in
  solve corrupted width nb_bytes


let part2 (input : string) (width : int) (nb_bytes : int) =
  
  let corrupted = parse_corrupted input in

  let rec aux (bytes : int) = 
    match solve corrupted width bytes with 
      | None -> List.nth corrupted (bytes - 1)
      | Some _ -> aux (bytes + 1)
  in
  
  aux (nb_bytes + 1)