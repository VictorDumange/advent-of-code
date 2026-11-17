open Tools

let parse_secrets input =
  (Parsing.get_lines input)
    |> List.map int_of_string

let mix a b =
  a lxor b

let prune secret =
  secret mod 16777216

let first_step secret =
  secret * 64

let second_step secret =
  secret / 32

let third_step secret =
  secret * 2048

let next_secret previous_secret process =
  List.fold_left
    (fun secret step -> (step secret)
      |> (fun updated_secret -> mix secret updated_secret)
      |> prune
    )
    previous_secret
    process

let nth_secret n initial_secret process =

  let rec aux i secret =
    if i = n then secret
    else aux (i+1) (next_secret secret process)
  in

  aux 0 initial_secret

let process =
  [first_step; second_step; third_step]

let part1 input =
  let initial_secrets = parse_secrets input in
  List.fold_left
    (fun res secret -> res + nth_secret 2000 secret process)
    0
  initial_secrets