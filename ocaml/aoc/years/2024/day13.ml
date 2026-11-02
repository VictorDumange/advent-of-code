open Tools

type button_type = A | B

type button = {
  x : int;
  y : int;
}

type prize = {
  x : int;
  y : int;
}

type claw_machine = {
  button_a : button;
  button_b : button;
  prize : prize;
}

let string_of_button button_type =
  match button_type with
    | A -> "A"
    | B -> "B"

let make_button x y : button =
  {x = x; y = y}

let parse_operand operand delimiter =
  match Parsing.split delimiter operand with
    | [_ ; rhs] -> int_of_string rhs
    | _ -> failwith("invalid translation " ^ operand )

let parse_x_y input delimiter =
  match Parsing.split "," input with
    | [lhs; rhs] -> (parse_operand lhs delimiter, parse_operand rhs delimiter)
    | _ -> failwith ("invalid button" ^ input)

let parse_button button_type input =
  let delimiter = 
    "Button " ^ 
    string_of_button button_type ^
    ":"
  in

  let marshalled_button = input
    |> Parsing.split delimiter
    |> List.hd  in

  let x, y = parse_x_y marshalled_button "+" in

  make_button x y

let parse_prize input =
  let marshalled_prize = input
    |> Parsing.split "Prize: "  
    |> List.hd 
  in

  let x,y = parse_x_y marshalled_prize "=" in
  {x = x; y = y}

let parse_claw_machine (input: string) =
  match Parsing.split "\n" input with
  | [left; middle; right] -> 
    {
      button_a = parse_button A left;
      button_b = parse_button B middle;
      prize = parse_prize right;
    }
  | _ -> failwith ("invalid input " ^ input)

let parse_machines input =
  input
    |> Parsing.split "\n\n"
    |> List.map parse_claw_machine

type linear_equation_system = {
  a1 : int;
  b1 : int; 
  
  a2: int;
  b2: int;

  c1 : int;
  c2 : int;
}

let determinant (eq: linear_equation_system) =
  eq.a1 * eq.b2 - eq.a2 * eq.b1

let solve (eq: linear_equation_system) =
  let d = determinant eq in
  if d != 0 then
    (*R2 case*)
    let xd = eq.c1 * eq.b2 - eq.c2 * eq.b1 in
    let yd = eq.a1 * eq.c2 - eq.a2 * eq.c1 in

    if ((xd mod d = 0) && (yd mod d = 0)) then
      Some((xd / d, yd /d))
    else 
      None 
  else
    (*
    * Otherwise, we would have to solve a Diophantine equation 
    * in N² (and find the best solution among all the tuples),
    * but for some reason, the directions of the buttons are 
    * never parallel; so solving the problem in R² seems to be 
    * enough… and it’s blazingly fast.
    *)
    failwith("diophantine not implemented")


let parse_eq (machine: claw_machine) =
  {
    a1 = machine.button_a.x;
    b1 = machine.button_b.x;

    a2 = machine.button_a.y;
    b2 = machine.button_b.y;

    c1 = machine.prize.x;
    c2 = machine.prize.y;
  }

let nb_tokens machines = 
  (*adapt data model to equation model*)
  machines 
    |> List.map parse_eq
    |> List.fold_left
        (fun tokens eq -> tokens +
          match solve eq with
            | Some (x, y) -> x * 3 + y
            | None -> 0)
        0
  
let update_prize (machine : claw_machine) =
  let prize = machine.prize in
  let updated_prize: prize = {x = prize.x + 10000000000000; y = prize.y + 10000000000000} in
  {machine with prize = updated_prize}

let part1 (input: string) =
  let machines = parse_machines input in
  nb_tokens machines

let part2 (input: string) =
  let machines = input
    |> parse_machines
    |> List.map update_prize
  in
  nb_tokens machines