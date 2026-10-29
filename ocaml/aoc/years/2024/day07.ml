
type equation = {
  target : int;
  operands : int list;
}

type operator = (int -> int -> int)

let split (delimiter: char) (input: string) =
  input
  |> String.trim
  |> String.split_on_char delimiter
  |> List.filter (fun x -> x <> "")

let parse_operands (rhs : string) =
  rhs 
    |> split ' '
    |> List.map (fun s -> int_of_string s)

let parse_equation (line : string) : equation =
  match (split ':' line) with
    | [lhs; rhs] -> {
        target = int_of_string lhs;
        operands = parse_operands rhs
      }
    | _ -> failwith ("wrong equation format " ^ line) 

let parse_equations (input : string) =
  let lines = split '\n' input in
  List.map 
    (fun l -> parse_equation l)
    lines

let rec is_possible (acc : int) (target : int) (operands : int list) (operators : operator list) = 
  match operands with 
    | (head :: tail) -> 
      List.fold_left 
        (fun res op -> 
          res || 
          is_possible (op acc head) target tail operators)
        false
        operators
    | [] -> acc == target

let is_possible (eq : equation) (operators : operator list) = 
  
  let head = List.hd eq.operands in
  let operands = List.tl eq.operands in

  let rec is_possible (acc : int) (operands : int list) (operators : operator list) = 
    match operands with 
      | (head :: tail) -> 
        List.fold_left 
          (fun res op -> 
            res || 
            is_possible (op acc head) tail operators)
          false
          operators
      | [] -> acc == eq.target
  in

  is_possible head operands operators

let total_calibration_result (equations : equation list) (operators : operator list) =
  List.fold_left
    (fun res eq -> res + (if is_possible eq operators then eq.target else 0))
    0
    equations

let digits (integer : int) : int list =
  let rec digits (remaining : int) (reversed_digits : int list) =
    if remaining == 0 then List.rev reversed_digits 
    else digits (remaining / 10) ((remaining mod 10) :: reversed_digits)
  in
  digits integer []

let rec pow10 n =
  if n < 0 then invalid_arg "pow10: negative exponent"
  else if n = 0 then 1
  else 10 * pow10 (n - 1)
let concatenate left right = 
  let offset = List.length (digits right) in
  (left * (pow10 offset)) + right

let part1 (input : string) =
  let equations = parse_equations input in
  let operators = [( + ) ; ( * )] in
  total_calibration_result equations operators

let part2 (input : string) = 
  let equations = parse_equations input in
  let operators : operator list = [( + ) ; ( * ) ; concatenate] in
  total_calibration_result equations operators