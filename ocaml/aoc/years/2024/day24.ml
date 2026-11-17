open Tools

type operator = And | Or | Xor

type wire = 
 | Value of bool
 | Expr of operator * string * string

module WireMap = Map.Make (String)

let xor a b =
  (a || b) && not (a && b)

let evaluate_operator (operator : operator) = 
  match operator with 
    | And -> (&&)
    | Or -> (||)
    | Xor -> xor

let rec evaluate 
  (wire_definitions : wire WireMap.t) 
  (wire_values : bool WireMap.t) 
  (wire_id : string) =
  match WireMap.find_opt wire_id wire_values with
    | Some (v) -> (wire_values, v)
    | None -> match WireMap.find wire_id wire_definitions with
        | Value (b) -> (wire_values, b)
        | Expr (operator, left_operand, right_operand) ->
          let wire_values, lhs = evaluate wire_definitions wire_values left_operand in
          let wire_values, rhs = evaluate wire_definitions wire_values right_operand in
          let res = (evaluate_operator operator) lhs rhs in
          (WireMap.add wire_id res wire_values, res)

let parse_operator input =
  match input with 
    | "AND" -> And
    | "OR" -> Or
    | "XOR" -> Xor
    | _ -> failwith ("invalid operator " ^ input)

let parse_expr input : wire =
  match Parsing.split " " input with 
    | [lhs; m; rhs;] -> Expr ((parse_operator m), lhs, rhs) 
    | _ -> failwith ("invalid expr " ^ input)

let parse_expression line : string * wire =
  match Parsing.split " -> " line with 
    | [expr; id] -> (id, parse_expr expr)
    | _ -> failwith ("invalid line " ^ line)

let parse_value input =
  match Parsing.split ": " input with
    | [lhs; rhs] -> (lhs, (rhs = "1"))
    | _ -> failwith ("invalid value : " ^ input)
    
let parse_values input = 
  let lines = Parsing.get_lines input in
  List.fold_left
    (fun values line -> 
      let id, value = parse_value line in
      WireMap.add id value values
    )
    WireMap.empty
    lines

let parse_expressions input : wire WireMap.t = 
  let lines = Parsing.get_lines input in
  List.fold_left
    (fun expressions line ->
      let id, (_ as exp) = parse_expression line in
      WireMap.add id exp expressions
    )
    WireMap.empty
    lines

let parse_input input = 
  match Parsing.split "\n\n" input  with
  | [lhs; rhs] -> (parse_values lhs, parse_expressions rhs)
  | _ -> failwith "invalid input"

let int_from_z id = 
  int_of_string (String.sub id 1 ((String.length id) - 1))

let compare_z a b =
  let int_a = int_from_z a in
  let int_b = int_from_z b in
  Int.compare int_a int_b

let z_wires wire_definitions =
  WireMap.fold
    (fun id _ zwires -> 
      if String.get id 0 = 'z' then id :: zwires
      else zwires
    ) 
    wire_definitions
    []
  |> List.sort compare_z

let rec pow x n =
  if n = 0 then 1
  else x * pow x (n-1)

let convert_bytes bytes =
  
  let rec aux xs i acc =
    match xs with 
      | [] -> acc
      | head :: tail -> 
          if head then aux tail (i+1) (acc + (pow 2 i))
          else aux tail (i+1) acc
  in

  aux (List.rev bytes) 0 0

let part1 input =
  let wire_values, wire_definitions = parse_input input in
  let z_wires = z_wires wire_definitions in

  let _, bytes = List.fold_left
    (fun (values, z_values) z -> 
      let updated_values, z_value = evaluate wire_definitions values z in
      (updated_values, z_value :: z_values)
    )
    (wire_values, [])
    z_wires
  in

  convert_bytes bytes

