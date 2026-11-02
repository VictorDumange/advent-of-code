open Tools

type ra
type rb
type rc

type register = A | B | C

type literal = Zero | One | Two | Three | Four | Five | Six | Seven

type operand =
  | Register of register
  | Literal of literal

type instruction =
  | Adv of operand
  | Bxl of literal
  | Bst of operand
  | Jnz of literal
  | Bxc
  | Out of operand
  | Bdv of operand
  | Cdv of operand

let literal_of_int = function
  | 0 -> Zero | 1 -> One | 2 -> Two | 3 -> Three 
  | 4 -> Four | 5 -> Five | 6 -> Six | 7 -> Seven
  | n -> invalid_arg (Printf.sprintf "literal_of_int: %d" n)

let int_of_literal = function
  | Zero -> 0 | One -> 1 | Two -> 2 | Three -> 3
  | Four -> 4 | Five -> 5 | Six -> 6 | Seven -> 7

type register_machine = {
  outputs : int list ;
  rip : int ;
  instructions : instruction array ;
  ra : int ;
  rb : int ;
  rc : int ;
}

let is_finished register_machine =
  register_machine.rip >= Array.length register_machine.instructions

let get_instruction register_machine =
  register_machine.instructions.(register_machine.rip)

let set_ip rip register_machine =
  { register_machine with rip = rip }

let increase_ip register_machine =
  let current_rip = register_machine.rip in
  {register_machine with rip = current_rip + 1}

let set_register r v register_machine =
  match r with 
  | A -> { register_machine with ra = v }
  | B -> { register_machine with rb = v }
  | C -> { register_machine with rc = v }

let add_output v register_machine =
  let updated_ouputs = v :: register_machine.outputs in
  { register_machine with outputs = updated_ouputs }

let register_value r register_machine =
  match r with 
  | A -> register_machine.ra
  | B -> register_machine.rb
  | C -> register_machine.rc

let combo (operand : operand) register_machine : int =
  match operand with 
  | Register r -> register_value r register_machine
  | Literal l -> int_of_literal l

let rec pow2 n =
  if n = 0 then 1
  else 2 * (pow2 (n-1))

let execute_adv operand register_machine =
  let a_value = register_value A register_machine in
  let op_value = combo operand register_machine in
  let division = a_value / pow2 op_value in
  set_register A division (increase_ip register_machine)

let execute_bxl operand register_machine =
  let b_value = register_value B register_machine in
  let op_value = int_of_literal operand in
  let xor = b_value lxor op_value in
  set_register B xor (increase_ip register_machine)

let execute_bst operand register_machine =
  let op_value = combo operand register_machine in
  let mod_eight = op_value mod 8 in
  set_register B mod_eight (increase_ip register_machine)

let execute_jnz operand register_machine =
  let a_value = register_value A register_machine in
  if (a_value = 0) then (increase_ip register_machine)
  else set_ip (int_of_literal operand) register_machine

let execute_bxc register_machine =
  let b_value = register_value B register_machine in
  let c_value = register_value C register_machine in
  let xor = b_value lxor c_value in
  set_register B xor (increase_ip register_machine)

let execute_out operand register_machine =
  let combo = combo operand register_machine in
  let output = combo mod 8 in
  add_output output (increase_ip register_machine)

let execute_bdv operand register_machine =
  let a_value = register_value A register_machine in
  let op_value = combo operand register_machine in
  let division = a_value / pow2 op_value in
  set_register B division (increase_ip register_machine)

let execute_cdv operand register_machine = 
  let a_value = register_value A register_machine in
  let op_value = combo operand register_machine in
  let division = a_value / pow2 op_value in
  set_register C division (increase_ip register_machine)

let rec run register_machine =
  if is_finished register_machine then register_machine
  else 
    let instruction = get_instruction register_machine in
    let updated_machine = match instruction with 
      | Adv adv -> execute_adv adv register_machine
      | Bxl bxl -> execute_bxl bxl register_machine
      | Bst bst -> execute_bst bst register_machine
      | Jnz jnz -> execute_jnz jnz register_machine
      | Bxc -> execute_bxc register_machine 
      | Out out -> execute_out out register_machine
      | Bdv bdv -> execute_bdv bdv register_machine
      | Cdv cdv -> execute_cdv cdv register_machine
    in
    run updated_machine

let operand_of_int = function
  | (0 | 1 | 2 | 3) as n -> Literal (literal_of_int n)
  | 4 -> Register A
  | 5 -> Register B
  | 6 -> Register C
  | n -> invalid_arg (Printf.sprintf "operand_of_int: %d" n)

let intruction_of_ints opcode operand = 
  if (opcode == 0) then Adv (operand_of_int operand)
  else if (opcode == 1) then Bxl (literal_of_int operand)
  else if (opcode == 2) then Bst (operand_of_int operand)
  else if (opcode == 3) then Jnz (literal_of_int operand)
  else if (opcode == 4) then Bxc
  else if (opcode == 5) then Out (operand_of_int operand)
  else if (opcode == 6) then Bdv (operand_of_int operand)
  else if (opcode == 7) then Cdv (operand_of_int operand)
  else failwith ("invalid opcode" ^ string_of_int opcode)

let words (input : string) =
  input 
    |> Parsing.split "Program: "
    |> List.hd
    |> Parsing.split ","
    |> List.map int_of_string

let parse_program (input : string) =  
  let words = words input in

  let rec aux wrds = 
    match wrds with 
      | (left :: (right :: rest)) -> 
          ((intruction_of_ints left right) :: (aux rest))
      | _ -> []
  in

  aux words

let register_of_string str : register = 
  match str with 
    | "A" -> A
    | "B" -> B
    | "C" -> C
    | _ -> failwith ("unknown register")

let marshalled_registered (line : string) =
  line 
      |> Parsing.split "Register "
      |> List.hd 

let parse_register (line : string) : (register * int) =
  let marshalled_registered = marshalled_registered line in

  match Parsing.split ": " marshalled_registered with
    | [lhs ; rhs] -> (register_of_string lhs, int_of_string rhs)
    | _ -> failwith ("invalid register : " ^ line)

let boot() : register_machine = 
  {
    outputs = [] ;
    rip = 0;
    instructions = Array.of_list [];
    ra = 0 ;
    rb = 0 ;
    rc = 0 ;
  }
  
let parse_machine (input : string) =
  let lines = Parsing.get_lines input in
  List.fold_left
      (fun machine line -> 
        let register, v = parse_register line in
        set_register register v machine
      )
      (boot())
      lines

let load program machine =
  let instructions = Array.of_list program in
  {machine with instructions = instructions}

let parse_input (input: string) =
  match Parsing.split("\n\n") input with
    | [left; right] -> (parse_machine left, parse_program right)
    | _ -> failwith ("invalid input")

let debug program machine =
  let initialized_machine = load program machine in
  let final_machine = run initialized_machine in
  List.rev final_machine.outputs

let part1 (input: string) =

  let machine, program = parse_input input in
  let outputs = debug program machine in

  outputs
    |> List.map string_of_int
    |> String.concat ","

  



(* load, is_finished, outputs, instruction, set-ip, set-register, get-register *)