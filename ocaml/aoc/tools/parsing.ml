let split (delimiter : string) (input : string) =

  let delimiter_length = String.length delimiter in
  let preprocessed_input = String.trim input in
  let input_length = String.length preprocessed_input in

  let rec aux (start : int) (i : int) (split_string : string list) (current : string) = 
    
    if i >= input_length then 
      List.rev (current :: split_string)

    else if 
      (i + delimiter_length <= input_length) &&
      (String.equal (String.sub preprocessed_input i delimiter_length)) delimiter then
      aux 
        (i + delimiter_length)
        (i + delimiter_length)  
        (current :: split_string)
        ""
    else 
      let elem = String.get preprocessed_input i in
      aux start (i + 1) split_string (current ^ String.make 1 elem)

  in

  aux 0 0 [] "" |> List.filter (fun x -> x <> "")

let get_lines (input: string) =
  split "\n" input

(* read the entire file *)
let read_file file =
  In_channel.with_open_bin file In_channel.input_all