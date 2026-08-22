let example_of_line (line : string) : float array * string =
    let fields = String.split_on_char ',' line in
    let num_fields = List.length fields in
    let arr_fields = Array.make (num_fields - 1) 0.0
    in
    List.iteri
      (fun ind value -> if ind < num_fields - 1 then arr_fields.(ind) <- float_of_string value)
      fields;
    let cls = List.nth fields (num_fields - 1) 
    in
    (arr_fields, cls)

let examples_of_file (filename : string) : (float array * string) list =
    let in_chan = open_in filename in
    let examples = ref []
    in
    begin
        try
            while true do
                let line = input_line in_chan
                in
                if String.length line > 0 then
                    examples := (example_of_line line) :: !examples
            done
        with End_of_file -> close_in in_chan
    end;
    List.rev !examples

let () =
  let examples = examples_of_file "ionosphere.test.csv" in
  Printf.printf "Loaded %d examples\n" (List.length examples);
  List.iter
    (fun (vec, cls) ->
       Array.iter (fun f -> Printf.printf "%f " f) vec;
       Printf.printf "-> %s\n" cls)
    examples