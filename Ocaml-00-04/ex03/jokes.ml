let read_lines (filename : string) : string array =
    let in_chan = open_in filename in
    let lines = ref [] 
    in
    begin
        try
            while true do
                let line = input_line in_chan
                in
                if String.length line > 0 then
                    lines := line :: !lines
            done
        with End_of_file -> close_in in_chan
    end;
    Array.of_list (List.rev !lines)

let () =
    if Array.length Sys.argv <> 2 then
        exit 1
    else (
        let jokes = read_lines Sys.argv.(1)
        in
        if Array.length jokes = 0 then
            exit 1
        else
            begin
                Random.self_init ();
                let ind = Random.int (Array.length jokes)
                in
                print_string jokes.(ind);
                print_newline ()
            end
    )