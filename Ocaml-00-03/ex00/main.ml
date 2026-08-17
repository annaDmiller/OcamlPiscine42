let rec print_all_colors (colors : Color.t list) =
    match colors with
        | [] -> ()
        | value :: rest -> (
            print_string (Color.toString value);
            print_string " -> ";
            print_endline (Color.toStringVerbose value);
            print_all_colors rest
        )

let () =
    print_endline "Color.all values";
    print_all_colors Color.all;
    print_endline "";
    
    print_string "Color.toString Color.Spade = ";
    print_endline (Color.toString Color.Spade);
    print_string "Color.toStringVerbose Color.Heart = ";
    print_endline (Color.toStringVerbose Color.Heart);
    print_string "Color.toString Color.Diamond = ";
    print_endline (Color.toString Color.Diamond);
    print_string "Color.toStringVerbose Color.Club = ";
    print_endline (Color.toStringVerbose Color.Club)