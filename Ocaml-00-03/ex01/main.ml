let rec print_all_values (values : Value.t list) =
    match values with
        | [] -> ()
        | v :: rest ->
            print_string (Value.toString v);
            print_string " (";
            print_string (Value.toStringVerbose v);
            print_string ", int = ";
            print_int (Value.toInt v);
            print_endline ")";
            print_all_values rest

let () =
    print_endline "Value.all values";
    print_all_values Value.all;
    print_endline "";

    print_endline "Check of next / previous logic";
    print_string "next T2 = ";
    print_endline (Value.toStringVerbose (Value.next Value.T2));
    print_string "previous As = ";
    print_endline (Value.toStringVerbose (Value.previous Value.As));
    print_string "next Queen = ";
    print_endline (Value.toStringVerbose (Value.next Value.Queen));
    print_string "previous Jack = ";
    print_endline (Value.toStringVerbose (Value.previous Value.Jack));
    print_endline "";

    print_endline "invalid_arg checks";
    (try
       let _ = Value.next Value.As in
       print_endline "ERROR: should have raised"
     with Invalid_argument msg -> print_string "next As raised: "; print_endline msg);
    (try
       let _ = Value.previous Value.T2 in
       print_endline "ERROR: should have raised"
     with Invalid_argument msg -> print_string "previous T2 raised: "; print_endline msg)