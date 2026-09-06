let () =
    print_endline "==== Testing the people class ====";
    let clara = new People.people "Clara"
    in
    print_string "to_sting signal:  ";
    print_endline clara#to_string;
    print_string "talk signal:  ";
    clara#talk;
    print_string "die signal:  ";
    clara#die;
