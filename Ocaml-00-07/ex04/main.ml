let print_reaction (r : Reaction.reaction) =
    print_endline "Start:";
    List.iter
        (fun (m, c) -> Printf.printf "  %d x %s\n" c m#formula)
        r#get_start;

    print_endline "Result:";
    List.iter
        (fun (m, c) -> Printf.printf "  %d x %s\n" c m#formula)
        r#get_result


let () =
    print_endline "== Methane combustion ==";
    print_endline "CH4 + 2 O2 -> CO2 + 2 H2O";

    let methane_combustion =
        new Alkane_combustion.alkane_combustion
            [ new Alkane.methane () ]
    in
    Printf.printf "Initially balanced: %b\n" methane_combustion#is_balanced;
    print_string "Try get_start with unbalanced alkane_combustion:  ";
    try
        List.iter
            (fun (m, c) -> Printf.printf "  %d x %s\n" c m#formula)
            methane_combustion#get_start
    with
        | Alkane_combustion.Reaction_not_balanced msg -> print_endline msg;

    let balanced_methane = methane_combustion#balance in

    Printf.printf "Balanced after balance(): %b\n"
        balanced_methane#is_balanced;

    print_reaction balanced_methane;


    print_newline ();
    print_endline "== Ethane combustion ==";
    print_endline "2 C2H6 + 7 O2 -> 4 CO2 + 6 H2O";

    let ethane_combustion =
        new Alkane_combustion.alkane_combustion
            [ new Alkane.ethane () ]
    in

    print_reaction (ethane_combustion#balance);


    print_newline ();
    print_endline "== Octane combustion ==";
    print_endline "2 C8H18 + 25 O2 -> 16 CO2 + 18 H2O";

    let octane_combustion =
        new Alkane_combustion.alkane_combustion
            [ new Alkane.octane () ]
    in

    print_reaction (octane_combustion#balance);


    print_newline ();
    print_endline "== Combustion of two propane molecules (removing duplicates) ==";
    print_endline "C3H8 + 5 O2 -> 3 CO2 + 4 H2O";

    let duplicate_combustion =
        new Alkane_combustion.alkane_combustion
            [
                new Alkane.propane ();
                new Alkane.propane ()
            ]
    in

    print_endline "After balancing:";
    print_reaction (duplicate_combustion#balance);


    print_newline ();
    print_endline "== Combustion of methane, ethane and propane ==";

    let multiple_combustion =
        new Alkane_combustion.alkane_combustion
            [
                new Alkane.methane ();
                new Alkane.ethane ();
                new Alkane.propane ()
            ]
    in

    print_endline "After balancing:";
    print_reaction (multiple_combustion#balance)
