let print_reaction (r : Reaction.reaction) =
    print_endline "Start:";
    List.iter (fun (m, c) -> Printf.printf "  %d x %s\n" c m#formula) r#get_start;
    print_endline "Result:";
    List.iter (fun (m, c) -> Printf.printf "  %d x %s\n" c m#formula) r#get_result

let () =
    print_endline "== Methane combustion (CH4 + 2 O2 -> CO2 + 2 H2O) ==";
    let methane_combustion =
        new Alkane_combustion.alkane_combustion [ new Alkane.methane () ]
    in
    Printf.printf "is_balanced: %b\n" methane_combustion#is_balanced;
    print_reaction methane_combustion;

    print_newline ();
    print_endline "== Ethane combustion (2 C2H6 + 7 O2 -> 4 CO2 + 6 H2O) ==";
    let ethane_combustion =
        new Alkane_combustion.alkane_combustion [ new Alkane.ethane () ]
    in
    print_reaction ethane_combustion;

    print_newline ();
    print_endline "== Octane combustion, obtained through balance () ==";
    print_endline "== Formula : 2 C8H18 + 25 O2 -> 16 CO2 + 18 H2O ==";
    let octane_combustion =
        (new Alkane_combustion.alkane_combustion [ new Alkane.octane () ])#balance
    in
    print_reaction octane_combustion;

    print_newline ();
    print_endline "== Duplicate alkanes are only burnt once ==";
    let dedup_combustion =
        new Alkane_combustion.alkane_combustion
            [ new Alkane.propane (); new Alkane.propane () ]
    in
    print_reaction dedup_combustion;

    print_newline ();
    print_endline "== Alkanes combustion for methane, ethane and propane ==";
    let mult_combustion =
        new Alkane_combustion.alkane_combustion
            [ new Alkane.methane (); new Alkane.ethane(); new Alkane.propane () ]
    in
    print_reaction mult_combustion