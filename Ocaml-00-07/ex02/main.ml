let () =
    let methane = new Alkane.methane () in
    let ethane = new Alkane.ethane () in
    let propane = new Alkane.propane () in
    let octane = new Alkane.octane () in

    print_endline "==== Alkane descriptions ====";
    List.iter print_endline
        [ methane#to_string; ethane#to_string; propane#to_string; octane#to_string ];

    print_newline ();
    print_endline "==== Equality tests ====";
    Printf.printf "methane == methane : %b\n"
        (methane#equals (new Alkane.methane () :> Molecule.molecule));
    Printf.printf "methane == ethane  : %b\n"
        (methane#equals (ethane :> Molecule.molecule));
    
