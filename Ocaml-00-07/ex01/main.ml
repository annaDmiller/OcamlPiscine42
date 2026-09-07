let () =
    let h2o = new Molecule.water () in
    let co2 = new Molecule.carbon_dioxide () in
    let ac_ac = new Molecule.acetic_acid() in
    let caff = new Molecule.caffeine() in
    let tnt = new Molecule.tnt() in

    print_endline "==== Molecule descriptions ====";
    List.iter print_endline
        [ h2o#to_string; co2#to_string; ac_ac#to_string;
        caff#to_string; tnt#to_string ];

    print_newline ();
    print_endline "==== Equality tests ====";
    Printf.printf "water == water : %b\n" (h2o#equals (new Molecule.water ()));
    Printf.printf "water == carbon dioxide : %b\n" (h2o#equals co2)