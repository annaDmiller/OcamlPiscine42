let () =
    let ch4 = new Alkane.methane() in
    let h2 = new Molecule.dihydrogen () in
    let o2 = new Molecule.dioxygen () in
    let co2 = new Molecule.carbon_dioxide () in
    let h2o = new Molecule.water () in
    let h2o2 = new Molecule.hydrogen_peroxide ()
    in

    let unbalanced = new Reaction.test_reaction
                        [ (h2, 1);
                         (o2, 1) ]
                        [ (h2o, 1) ]
    in

    let balanced = new Reaction.test_reaction
                        [ ( (ch4 :> Molecule.molecule), 1);
                         ( o2, 2) ]
                        [ (co2, 1); (h2o, 2) ]
    in

    print_endline "== Check of the balanced reaction ==";
    Printf.printf "CH4 + 2O2 -> CO2 + 2H2O is_balanced: %b\n" balanced#is_balanced;

    print_endline "== Check of the unbalanced reaction ==";
    Printf.printf "H2 + O2 -> H2O   is_balanced: %b\n" unbalanced#is_balanced;

    print_newline();
    print_endline "== Balanced reaction content ==";
    print_endline "Start:";
    List.iter (fun (m, c) -> Printf.printf "  %d x %s\n" c m#formula) balanced#get_start;
    print_endline "Result:";
    List.iter (fun (m, c) -> Printf.printf "  %d x %s\n" c m#formula) balanced#get_result;

    print_newline();
    print_endline "== Balance the unbalanced reaction ==";
    let balanced_unbalanced = unbalanced#balance
    in
    print_endline "Start:";
    List.iter (fun (m, c) -> Printf.printf "  %d x %s\n" c m#formula) balanced_unbalanced#get_start;
    print_endline "Result:";
    List.iter (fun (m, c) -> Printf.printf "  %d x %s\n" c m#formula) balanced_unbalanced#get_result;

    print_newline ();
    print_endline "== Check of fafilures of balance method ==";
    let test_balance (name: string) (reaction: Reaction.reaction) =
        print_endline ("Testing: " ^ name);
        try
            let _ = reaction#balance
            in
            print_endline "No exception raised.";
        with Failure msg -> print_endline msg;
    in
    let no_free = new Reaction.test_reaction
                                [ (h2, 1) ]
                                [ (o2, 1) ]
    in
    let more_than_one = new Reaction.test_reaction
                                [ (h2, 1); (o2, 1) ]
                                [ (h2o, 1); (h2o2, 1) ]
    in
    let no_pos = new Reaction.test_reaction
                                [ (h2, 1); (o2, 1) ]
                                [ (h2o, 1); (co2, 1) ] 
    in                           
    test_balance "H2 -> O2 - no non-zero solution" no_free;
    test_balance "H2 + O2 -> H2O + H2O2 - can't define one solution" more_than_one;
    test_balance "H2 + O2 -> H2O + CO2 - no strictly positive solution" no_pos;
