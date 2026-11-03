let () =
    let ch4 = new Alkane.methane() in
    let h2 = new Molecule.dihydrogen () in
    let o2 = new Molecule.dioxygen () in
    let co2 = new Molecule.carbon_dioxide () in
    let h2o = new Molecule.water ()
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

