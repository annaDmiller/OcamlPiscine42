class test_reaction start result =
object (self)
    inherit Reaction.reaction start result

    method get_start = start
    method get_result = result
    method balance = (self :> Reaction.reaction)
    method is_balanced = self#check_balance
end

let () =
    let ch4 = new Alkane.methane() in
    let o2 = new Molecule.dioxygen () in
    let co2 = new Molecule.carbon_dioxide () in
    let h2o = new Molecule.water ()
    in

    let unbalanced = new test_reaction
                        [ ( (ch4 :> Molecule.molecule), 1);
                         ( o2, 1) ]
                        [ (co2, 1); (h2o, 1) ]
    in

    let balanced = new test_reaction
                        [ ( (ch4 :> Molecule.molecule), 1);
                         ( o2, 2) ]
                        [ (co2, 1); (h2o, 2) ]
    in

    print_endline "== Check of the balanced reaction ==";
    Printf.printf "CH4 + 2O2 -> CO2 + 2H2O is_balanced: %b\n" balanced#is_balanced;

    print_endline "== Check of the unbalanced reaction ==";
    Printf.printf "CH4 + O2 -> CO2 + H2O   is_balanced: %b\n" unbalanced#is_balanced;

    print_newline();
    print_endline "== Balanced reaction content ==";
  print_endline "Start:";
  List.iter (fun (m, c) -> Printf.printf "  %d x %s\n" c m#formula) balanced#get_start;
  print_endline "Result:";
  List.iter (fun (m, c) -> Printf.printf "  %d x %s\n" c m#formula) balanced#get_result