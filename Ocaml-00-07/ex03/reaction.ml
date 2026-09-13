class virtual reaction
    (start_molecules : (Molecule.molecule * int) list)
    (result_molecules : (Molecule.molecule * int) list) =
object (self)
    val start_molecules = start_molecules
    val result_molecules = result_molecules

    method virtual get_start : (Molecule.molecule * int) list
    method virtual get_result : (Molecule.molecule * int) list
    method virtual balance : reaction
    method virtual is_balanced : bool

    method private count_atoms (side : (Molecule.molecule * int) list) : (string * int) list =
        let add acc symbol amount =
            match List.assoc_opt symbol acc with
                | Some count -> (symbol, count + amount) :: List.remove_assoc symbol acc
                | None -> (symbol, amount) :: acc
        in
        List.fold_left 
            (fun curr_acc_mol (mol, quant) -> List.fold_left 
                                              (fun curr_acc_a at -> add curr_acc_a at#symbol quant)
                                              curr_acc_mol mol#atom_list)
            [] side

    method private check_balance =
        let normalize sym_lst = List.sort compare sym_lst
        in
        normalize (self#count_atoms start_molecules) = normalize (self#count_atoms result_molecules)
end