class virtual molecule (n : string) (atom_list : Atom.atom list) =
object (self)
    method name = n
    method atom_list = atom_list

    method private atom_counts : (string * int) list =
        let add acc symbol =
            match List.assoc_opt symbol acc with
                | Some count -> (symbol, count + 1) :: List.remove_assoc symbol acc
                | None -> (symbol, 1) :: acc
        in
        List.fold_left (fun acc x -> add acc x#symbol) [] atom_list

    method formula =
        let count_list = self#atom_counts in
        let string_of_pair (symbol, count) =
            if count = 1 then
                symbol
            else
                symbol ^ string_of_int count
        in
        let search sym =
            match List.assoc_opt sym count_list with
                | Some count -> string_of_pair (sym, count)
                | None -> ""
        in
        let carbon_part = search "C" in
        let hydro_part = search "H" in
        let rec others lst =
            match lst with
                | (symb, count) :: rest -> (
                    if symb = "H" || symb = "C" then
                        others rest
                    else
                        string_of_pair (symb, count) ^ others rest
                )
                | [] -> ""
        in
        carbon_part ^ hydro_part ^ others (List.sort (fun (a, _) (b, _) -> compare a b) count_list)
        
    method to_string = Printf.sprintf "%s: %s" self#name self#formula

    method equals (other : molecule) = self#formula = other#formula
end

class water () =
object
    inherit molecule "Water"
        [ new Atom.oxygen(); new Atom.hydrogen(); new Atom.hydrogen() ]
end

class carbon_dioxide () =	
object
    inherit molecule "Carbon dioxide"
        [ new Atom.carbon (); new Atom.oxygen (); new Atom.oxygen () ]
end

class acetic_acid () =
object
    inherit molecule "Acetic acid"
        [ new Atom.oxygen(); new Atom.hydrogen(); new Atom.carbon();
        new Atom.carbon(); new Atom.hydrogen(); new Atom.hydrogen();
        new Atom.oxygen(); new Atom.hydrogen() ]
end

class caffeine () =
object
    inherit molecule "Caffeine"
        [ new Atom.nitrogen(); new Atom.nitrogen(); new Atom.nitrogen();
        new Atom.nitrogen(); new Atom.hydrogen(); new Atom.hydrogen();
        new Atom.hydrogen(); new Atom.hydrogen(); new Atom.hydrogen();
        new Atom.hydrogen(); new Atom.hydrogen(); new Atom.hydrogen();
        new Atom.hydrogen(); new Atom.hydrogen(); new Atom.oxygen();
        new Atom.oxygen(); new Atom.carbon(); new Atom.carbon();
        new Atom.carbon(); new Atom.carbon(); new Atom.carbon();
        new Atom.carbon(); new Atom.carbon(); new Atom.carbon() ] 
end

class tnt () =
object
    inherit molecule "Trinitrotoluene"
        [ new Atom.carbon (); new Atom.carbon (); new Atom.carbon ();
        new Atom.carbon (); new Atom.carbon (); new Atom.carbon (); new Atom.carbon ();
        new Atom.hydrogen (); new Atom.hydrogen (); new Atom.hydrogen ();
        new Atom.hydrogen (); new Atom.hydrogen ();
        new Atom.nitrogen (); new Atom.nitrogen (); new Atom.nitrogen ();
        new Atom.oxygen (); new Atom.oxygen (); new Atom.oxygen ();
        new Atom.oxygen (); new Atom.oxygen (); new Atom.oxygen () ]
end
