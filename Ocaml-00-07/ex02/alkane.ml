exception Alkane_name of string

let alkane_name (size : int) =
    match size with
        | 1 -> "Methane"
        | 2 -> "Ethane"
        | 3 -> "Propane"
        | 4 -> "Butane"
        | 5 -> "Pentane"
        | 6 -> "Hexane"
        | 7 -> "Heptane"
        | 8 -> "Octane"
        | 9 -> "Nonane"
        | 10 -> "Decane"
        | 11 -> "Undecane"
        | 12 -> "Dodecane"
        | _ -> raise (Alkane_name "Alkane's size must be between 1 and 12")

class virtual alkane (n : int) =
    let rec make_atoms (sym : string) (count : int) : Atom.atom list = 
        if count <= 0 then
            []
        else
            match sym with
                | "H" -> new Atom.hydrogen() :: make_atoms sym (count - 1)
                | "C" -> new Atom.carbon() :: make_atoms sym (count - 1)
                | _ -> []
    in
    let atoms = make_atoms "C" n @ make_atoms "H" (2 * n + 2)
    in
object
    inherit Molecule.molecule (alkane_name n) atoms
    method size = n
end

class methane () = object inherit alkane 1 end
class ethane ()  = object inherit alkane 2 end
class propane () = object inherit alkane 3 end
class butane ()  = object inherit alkane 4 end
class pentane () = object inherit alkane 5 end
class hexane ()  = object inherit alkane 6 end
class heptane () = object inherit alkane 7 end
class octane ()  = object inherit alkane 8 end
class nonane ()  = object inherit alkane 9 end
class decane ()  = object inherit alkane 10 end
class undecane () = object inherit alkane 11 end
class dodecane () = object inherit alkane 12 end
