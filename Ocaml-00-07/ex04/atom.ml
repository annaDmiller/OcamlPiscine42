class virtual atom (n : string) (sym: string) (at_num : int) =
object (self)
    method name = n
    method symbol = sym
    method atomic_number = at_num

    method to_string =
        Printf.sprintf "%s (%s) - atomic number %d" self#name self#symbol self#atomic_number

    method equals (other : atom) =
        self#symbol = other#symbol && self#atomic_number = other#atomic_number
end

class hydrogen ()  = object inherit atom "Hydrogen" "H" 1 end
class helium ()    = object inherit atom "Helium" "He" 2 end
class lithium ()   = object inherit atom "Lithium" "Li" 3 end
class beryllium () = object inherit atom "Beryllium" "Be" 4 end
class carbon ()    = object inherit atom "Carbon" "C" 6 end
class nitrogen () = object inherit atom "Nitrogen" "N" 7 end
class oxygen ()    = object inherit atom "Oxygen" "O" 8 end
