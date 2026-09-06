class doctor (name : string) (age : int) (side : People.people) =
    object (self)
        val _name:string = name
        val mutable _age:int = age
        val _sidekick:People.people = side
        val mutable _hp:int = 100

        initializer
            print_endline ("A new incarnation, " ^ _name ^ ", steps out of the light. The Doctor has arrived!")
        
        method to_string =
            _name ^ " (Time Lord) [age: " ^ string_of_int _age ^
            ", hp: " ^ string_of_int _hp ^
            ", sidekick: " ^ _sidekick#to_string ^ "]"
        
        method talk =
            print_endline "Hi! I'm the Doctor!"

        method private regenerate = 
            _hp <- 100;
            print_endline (_name ^ " : hp restored")


        method travel_in_time (start : int) (arrival : int) =
            let delta = arrival - start
            in
            _age <- _age + delta;
            if _age < 0 
                then _age <- 0;
            print_endline "   ___________ ";
            print_endline "  | .-------. |";
            print_endline "  | |POLICE | |";
            print_endline "  | | BOX   | |";
            print_endline "  | |_______| |";
            print_endline "  |___________|";
            print_endline "  |  |     |  |";
            print_endline "  |__|_____|__|";
            (* We will test private regenerate method as part of this method *)
            self#regenerate

        (* For testing purposes of the regeneration *)
        method take_damage (dmg : int) =
            _hp <- _hp - dmg

        method use_sonic_screwdriver =
            print_endline "Whiiiiwhiiiwhiii Whiiiiwhiiiwhiii Whiiiiwhiiiwhiii"
    end