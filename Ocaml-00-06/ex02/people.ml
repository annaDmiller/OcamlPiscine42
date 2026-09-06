class people (n : string) =
    object
        val _name:string = n
        val mutable _hp:int = 100

        initializer
            print_endline (_name ^ " materializes into the story, ready to meet strange men in blue boxes.")
        
        method to_string =
            _name ^ " of hp: " ^ string_of_int _hp

        method talk =
            print_endline ("I'm " ^ _name ^ "! Do you know the Doctor?")

        method die =
            if _hp <> 0 then (
                print_endline "Aaaarghh!";
                _hp <- 0
            );

        method get_name = _name
    end
