class people (n : string) =
    object
        val _name:string = n
        val _hp:int = 100

        initializer
            print_endline (_name ^ " materializes into the story, ready to meet strange men in blue boxes.")
        
        method to_string =
            _name ^ " of hp: " ^ string_of_int _hp

        method talk =
            print_endline ("I'm " ^ _name ^ "! Do you know the Doctor?")

        method die =
            print_endline "Aaaarghh!"   
    end
