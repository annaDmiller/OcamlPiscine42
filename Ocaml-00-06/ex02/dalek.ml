let () = Random.self_init ()

let random_upper_char () =
    let letter_count = 26
    in
    char_of_int (int_of_char 'A' + Random.int letter_count)

let random_suffix (len : int) : string =
    String.init len (fun _ -> random_upper_char ())

class dalek =
    object (self)
        val _name = "Dalek" ^ random_suffix 3
        val _hp = 100
        val mutable _shield = true

        initializer
            print_endline (_name ^ " rises from its casing, ready to exterminate all who oppose the Daleks!")

        method to_string =
            _name ^ " [hp: " ^ string_of_int _hp ^ ", shield: " ^
            (if _shield then "up" else "down") ^ "]"

        method talk =
            let roll = Random.int 4 in
            let line =
                match roll with
                    | 0 -> "Explain! Explain!"
                    | 1 -> "Exterminate! Exterminate!"
                    | 2 -> "I obey!"
                    | _ -> "You are the Doctor! You are the enemy of the Daleks!"
            in
            print_endline line

        method exterminate (target : People.people) =
            _shield <- not _shield;
            print_endline (_name ^ " screeches: EXTERMINATE! on " ^ target#to_string);
            print_string (target#get_name ^ ": ");
            target#die

        method die =
            print_endline "Emergency Temporal Shift!"
    end
