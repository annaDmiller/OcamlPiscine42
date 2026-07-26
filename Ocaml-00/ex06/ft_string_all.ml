let rec check_str func str ind =
    if ind >= 0 then (
        if func (String.get str ind) = true then (
            if check_str func str (ind - 1) = true then
                true
            else
                false
        ) else
            false
    ) else
        true


let ft_string_all func str = 
    if String.length str = 0 then
        false
    else
        check_str func str (String.length str - 1)


(*let is_digit c = c >= '0' && c <= '9'

let print_bool boolean =
    match boolean with
        | true -> print_string "true";
        | false -> print_string "false"

let () = 
    print_bool (ft_string_all is_digit "0123456789");
    print_endline "";

    print_bool (ft_string_all is_digit "0123456789A");
    print_endline "";
    
    print_bool (ft_string_all is_digit "");
    print_endline ""*)