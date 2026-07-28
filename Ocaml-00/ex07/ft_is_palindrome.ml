let rec compare_letters str left_ind right_ind = 
    if left_ind = right_ind || left_ind > right_ind then
        true
    else (
        if String.get str left_ind <> String.get str right_ind then
            false
        else
            compare_letters str (left_ind + 1) (right_ind - 1)
    )


let ft_is_palindrome str =
    if String.length str = 0 then
        true
    else
        compare_letters str 0 (String.length str - 1)


(*let print_bool boolean =
    match boolean with
        | true -> print_string "true";
        | false -> print_string "false"


let () = 
    print_endline "ft_is_palindrome 'radar'";
    print_bool (ft_is_palindrome "radar");
    print_endline "";

    print_endline "ft_is_palindrome 'madam'";
    print_bool (ft_is_palindrome "madam");
    print_endline "";

    print_endline "ft_is_palindrome 'car'";
    print_bool (ft_is_palindrome "car");
    print_endline "";

    print_endline "ft_is_palindrome ''";
    print_bool (ft_is_palindrome "");
    print_endline "";*)
