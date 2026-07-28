let ft_is_alpha car =
    if (car >= 'a' && car <= 'z') || (car >= 'A' && car <= 'Z') then
        true
    else
        false


let move_car_one_step car = 
    if ft_is_alpha car = false then
        car
    else (
        match car with
            | 'z' ->  'a'
            | 'Z' -> 'A'
            | _ -> char_of_int (int_of_char car + 1)
    )


let rec ft_rot_n move str = 
    if move = 0 then
        str
    else
        ft_rot_n (move - 1) (String.map move_car_one_step str)

(*let () =
    print_endline "ft_rot_n 1 'abcdefghijklmnopqrstuvwxyz'";
    print_string @@ ft_rot_n 1 "abcdefghijklmnopqrstuvwxyz";
    print_endline "";

    print_endline "ft_rot_n 13 'abcdefghijklmnopqrstuvwxyz'";
    print_string @@ ft_rot_n 13 "abcdefghijklmnopqrstuvwxyz";
    print_endline "";

    print_endline "ft_rot_n 1 '0123456789'";
    print_string @@ ft_rot_n 1 "0123456789";
    print_endline "";

    print_endline "ft_rot_n 3 'ABCD565yuio'";
    print_string @@ ft_rot_n 3 "ABCD565yuio";
    print_endline "";

    print_endline "ft_rot_n 0 'Dammed! end'";
    print_string @@ ft_rot_n 0 "Dammed! end";
    print_endline "";

    print_endline "ft_rot_n 42 ''";
    print_string @@ ft_rot_n 0 "";
    print_endline "";*)
