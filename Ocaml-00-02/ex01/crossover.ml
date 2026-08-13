let rec is_present value lst =
    match lst with
        | [] -> false
        | x :: rest when x = value -> true
        | _ :: rest -> is_present value rest


let rec crossover lst_a lst_b =
    if lst_b = [] then
        []
    else (
        match lst_a with
            | [] -> []
            | val_a :: rest_a when is_present val_a lst_b -> val_a :: crossover rest_a lst_b
            | _ :: rest_a -> crossover rest_a lst_b
    )



let rec print_lst_int arg =
    match arg with
        | [] -> print_endline "]"
        | [value] -> (
            print_int value;
            print_endline "]"
        )
        | value :: rest -> (
            print_int value;
            print_string "; ";
            print_lst_int rest
        )

let () =
    print_endline "crossover [0; 1; 2; 3; 4; 5; 6; 7; 8] and [6; 7; 8; 9; 10]";
    print_char '[';
    let first_lst = [0; 1; 2; 3; 4; 5; 6; 7; 8]
    and second_lst = [6; 7; 8; 9; 10]
    in
    print_lst_int @@ crossover first_lst second_lst;

    print_endline "crossover [] and [6; 7; 8; 9; 10]";
    print_char '[';
    let first_lst = []
    and second_lst = [6; 7; 8; 9; 10]
    in
    print_lst_int @@ crossover first_lst second_lst;

    print_endline "crossover [0; 1; 2; 3; 4; 5; 6; 7; 8] and []";
    print_char '[';
    let first_lst = [0; 1; 2; 3; 4; 5; 6; 7; 8]
    and second_lst = []
    in
    print_lst_int @@ crossover first_lst second_lst;

    print_endline "crossover [0; 1; 2; 3; 4; 5] and [6; 7; 8; 9; 10]";
    print_char '[';
    let first_lst = [0; 1; 2; 3; 4; 5]
    and second_lst = [6; 7; 8; 9; 10]
    in
    print_lst_int @@ crossover first_lst second_lst
