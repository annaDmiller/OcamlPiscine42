let print_value number = 
    if number >= 10 then
        print_int number
    else (
        print_char '0';
        print_int number
    )


let rec print_comb_sec first second = 
    if second <= 99 then (
        if first <> 0 || second <> 1 then
            print_char ' ';

        print_value first;
        print_char ' ';
        print_value second;
        if second <> 99 then (
            print_char ',';
            print_comb_sec first (second + 1)
        )
    )


let rec print_comb first = 
    if first <= 98 then (
        print_comb_sec first (first + 1);
        if first <> 98 then (
            print_char ',';
            print_comb (first + 1)
        )
    )


let ft_print_comb2 () = 
    print_comb 0;
    print_char '\n'


(*let () = ft_print_comb2 ()*)