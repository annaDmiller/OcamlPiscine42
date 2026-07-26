let rec print_num_comb (one : int) (two : int) (three : int) = 
    if three <= 9 && two <= 9 && one <= 9 then (
        if three > two && two > one then (
            print_int one;
            print_int two;
            print_int three;
            if one <> 7 || two <> 8 || three <> 9 then 
                print_string ", "
        );
        if three <> 9 then
            print_num_comb one two (three + 1)
        else (
            if two <> 9 then
                print_num_comb one (two + 1) 0
            else (
                print_num_comb (one + 1) 0 0
            )
        )
    ) else
        print_string "\n"
  
let ft_print_comb () = 
    let first = 0 in
    let second = 1 in
    let third = 2 in
    print_num_comb first second third

(*let () = ft_print_comb()*)