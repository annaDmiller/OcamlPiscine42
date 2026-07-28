let rec print_rev string ind = 
    if ind >= 0 then (
        print_char (String.get string ind);
        print_rev string (ind - 1)
    )

let ft_print_rev string =
    print_rev string (String.length string - 1);
    print_char '\n'


(*let () = 
    ft_print_rev "Hello World !";

    print_endline "-------";

    ft_print_rev ""*)