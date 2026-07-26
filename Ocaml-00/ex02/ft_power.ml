let rec ft_power number power =
    if power = 0 then
      1
    else
      number * ft_power number (power - 1)

(*let () =
    print_endline "ft_power 2 4";
    print_int (ft_power 2 4);
    print_endline "";

    print_endline "ft_power 2 0";
    print_int (ft_power 2 0);
    print_endline "";

    print_endline "ft_power 1 2";
    print_int (ft_power 1 2);
    print_endline "";

    print_endline "ft_power 0 1";
    print_int (ft_power 0 1);
    print_endline "";*)