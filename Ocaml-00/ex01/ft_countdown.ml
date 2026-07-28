let rec ft_countdown start =
    if start <= 0 then (
      print_int 0;
      print_char '\n'
    ) else (
      print_int start;
      print_char '\n';
      ft_countdown (start - 1)
    )

    
(*let () =
    print_endline "ft_countdown 3";
    ft_countdown 3;
    print_endline "";

    print_endline "ft_countdown 0";
    ft_countdown 0;
    print_endline "";

    print_endline "ft_countdown (-2)";
    ft_countdown (-2)*)
