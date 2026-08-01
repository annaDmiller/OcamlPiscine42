let rec iter func (arg : int) (num_iter : int) = 
    if num_iter < 0 then
        -1
    else if num_iter = 0 then
        arg
    else
        func (iter func arg (num_iter - 1))


let ft_square num = num * num

let () = 
    print_endline "iter (ft_square) 2 2 - expected 16";
    print_int @@ iter ft_square 2 2;
    print_endline "";

    print_endline "iter (ft_square) 2 4 - expected 65536";
    print_int @@ iter ft_square 2 4;
    print_endline "";

    print_endline "iter (ft_square) 2 0 - expected 2";
    print_int @@ iter ft_square 2 0;
    print_endline "";

    print_endline "iter (ft_square) 2 -4 - expected -1";
    print_int @@ iter ft_square 2 (-4);
    print_endline ""
