let rec converges func arg num_iter = 
    if num_iter < 0 then
      false
    else if num_iter >= 0 && arg = func (arg) then
        true
    else
        converges func (func (arg)) (num_iter - 1)


let ft_print_bool (value : bool) = 
    match value with
        | true -> print_endline "true";
        | false -> print_endline "false"


let () = 
    print_endline "converges (( * ) 2) 2 5 - expected false";
    ft_print_bool @@ converges (( * ) 2) 2 5;

    print_endline "converges (fun x -> x / 2) 2 3 - expected true";
    ft_print_bool @@ converges (fun x -> x / 2) 2 3;

    print_endline "converges (fun x -> x / 2) 2 2 - expected true";
    ft_print_bool @@ converges (fun x -> x / 2) 2 2;

    print_endline "converges (fun x -> x * x) 1 5 - expected true";
    ft_print_bool @@ converges (fun x -> x * x) 1 5
