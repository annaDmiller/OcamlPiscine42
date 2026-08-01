let ft_sum express lower_ind upper_ind =
    let rec sum_val func ind limit = 
        if ind = limit then
            func ind
        else
            func ind +. sum_val func (ind + 1) limit
    in
    
    if upper_ind < lower_ind then
        nan
    else
        sum_val express lower_ind upper_ind


let ft_square num = float_of_int (num * num)

let () = 
    print_endline "ft_sum ft_square 0 10 - expected 385";
    print_float @@ ft_sum ft_square 0 10;
    print_endline "";

    print_endline "ft_sum (fun x -> float_of_int x) 0 15 - expected 120";
    print_float @@ ft_sum (fun x -> float_of_int x) 0 15;
    print_endline ""