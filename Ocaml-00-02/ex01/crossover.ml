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