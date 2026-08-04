let rec encode lst =
    match lst with
        | [] -> []
        | val_lst :: rest_lst -> (
            match encode rest_lst with
                | (count, val_tup) :: rest_tup when val_tup = val_lst -> (count + 1, val_tup) :: rest_tup
                | rest_tup -> (1, val_lst) :: rest_tup
        )




let rec print_lst_tupl_int lst =
    match lst with
        | [] -> print_string "]"
        | [(count, value)] -> (
            print_string "(";
            print_int count;
            print_string ", ";
            print_int value;
            print_string ")]"
        )
        | (count, value) :: rest -> (
            print_string "(";
            print_int count;
            print_string ", ";
            print_int value;
            print_string "); ";
            print_lst_tupl_int rest
        )

let rec print_lst_tupl_char lst =
    match lst with
        | [] -> print_string "]"
        | [(count, value)] -> (
            print_string "(";
            print_int count;
            print_string ", ";
            print_char value;
            print_string ")]"
        )
        | (count, value) :: rest -> (
            print_string "(";
            print_int count;
            print_string ", ";
            print_char value;
            print_string "); ";
            print_lst_tupl_char rest
        )

let () =
    print_endline "encode ['a'; 'a'; 'a'; 'b'; 'a']";
    print_char '[';
    let temp_lst = ['a'; 'a'; 'a'; 'b'; 'a']
    in
    print_lst_tupl_char @@ encode temp_lst;
    print_string "\n\n";

    print_endline "encode [2; 3; 3; 3]";
    print_char '[';
    let temp_lst = [2; 3; 3; 3]
    in
    print_lst_tupl_int @@ encode temp_lst;
    print_string "\n\n";

    print_endline "encode []";
    print_char '[';
    let empty_lst = []
    in
    print_lst_tupl_char @@ encode empty_lst;
    print_char '\n'
