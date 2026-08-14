let rec list_to_str_with_pref pref res_str rest_lst =
    match rest_lst with
        | [] -> res_str
        | [""] -> (
            if String.is_empty res_str then
                pref
            else
                res_str
        )
        | str :: rest -> (
            if String.is_empty res_str then
                list_to_str_with_pref pref (pref ^ str) rest
            else
                list_to_str_with_pref pref (res_str ^ " " ^ pref ^ str) rest
        )

let add_prefix pref seq =
    let str_lst = String.split_all ~sep:" " seq
    in
    list_to_str_with_pref pref "" str_lst

let add_prefix_and_rev pref seq =
    let str_lst = String.rsplit_all ~sep:" " seq in
        let rec aux res_lst rest_lst = 
            match rest_lst with
                | [] -> res_lst
                | value::rest -> aux (value::res_lst) rest
        in
    list_to_str_with_pref pref "" (aux [] str_lst)


let rec gray_seq bit_num =
    if bit_num <= 0 then
        ""
    else
        let prev_seq = gray_seq (bit_num - 1)
        in
            let first_half = add_prefix "0" prev_seq in
            let sec_half = add_prefix_and_rev "1" prev_seq in
        first_half ^ " " ^ sec_half

let gray bit_num = 
    print_endline (gray_seq bit_num)


let () = 
    print_endline "gray 1";
    gray 1;
    print_endline "";

    print_endline "gray 2";
    gray 2;
    print_endline "";

    print_endline "gray 3";
    gray 3;
    print_endline "";

    print_endline "gray 4";
    gray 4;
    print_endline "";

    print_endline "gray 0";
    gray 0;
    print_endline ""
