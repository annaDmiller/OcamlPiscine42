let match_char_str (car : char) : string = 
    match car with
        | '0' -> "0"
        | '1' -> "1"
        | '2' -> "2"
        | '3' -> "3"
        | '4' -> "4"
        | '5' -> "5"
        | '6' -> "6"
        | '7' -> "7"
        | '8' -> "8"
        | '9' -> "9"
        | _ -> ""

let rec encode (lst : char list) : (int * char) list =
    match lst with
        | [] -> []
        | val_lst :: rest -> (
            match encode rest with
                | (count, val_tup) :: rest_tup when val_tup = val_lst -> (count + 1, val_tup) :: rest_tup
                | rest_tup -> (1, val_lst) :: rest_tup
        )

let rec int_to_char_lst (num : int) (res : char list) : char list = 
    let match_int_char (num : int) : char = 
        match num with
            | 0 -> '0'
            | 1 -> '1'
            | 2 -> '2'
            | 3 -> '3'
            | 4 -> '4'
            | 5 -> '5'
            | 6 -> '6'
            | 7 -> '7'
            | 8 -> '8'
            | 9 -> '9'
            | _ -> '-'
    in
    if num < 10 then
        match_int_char num :: res
    else
    int_to_char_lst (num / 10) (match_int_char (num mod 10) :: res)

let rec tup_to_char_list (tup : (int * char) list) : char list =
    match tup with
        | (count, value) :: rest -> int_to_char_lst count [] @ (value :: tup_to_char_list rest)
        | [] -> []

let rec char_list_to_str (lst : char list) : string =
    match lst with
        | value :: rest -> match_char_str value ^ char_list_to_str rest
        | _ -> ""

let rec init_char_lst (el_num : int) : char list = 
    if el_num = 1 then
        ['1']
    else
        tup_to_char_list (encode (init_char_lst (el_num - 1)))

let sequence (el_num : int) : string = 
    if el_num <= 0 then 
        ""
    else
        char_list_to_str (init_char_lst el_num)


let () =
    let test n =
        print_string "sequence ";
        print_int n;
        print_string " = \"";
        print_endline ((sequence n) ^ "\"")
    in
    test 1;
    test 2;
    test 3;
    test 4;
    test 5;
    test 6;
    test 7;
    test 0;
    test (-1)