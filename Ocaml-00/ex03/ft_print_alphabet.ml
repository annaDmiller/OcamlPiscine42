let rec print_letter code =
    if code <= 122 then (
        print_char (char_of_int code);
        print_letter (code + 1)
    ) else
        print_char '\n'

let ft_print_alphabet () = 
    let a_code = int_of_char 'a' in
    print_letter a_code

(*let () = ft_print_alphabet()*)