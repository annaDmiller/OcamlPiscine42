let rec print_letter code =
    if code <= int_of_char 'z' then (
        print_char (char_of_int code);
        print_letter (code + 1)
    ) else
        print_char '\n'

let ft_print_alphabet () = 
    print_letter (int_of_char 'a')

    
(*let () = ft_print_alphabet()*)