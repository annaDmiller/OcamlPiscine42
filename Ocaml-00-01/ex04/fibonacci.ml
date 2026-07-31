let rec fibonacci num = 
    if num = 0 then
        0
    else if num = 1 then
        1
    else if num > 1 then
        fibonacci (num - 2) + fibonacci (num - 1)
    else
        -1
    

let () =
      print_endline "fibonacci (-3) - expected -1";
      print_int (fibonacci (-3));
      print_endline "";

      print_endline "fibonacci 0 - expected 0";
      print_int (fibonacci 0);
      print_endline "";

      print_endline "fibonacci 1 - expected 1";
      print_int (fibonacci 1);
      print_endline "";

      print_endline "fibonacci 3 - expected 2";
      print_int (fibonacci 3);
      print_endline "";

      print_endline "fibonacci 15 - expected 610";
      print_int (fibonacci 15);
      print_endline ""