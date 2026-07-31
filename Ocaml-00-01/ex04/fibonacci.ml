let fibonacci num = 
    let rec revers ind res_ind next_val = 
        if ind = num then
            res_ind
        else
            revers (ind + 1) next_val (res_ind + next_val)
      in
      if num < 0 then
          -1
      else
          revers 0 0 1

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