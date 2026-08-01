let rec tak x y z =
    if y < x then
        tak (tak (x - 1) y z) (tak (y - 1) z x) (tak (z - 1) x y)   
    else
        z

let () = 
      print_endline "tak 1 2 3 - expected 3";
      print_int (tak 1 2 3);
      print_endline "";

      print_endline "tak 5 23 7 - expected 7";
      print_int (tak 5 23 7);
      print_endline "";

      print_endline "tak 9 1 0 - expected 1";
      print_int (tak 9 1 0);
      print_endline "";

      print_endline "tak 1 1 1 - expected 1";
      print_int (tak 1 1 1);
      print_endline "";

      print_endline "tak 0 42 0 - expected 0";
      print_int (tak 0 42 0);
      print_endline "";

      print_endline "tak 5 2 7 - expected 5";
      print_int (tak 5 2 7);
      print_endline ""
