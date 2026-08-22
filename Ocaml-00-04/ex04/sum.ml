let sum (a : float) (b : float) : float =
    a +. b

let () =
    print_float (sum 1.0 2.0);
    print_newline ();
  
    print_float (sum (-1.5) 3.5);
    print_newline ();
  
    print_float (sum 0.0 0.0);
    print_newline ();
  
    print_float (sum 100.25 (-50.75));
    print_newline ()