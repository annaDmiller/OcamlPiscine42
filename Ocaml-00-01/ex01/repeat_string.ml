let rec repeat_string ?(str="x") n = 
    if n < 0 then
        "Error"
    else if n = 0 then
        ""
    else
        str ^ repeat_string ~str:str (n - 1)


let () =
      print_endline "repeat_string (-1)";
      print_endline (repeat_string (-1));

      print_endline "repeat_string ~str:'Bye' 0";
      print_endline (repeat_string ~str:"Bye" 0);

      print_endline "repeat_string ~str:'Hello' 5";
      print_endline (repeat_string ~str:"Hello" 5);

      print_endline "repeat_string 5";
      print_endline (repeat_string 5)