let rec hfs_f num = 
    if num < 0 then
        -1
    else if num = 0 then
        1
    else
        num - hfs_m (hfs_f (num - 1))

and hfs_m num =
    if num < 0 then
        -1
    else if num = 0 then
        0
    else
        num - hfs_f (hfs_m (num - 1))


let () =
      print_endline "hfs_f 0 - expected 1";
      print_int (hfs_f 0);
      print_endline "";

      print_endline "hfs_m 0 - expected 0";
      print_int (hfs_m 0);
      print_endline "";

      print_endline "hfs_f (-2) - expected -1";
      print_int (hfs_f (-2));
      print_endline "";

      print_endline "hfs_m (-3) - expected -1";
      print_int (hfs_m (-3));
      print_endline "";

      print_endline "hfs_f 4 - expected 3";
      print_int (hfs_f 4);
      print_endline "";

      print_endline "hfs_m 4 - expected 2";
      print_int (hfs_m 4);
      print_endline ""
