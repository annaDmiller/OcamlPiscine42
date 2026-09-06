let () =
  let h = new Atom.hydrogen () in
  let c = new Atom.carbon () in
  let o = new Atom.oxygen () in
  let he = new Atom.helium () in
  let li = new Atom.lithium () in
  let be = new Atom.beryllium () in

  print_endline "-- Atom descriptions --";
  List.iter print_endline
    [ h#to_string; c#to_string; o#to_string;
      he#to_string; li#to_string; be#to_string ];

  print_newline ();
  print_endline "-- Accessor check --";
  Printf.printf "hydrogen : name=%s symbol=%s atomic_number=%d\n"
    h#name h#symbol h#atomic_number;

  print_newline ();
  print_endline "-- Equality tests --";
  Printf.printf "hydrogen == hydrogen : %b\n" (h#equals (new Atom.hydrogen ()));
  Printf.printf "hydrogen == carbon   : %b\n" (h#equals c);
  Printf.printf "oxygen == oxygen     : %b\n" (o#equals (new Atom.oxygen ()))