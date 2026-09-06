let () =
  print_endline "=== The battle begins! ===";

    let amy = new People.people "Amy Pond" in
    let doc = new Doctor.doctor "The Doctor" 900 amy in
    let dalek = new Dalek.dalek in

    print_endline doc#to_string;
    print_endline dalek#to_string;

    doc#talk;
    dalek#talk;

    doc#use_sonic_screwdriver;
    doc#travel_in_time 2005 1963;
    print_endline doc#to_string;

    print_endline "=== The dalek strikes! ===";
    dalek#exterminate amy;
    print_endline amy#to_string;
    print_endline dalek#to_string;

    amy#die;
    dalek#die;

    print_endline "=== The battle ends. ==="
