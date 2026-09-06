let () =
    print_endline "==== Testing the doctor class ====";
    let clara = new People.people "Clara" in
    let doc = new Doctor.doctor "The Doctor" 900 clara 
    in

    print_string "Doctor's to_string signal:  ";
    print_endline doc#to_string;
    print_string "Doctor's talk signal:  ";
    doc#talk;
    print_string "Doctor's use_sonic_screwdriver signal:  ";
    doc#use_sonic_screwdriver;

    (* Wound the Doctor a bit so we can later prove that travel_in_time
       really triggers the private regenerate method. *)
    print_endline "Now we will test regeneration and for that we will make a wound for the doctor so resulting hp is 40";
    doc#take_damage 60;
    print_endline doc#to_string;

    print_endline "Now we travel in time";
    doc#travel_in_time 2005 1963;
    print_endline doc#to_string