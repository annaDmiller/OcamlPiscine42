let rec print_all_cards (cards : Card.t list) =
    match cards with
        | [] -> ()
        | c :: rest ->
                print_string (Card.toString c);
                print_string " -> ";
                print_endline (Card.toStringVerbose c);
                print_all_cards rest

let () =
    print_endline "=== Card.all (52 cards) ===";
    print_all_cards Card.all;
    print_string "Number of cards: ";
    print_int (List.length Card.all);
    print_newline ();
    print_newline ();

    print_endline "Check of the functions newCard / getValue / getColor";
    let c1 = Card.newCard Card.Value.King Card.Color.Heart
    in
    print_endline (Card.toString c1);
    print_endline (Card.Value.toStringVerbose (Card.getValue c1));
    print_endline (Card.Color.toStringVerbose (Card.getColor c1));
    print_newline ();

    print_endline "Check of the functions compare / max / min";
    let c2 = Card.newCard Card.Value.T5 Card.Color.Spade in
    let c3 = Card.newCard Card.Value.T5 Card.Color.Club 
    in
    print_string "Test 1: not equal (positive): compare c1 c2 = ";
    print_int (Card.compare c1 c2);
    print_newline ();
    print_string "Test 2: 1st card bigger (King of Heart): max c1 c2 = ";
    print_endline (Card.toString (Card.max c1 c2));
    print_string "Test 3: 2nd card lower (5 of Spade): min c1 c2 = ";
    print_endline (Card.toString (Card.min c1 c2));
    print_string "Test 4: they are equal (5 of Spade): max c2 c3 (equal value, first kept) = ";
    print_endline (Card.toString (Card.max c2 c3));
    print_newline ();

    print_endline "Check of the function best";
    print_endline ("King of Heart expected: " ^ Card.toString (Card.best [ c2; c1; c3 ]));
    (try
       let _ = Card.best [] 
        in
        print_endline "ERROR: should have raised"
    with Invalid_argument msg -> print_string "best [] raised: "; print_endline msg);
    print_newline ();

    print_endline "Check of the functions isOf / isSpade / isHeart / isDiamond / isClub";
    Printf.printf "isSpade c2 (expected true) = %b\n" (Card.isSpade c2);
    Printf.printf "isHeart c1 (expected true) = %b\n" (Card.isHeart c1);
    Printf.printf "isOf c3 Card.Color.Club (expected true) = %b\n" (Card.isOf c3 Card.Color.Club)