let rec print_string_list (lst : string list) =
    match lst with
        | [] -> ()
        | s :: rest ->
            print_endline s;
            print_string_list rest

let () =
    Random.self_init ();

    print_endline "newDeck (first shuffle)";
    let deck1 = Deck.newDeck ()
    in
    print_string_list (Deck.toStringList deck1);
    print_string "Deck size: ";
    print_int (List.length (Deck.toStringList deck1));
    print_newline ();
    print_newline ();

    print_endline "newDeck (second shuffle, should differ in order)";
    let deck2 = Deck.newDeck () 
    in
    print_string_list (Deck.toStringList deck2);
    print_newline ();

    print_endline "Check of the function toStringListVerbose (first 5 cards)";
    let rec take n lst =
        match (n, lst) with
            | 0, _ | _, [] -> []
            | n, x :: rest -> x :: take (n - 1) rest
    in
    print_string_list (take 5 (Deck.toStringListVerbose deck1));
    print_newline ();

    print_endline "Check of the function drawCard";
    let card, remaining = Deck.drawCard deck1
    in
    print_string "Drawn card: ";
    print_endline (Deck.Card.toStringVerbose card);
    print_string "Remaining deck size: ";
    print_int (List.length (Deck.toStringList remaining));
    print_newline ()

