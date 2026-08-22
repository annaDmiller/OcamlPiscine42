let () =
    Random.self_init ();
    let jokes = [|
        "My grandfather has the heart of a lion… and a lifetime ban from the zoo.";
        "What’s the difference between a snowman and a snowwoman? Snowballs.";
        "The man who invented autocorrect has died. Restaurant in peace.";
        "What’s the best thing about being dead? You don’t have to pay taxes.";
        "I told my wife she should embrace her mistakes. She gave me a hug."
    |] in
    let ind = Random.int (Array.length jokes)
    in
    print_string jokes.(ind);
    print_newline ()