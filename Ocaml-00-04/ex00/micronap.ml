let my_sleep () =
    Unix.sleep 1

let main (argc : int) (argv : string array) =
    if argc <> 2 then
        exit 1
    else
        begin
            try
                let num = int_of_string argv.(1)
                in
                if num < 0 then
                    exit 1
                else
                    for _ = 1 to num do
                        my_sleep ()
(*                        print_int ind;
                        print_newline ()*)
                    done
              with
                  | Failure _ -> exit 1
        end

let () = 
    let argc = Array.length Sys.argv
    in
    main argc Sys.argv