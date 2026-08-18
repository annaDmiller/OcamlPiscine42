type 'a ft_ref = { mutable content : 'a }

let return (value : 'a) : 'a ft_ref =
    {content = value}

let get (my_ref : 'a ft_ref) : 'a =
    my_ref.content

let set (my_ref : 'a ft_ref) (value : 'a) =
    my_ref.content <- value

let bind (my_ref : 'a ft_ref) (func : 'a -> 'b ft_ref) : 'b ft_ref =
    func (get my_ref)

let () =
    print_endline "Check of the ft_ref type and get function";

    let temp = return 42
    in
    print_string "Value of the ft_ref 42 => ";
    print_int @@ get temp;
    print_newline ();
    print_newline ();

    print_endline "Check of the function set";
    set temp 24;
    print_string "New value of the ft_ref => ";
    print_int @@ get temp;
    print_newline ();
    print_newline ();

    print_endline "Check of the ft_ref for string";
    let str = return "Hi"
    in
    print_string "Value of the string ft_ref => \"";
    print_string @@ get str;
    print_endline "\"";
    print_newline ();

    print_endline "Check of the function bind";
    let new_str = bind str (function x -> return (x ^ " you :)"))
    in
    print_string "New value of the string ft_ref => \"";
    print_string @@ get new_str;
    print_endline "\""