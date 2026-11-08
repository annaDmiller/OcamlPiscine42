module Watchtower : 
sig
    type hour = int
    val zero : hour
    val add : hour -> hour -> hour
    val sub : hour -> hour -> hour
end = 
struct
    type hour = int

    let zero : hour = 12

    let normalize (num : int) : hour =
        let h = num mod 12
        in
        if h <= 0 then
            h + zero
        else
            h
        
    let add (x : hour) (y : hour) : hour = normalize (x + y)
        
    let sub (x : hour) (y : hour) : hour = normalize (x - y)
end

let show (label : string) (h : Watchtower.hour) : unit =
    print_endline (label ^ " -> " ^ string_of_int h)

let check (label : string) (b : bool) : unit = 
    print_endline ((if b then "[OK] " else "[KO] ") ^ label)

let rec for_all (f : 'a -> bool) (lst : 'a list) : bool =
    match lst with
        | [] -> true
        | x :: rest -> f x && for_all f rest

let () =
    let add = Watchtower.add and sub = Watchtower.sub in
    show "zero" Watchtower.zero;
    show "add 5 3   (expect 8)" (add 5 3);
    show "add 10 5  (expect 3)" (add 10 5);
    show "add 6 6   (expect 12)" (add 6 6);
    show "sub 5 3   (expect 2)" (sub 5 3);
    show "sub 3 5   (expect 10)" (sub 3 5);
    show "sub 12 12 (expect 12)" (sub 12 12);
    show "add 100 37 (expect 5)" (add 100 37);

    let hours =
        let rec concat acc h =
            if h > 12 then acc
            else concat (h::acc) (h + 1)
        in

        concat [] 1
    in
    let zero = Watchtower.zero in
    check "zero is a left and right identity"
        (for_all (fun h -> add zero h = h && add h zero = h) hours);

    check "add function is associative"
    (
        for_all 
            (fun a -> for_all 
                (fun b -> for_all 
                    (fun c -> add (add a b) c = add a (add b c)) 
                    hours) 
                hours) 
            hours
    )