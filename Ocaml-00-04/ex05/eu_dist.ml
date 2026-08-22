let eu_dist (a : float array) (b : float array) : float =
    let arr_size = Array.length a in
    let dist = ref 0.0 
    in
    for ind = 0 to arr_size - 1 do
        let diff = a.(ind) -. b.(ind)
        in
        dist := !dist +. (diff *. diff)
    done;
    sqrt !dist

let () =
    let a = [| 0.0; 0.0 |] in
    let b = [| 3.0; 4.0 |]
    in
    Printf.printf "eu_dist [0;0] [3;4] (expected 5) = %f\n" (eu_dist a b);
    print_newline ();

    let c = [| 1.0; 2.0; 3.0 |] in
    let d = [| 4.0; 6.0; 8.0 |]
    in
    Printf.printf "eu_dist [1;2;3] [4;6;8] (expected ~7.071) = %f\n" (eu_dist c d);
    print_newline ();

    let e = [| 1.0 |] in
    let f = [| 1.0 |]
    in
    Printf.printf "eu_dist [1] [1] (expected 0) = %f\n" (eu_dist e f)