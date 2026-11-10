module Set :
sig
    type 'a t

    val return : 'a -> 'a t
    val bind : 'a t -> ('a -> 'b t) -> 'b t
    val union : 'a t -> 'a t -> 'a t
    val inter : 'a t -> 'a t -> 'a t
    val diff : 'a t -> 'a t -> 'a t
    val filter : 'a t -> ('a -> bool) -> 'a t
    val foreach : 'a t -> ('a -> unit) -> unit
    val for_all : 'a t -> ('a -> bool) -> bool
    val exists : 'a t -> ('a -> bool) -> bool

    (* additional functions for testing purpose *)
    val of_list : 'a list -> 'a t
	  val to_list : 'a t -> 'a list
end =
struct
    type 'a t = 'a list

    let return (value : 'a) : 'a t = [ value ]

    let add_unique (value : 'a) (acc : 'a t) : 'a t =
        if List.mem value acc then
            acc
        else
            value :: acc

    let union (base : 'a t) (add : 'a t) : 'a t =
		    List.fold_left (fun acc x -> add_unique x acc) base add

    let bind (src_set : 'a t) (fnc : 'a -> 'b t) : 'b t =
        List.fold_left (fun acc x -> union acc (fnc x)) [] src_set

    let exists (src : 'a t) (predicat : 'a -> bool) : bool =
        List.exists predicat src
    
    let filter (src : 'a t) (predicat : 'a -> bool) : 'a t =
        List.filter predicat src

    let inter (s1 : 'a t) (s2 : 'a t) : 'a t =
        List.filter (fun x -> List.mem x s2) s1

    let diff (s1 : 'a t) (s2 : 'a t) : 'a t =
        List.filter (fun x -> not (List.mem x s2)) s1

    let foreach (src : 'a t) (fnc : 'a -> unit) : unit =
        List.iter fnc src

    let for_all (src : 'a t) (predicat : 'a -> bool) : bool =
        List.for_all predicat src

    (* additional functions for testin purpose *)
	  let rec of_list (lst : 'a list) : 'a t =
	  	  match lst with
	  	      | [] -> []
	  	      | x :: rest -> add_unique x (of_list rest)

	  let to_list (s : 'a t) : 'a list =
	  	  s
end

let print_int_set (label : string) (s : int Set.t) : unit =
	  print_string label;
	  print_string " = { ";
	  Set.foreach s (fun x -> Printf.printf "%d " x);
	  print_endline "}"

let () = 
	let s1 = Set.of_list [ 1; 2; 3; 2; 1 ] in
	let s2 = Set.of_list [ 3; 4; 5 ] in

	print_int_set "s1 (deduplicated)" s1;
	print_int_set "s2" s2;
	print_int_set "return 42" (Set.return 42);

	print_int_set "union s1 s2" (Set.union s1 s2);
	print_int_set "inter s1 s2" (Set.inter s1 s2);
	print_int_set "diff s1 s2" (Set.diff s1 s2);
	print_int_set "diff s2 s1" (Set.diff s2 s1);

	print_int_set "filter s1 even" (Set.filter s1 (fun x -> x mod 2 = 0));

	Printf.printf "for_all s1 (< 10) = %b\n" (Set.for_all s1 (fun x -> x < 10));
	Printf.printf "for_all s1 (< 3)  = %b\n" (Set.for_all s1 (fun x -> x < 3));
	Printf.printf "exists s2 (= 4)   = %b\n" (Set.exists s2 (fun x -> x = 4));
	Printf.printf "exists s2 (= 99)  = %b\n" (Set.exists s2 (fun x -> x = 99));

	(* bind: for each x in s1, produce {x; x*10}, then union everything *)
	let bound = Set.bind s1 (fun x -> Set.of_list [ x; x * 10 ]) in
	print_int_set "bind s1 (fun x -> {x; x*10})" bound
