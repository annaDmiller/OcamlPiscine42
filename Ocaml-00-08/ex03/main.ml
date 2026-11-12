module Try :
sig
    type 'a t = 
        | Success of 'a
        | Failure of exn
    
    val return : 'a -> 'a t
    val bind : 'a t -> ('a -> 'b t) -> 'b t
    val recover : 'a t -> (exn -> 'a t) -> 'a t
    val filter : 'a t -> ('a -> bool) -> 'a t
    val flatten : 'a t t -> 'a t
end =
struct
    type 'a t = 
        | Success of 'a
        | Failure of exn

    let return (src : 'a) : 'a t = Success src

    let bind (src : 'a t) (fnc : ('a -> 'b t)) : 'b t =
        match src with
            | Failure e -> Failure e
            | Success x ->
                try
                    fnc x
                with
                    | e -> Failure e
    
    let recover (src : 'a t) (fnc : (exn -> 'a t)) : 'a t =
        match src with
            | Success _ -> src
            | Failure e -> fnc e

    exception Predicate_not_satisfied

    let filter (src : 'a t) (fnc : ('a -> bool)) : 'a t =
        match src with
            | Failure _ -> src
            | Success x ->
                  if fnc x then
                      src
                  else
                      Failure Predicate_not_satisfied

    let flatten (src : 'a t t) : 'a t =
        match src with
            | Success inner -> inner
            | Failure e -> Failure e
end

exception My_error of string

let describe (t : int Try.t) : string =
	match t with
	| Try.Success x -> Printf.sprintf "Success %d" x
	| Try.Failure (My_error msg) -> Printf.sprintf "Failure (My_error %S)" msg
	| Try.Failure e -> Printf.sprintf "Failure %S" (Printexc.to_string e)

let check (label : string) (b : bool) : unit =
	  print_endline ((if b then "[OK] " else "[KO] ") ^ label)

let () =
	(* return on a Success *)
	let ok = Try.return 10 in
	print_endline (describe ok);
  
	(* bind that raises exception, turning Success into Failure *)
    let divide a b = a / b in
	let bad = Try.bind ok (fun x -> Try.return (divide x 0)) in
	print_endline (describe bad);

	(* bind that succeeds *)
	let good = Try.bind ok (fun x -> Try.return (divide x 2)) in
	print_endline (describe good);

	(* recover from a Failure *)
	let recovered = Try.recover bad (fun _ -> Try.return (-1)) in
	print_endline (describe recovered);

	(* recover on an already-successful monad is a no-op *)
	let untouched = Try.recover good (fun _ -> Try.return (-1)) in
	print_endline (describe untouched);

	(* filter that fails the predicate *)
	let filtered_out = Try.filter ok (fun x -> x > 100) in
	print_endline (describe filtered_out);

	(* filter that passes the predicate *)
	let filtered_in = Try.filter ok (fun x -> x > 5) in
	print_endline (describe filtered_in);

	(* flatten a nested Success (Success ...) *)
	let nested_ok = Try.return (Try.return 42) in
	print_endline (describe (Try.flatten nested_ok));

	(* flatten a nested Success (Failure ...) -> Failure *)
	let nested_fail = Try.return bad in
	print_endline (describe (Try.flatten nested_fail));

	(* flatten a direct Failure *)
	let direct_fail : int Try.t Try.t = Try.Failure (My_error "outer failure") in
	print_endline (describe (Try.flatten direct_fail));

    print_newline();
    print_endline "Test of the monads rules";
    
    let incr (a : int) : int Try.t = Try.return (a + 1) in
    check "Left identity rule check"
        (Try.bind (Try.return 10) incr = incr 10);

    check "Right identity rule check on success"
        (Try.bind ok Try.return = ok);

    let failed : int Try.t = Try.Failure (My_error "test") in
    check "Right identity rule check on failed"
        (Try.bind failed Try.return = failed);

    let f (x : int) : int Try.t = Try.return (x + 1) in
    let g (x : int) : int Try.t = Try.return (x * 2) in
    let m = Try.return 10 in
    check "Associativity rule check"
        (Try.bind (Try.bind m f) g = Try.bind m (fun x -> Try.bind (f x) g))
    
