let check (label : string) (b : bool) : unit = 
    print_endline ((if b then "[OK] " else "[KO] ") ^ label)

let print_proj (src : App.project) : unit =
    let (name, status, grade) = src in
    print_endline ("(\"" ^ name ^ "\", \"" ^ status ^ "\", " ^ string_of_int grade ^ ")")

let rec for_all (f : 'a -> bool) (lst : 'a list) : bool =
    match lst with
        | [] -> true
        | x :: rest -> f x && for_all f rest

let projects : App.project list = [
    ("First", "fail", 40);
    ("Second", "succeed", 95);
    ("Third", "succeed", 85)
]

let () = 
    let p1 : App.project = ("Transcendece", "succeed", 92) in
    let p2 : App.project = ("ft_rt", "fail", 15) in
    let zero = App.zero in
	  print_string "zero                    -> "; print_proj zero;
	  print_string "project 1               -> "; print_proj p1;
	  print_string "project 2               -> "; print_proj p2;
	  print_string "combine p1 p2           -> "; print_proj (App.combine p1 p2);
	  print_string "combine p1 zero         -> "; print_proj (App.combine p1 zero);
	  print_string "fail p1                 -> "; print_proj (App.fail p1);
	  print_string "success p2              -> "; print_proj (App.success p2);
	  print_string "combine (success p2) p1 -> "; print_proj (App.combine (App.success p2) p1);
	  check "combine concatenates names and averages grades"
	  	  (App.combine p1 p2 = ("Transcendeceft_rt", "fail", 53));
	  check "average above 80 gives succeed"
	  	  (App.combine p1 ("X", "succeed", 100) = ("TranscendeceX", "succeed", 96));
	  check "average of exactly 80 gives failed"
	  	  (App.combine ("A", "", 80) ("B", "", 80) = ("AB", "fail", 80));
	  check "zero keeps the element's name the same"
	  	  (let (n, _, _) = App.combine p1 App.zero in n = "Transcendece");    

    check "combine function is associative for the project name"
    (
        for_all 
            (fun a -> for_all 
                (fun b -> for_all 
                    (fun c ->
                        let (p1, _, _) = App.combine (App.combine a b) c in
                        let (p2, _, _)= App.combine a (App.combine b c) in
                        p1 = p2)
                    projects)
                projects)
            projects
    )
