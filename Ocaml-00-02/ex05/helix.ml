(* ================ Code from Ex04 ================ *)

type phosphate = string
type deoxyribose = string

type nucleobase =
    | A
    | T
    | C
    | G
    | None

type nucleotide = {
    phosphate : phosphate;
    deoxyribose : deoxyribose;
    nucleobase :nucleobase;
}

let generate_nucleotide (c : char) : nucleotide =
    let base =
        match c with
            | 'A' -> A
            | 'T' -> T
            | 'C' -> C
            | 'G' -> G
            | _ -> None
    in
    { phosphate = "phosphate"; deoxyribose = "deoxyribose"; nucleobase = base }

let string_of_nucleobase (value : nucleobase) : string =
    match value with
        | A -> "A"
        | T -> "T"
        | C -> "C"
        | G -> "G"
        | None -> "None"

(* ================ Code for Ex05 ================ *)

type helix = nucleotide list

let random_base_char () : char =
    match Random.int 4 with
        | 0 -> 'A'
        | 1 -> 'T'
        | 2 -> 'C'
        | _ -> 'G'

let rec generate_helix (size : int) : helix =
    if size <= 0 then
        []
    else
        generate_nucleotide (random_base_char ()) :: generate_helix (size - 1)

let rec helix_to_string (lst : helix) : string =
    match lst with
        | [] -> ""
        | nucleo :: rest -> (string_of_nucleobase nucleo.nucleobase) ^ (helix_to_string rest)

let complementary_nucleobase (value : nucleobase) : nucleobase = 
    match value with
        | A -> T
        | T -> A
        | C -> G
        | G -> C
        | None -> None

let rec complementary_helix (orig : helix) : helix = 
    match orig with
        | [] -> []
        | nucleo :: rest ->
            { phosphate = "phosphate"; deoxyribose = "deoxyribose"; nucleobase = (complementary_nucleobase nucleo.nucleobase) }
            :: complementary_helix rest




let () =
    Random.self_init ();
    let hel_example = generate_helix 10
    in
    print_string "Original Helix : ";
    print_endline (helix_to_string hel_example);
    print_string "Complementary :  ";
    print_endline (helix_to_string (complementary_helix hel_example))

