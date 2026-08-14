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

let print_nucleotide (value : nucleotide) =
    print_string "(";
    print_string value.phosphate;
    print_string ", ";
    print_string value.deoxyribose;
    print_string ", ";
    print_string (string_of_nucleobase value.nucleobase);
    print_endline ")"

let () =
    let test value =
        print_string "generate_nucleotide ";
        print_char value;
        print_string " = ";
        print_nucleotide (generate_nucleotide value)
    in
    test 'A';
    test 'C';
    test 'T';
    test 'G';
    test 'X';
    test 'a'
