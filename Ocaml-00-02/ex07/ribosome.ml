(* ================ Code from Ex04 ================ *)

type phosphate = string
type deoxyribose = string

type nucleobase =
    | A
    | T
    | C
    | G
    | U
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
        | U -> "U"
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
        | U -> A
        | None -> None

let rec complementary_helix (orig : helix) : helix = 
    match orig with
        | [] -> []
        | nucleo :: rest ->
            { phosphate = "phosphate"; deoxyribose = "deoxyribose"; nucleobase = (complementary_nucleobase nucleo.nucleobase) }
            :: complementary_helix rest

(* ================ Code from Ex06 ================ *)

type rna = nucleobase list

let rna_transform_base (value : nucleobase) : nucleobase =
    match value with
        | T -> U
        | other -> other

let rec generate_rna (orig_hel : helix) : rna =
    match orig_hel with
        | [] -> []
        | value :: rest_hel -> 
            (rna_transform_base (complementary_nucleobase value.nucleobase))
            :: generate_rna rest_hel

let rec rna_to_string (orig : rna) =
    match orig with
        | [] -> ""
        | value :: rest -> (string_of_nucleobase value) ^ (rna_to_string rest)

(* ================ Code from Ex07 ================ *)

type aminoacid =
    | Stop
    | Ala
    | Arg
    | Asn
    | Asp
    | Cys
    | Gln
    | Glu
    | Gly
    | His
    | Ile
    | Leu
    | Lys
    | Met
    | Phe
    | Pro
    | Ser
    | Thr
    | Trp
    | Tyr
    | Val
    
type protein = aminoacid list

let rec generate_bases_triplets (orig_rna : rna) :
        (nucleobase * nucleobase * nucleobase) list =
    match orig_rna with
        | first :: sec :: third :: rest -> (first, sec, third) :: generate_bases_triplets rest
        | _ -> []

let aminoacid_of_triplet (trip : (nucleobase * nucleobase * nucleobase)) : aminoacid =
    match trip with
        | (U, A, A) | (U, A, G) | (U, G, A) -> Stop
        | (G, C, A) | (G, C, C) | (G, C, G) | (G, C, U) -> Ala
        | (A, G, A) | (A, G, G) | (C, G, A) | (C, G, C) | (C, G, G) | (C, G, U) -> Arg
        | (A, A, C) | (A, A, U) -> Asn
        | (G, A, C) | (G, A, U) -> Asp
        | (U, G, C) | (U, G, U) -> Cys
        | (C, A, A) | (C, A, G) -> Gln
        | (G, A, A) | (G, A, G) -> Glu
        | (G, G, A) | (G, G, C) | (G, G, G) | (G, G, U) -> Gly
        | (C, A, C) | (C, A, U) -> His
        | (A, U, A) | (A, U, C) | (A, U, U) -> Ile
        | (C, U, A) | (C, U, C) | (C, U, G) | (C, U, U) | (U, U, A) | (U, U, G) -> Leu
        | (A, A, A) | (A, A, G) -> Lys
        | (A, U, G) -> Met
        | (U, U, C) | (U, U, U) -> Phe
        | (C, C, C) | (C, C, A) | (C, C, G) | (C, C, U) -> Pro
        | (U, C, A) | (U, C, C) | (U, C, G) | (U, C, U) | (A, G, U) | (A, G, C) -> Ser
        | (A, C, A) | (A, C, C) | (A, C, G) | (A, C, U) -> Thr
        | (U, G, G) -> Trp
        | (U, A, C) | (U, A, U) -> Tyr
        | (G, U, A) | (G, U, C) | (G, U, G) | (G, U, U) -> Val
        | _ -> Stop

let decode_arn (orig_rna : rna) : protein =
    let rec decode_triplet_to_amino 
            (trip : (nucleobase * nucleobase * nucleobase) list) : protein = 
        match trip with
            | [] -> []
            | value :: rest ->
                let amin = aminoacid_of_triplet value in
                if amin = Stop then [] else amin :: decode_triplet_to_amino rest
    in
    decode_triplet_to_amino (generate_bases_triplets orig_rna)




let aminoacid_to_rna (orig : aminoacid) : string =
    match orig with
        | Stop -> ""
        | Ala -> "Ala (Alanine)"
        | Arg -> "Arg (Arginine)"
        | Asn -> "Asn (Asparagine)"
        | Asp -> "Asp (Aspartique)"
        | Cys -> "Cys (Cysteine)"
        | Gln -> "Gln (Glutamine)"
        | Glu -> "Glu (Glutamique)"
        | Gly -> "Gly (Glycine)"
        | His -> "His (Histidine)"
        | Ile -> "Ile (Isoleucine)"
        | Leu -> "Leu (Leucine)"
        | Lys -> "Lys (Lysine)"
        | Met -> "Met (Methionine)"
        | Phe -> "Phe (Phenylalanine)"
        | Pro -> "Pro (Proline)"
        | Ser -> "Ser (Serine)"
        | Thr -> "Thr (Threonine)"
        | Trp -> "Trp (Tryptophane)"
        | Tyr -> "Tyr (Typosine)"
        | Val -> "Val (Valine)"

let rec protein_to_string (orig : protein) : string = 
    match orig with
        | [] -> ""
        | value :: rest -> 
            if rest <> [] then
                (aminoacid_to_rna value) ^ "--" ^ protein_to_string rest
            else
                aminoacid_to_rna value

let () =
    Random.self_init ();
    let hel_example = generate_helix 10 in
    let m_rna = generate_rna hel_example
    in
    print_string "Original Helix : ";
    print_endline (helix_to_string hel_example);
    print_string "Complementary :  ";
    print_endline (helix_to_string (complementary_helix hel_example));
    print_string "RNA :            ";
    print_endline (rna_to_string m_rna);
    print_string "Protein :        ";
    print_endline (protein_to_string (decode_arn m_rna))
