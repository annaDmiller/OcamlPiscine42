module StringHashedType =
struct
    type t = string

    let equal key value =
        key = value

    (*DJB2 string hashing algorithm*)
    let hash str =
        let len = String.length str
        in
        let rec loop ind acc =
            if ind >= len then 
                acc
            else
                loop (ind + 1) (acc * 33 + Char.code (String.get str ind))
      in
      loop 0 5381
end

module StringHashtbl = Hashtbl.Make(StringHashedType)

let () =
    let hsh_tbl = StringHashtbl.create 5 in
    let values = [ "Hello"; "world"; "42"; "Ocaml"; "H" ] in
    let pairs = List.map (fun s -> (s, String.length s)) values 
    in
    List.iter (fun (key, value) -> StringHashtbl.add hsh_tbl key value) pairs;
    StringHashtbl.iter (fun key value -> Printf.printf "key = \"%s\", value = %d\n" key value) hsh_tbl