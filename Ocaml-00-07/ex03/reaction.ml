class virtual reaction
    (start_molecules : (Molecule.molecule * int) list)
    (result_molecules : (Molecule.molecule * int) list) =
object (self)
    val start_molecules = start_molecules
    val result_molecules = result_molecules

    method virtual get_start : (Molecule.molecule * int) list
    method virtual get_result : (Molecule.molecule * int) list
    method virtual balance : reaction
    method virtual is_balanced : bool

    method private count_atoms (side : (Molecule.molecule * int) list) : (string * int) list =
        let add acc symbol amount =
            match List.assoc_opt symbol acc with
                | Some count -> (symbol, count + amount) :: List.remove_assoc symbol acc
                | None -> (symbol, amount) :: acc
        in
        List.fold_left 
            (fun curr_acc_mol (mol, quant) -> List.fold_left 
                                              (fun curr_acc_a at -> add curr_acc_a at#symbol quant)
                                              curr_acc_mol mol#atom_list)
            [] side

    method private check_balance =
        let normalize sym_lst = List.sort compare sym_lst
        in
        normalize (self#count_atoms start_molecules) = normalize (self#count_atoms result_molecules)

end

(* ----------------------------------------------------------------------------- *)
(* It's Hell below. It's not me but the AI wrote, I hardly can even understand the algo ... *)

(*GCD - Greatest Common Divisor = НОД*)
let rec gcd (a : int) (b : int) : int =
    if b = 0 then 
        abs a 
    else 
        gcd b (a mod b)

(*Simplifies the list of integers by dividing every element by gcd of elements.*)
let normalize (row : int list) : int list =
    let gcd_val = List.fold_left gcd 0 row
    in
    if gcd_val = 0 then 
        row
    else
        List.map (fun x -> x / gcd_val) row

(* Gauss-Jordan elimination over the integers. Returns (pivot column, row)
   pairs; every pivot column is zero in all the other rows. *)
let rec eliminate (rows : (int list) list) (curr_col : int)
    (num_cols : int) (pivots : (int * int list) list) =
    if curr_col >= num_cols then 
        pivots
    else
        match List.partition (fun row -> List.nth row curr_col <> 0) rows with
            | [], _ -> eliminate rows (curr_col + 1) num_cols pivots
            | p :: others_nz, zeros ->
                let pv = List.nth p curr_col
                in
                let reduce r =
                    let rv = List.nth r curr_col
                    in
                    normalize (List.map2 (fun a b -> a * pv - b * rv) r p)
                in
                let pivots' = (curr_col, p) :: List.map (fun (c, r) -> (c, reduce r)) pivots
                in
                eliminate (List.map reduce (others_nz @ zeros)) (curr_col + 1) num_cols pivots'


let rec split (num : int) (lst : (Molecule.molecule * int) list)
    : ((Molecule.molecule * int) list * (Molecule.molecule * int) list) =
    if num = 0 then
        ([], lst)
    else match lst with
        | [] -> ([], [])
        | x :: rest -> 
            let (a, b) = split (num - 1) rest
            in
            (x :: a, b)
         
let rec range a b = if a >= b then [] else a :: range (a + 1) b

let rec unique l =
  match l with
  | [] -> []
  | x :: rest -> x :: unique (List.filter (fun y -> y <> x) rest)

let count_symbol sym (m : Molecule.molecule) =
  List.length (List.filter (fun a -> a#symbol = sym) m#atom_list)

(* Smallest positive integer coefficients of the molecules [molecules], the first
   [num_mol_start] of which are on the left side. *)
let solve (molecules : Molecule.molecule list) (num_mol_start : int)
    : int list =
    let ncols = List.length molecules in
    let symbols =
    unique (List.concat_map (fun (m : Molecule.molecule) ->
        List.map (fun a -> a#symbol) m#atom_list) molecules)
  in
  let row_of sym =
    List.mapi (fun i m ->
        if i < num_mol_start then count_symbol sym m else - (count_symbol sym m)) molecules
  in
  let pivots = eliminate (List.map row_of symbols) 0 ncols [] in
  let pivot_cols = List.map fst pivots in
  match List.filter (fun c -> not (List.mem c pivot_cols)) (range 0 ncols) with
  | [ free ] ->
    let prod = List.fold_left (fun acc (c, r) -> acc * abs (List.nth r c)) 1 pivots in
    let value_of col =
      if col = free then prod
      else
        let (c, r) = List.find (fun (c, _) -> c = col) pivots in
        - (List.nth r free) * prod / (List.nth r c)
    in
    let sol = normalize (List.map value_of (range 0 ncols)) 
    in
    let sol = 
        if List.exists (fun x -> x < 0) sol then 
            List.map (fun x -> -x) sol
        else
            sol
    in
    if List.exists (fun x -> x <= 0) sol then
        failwith "balance: no positive solution"
    else sol
        | [] -> failwith "balance: the reaction can't be balanced"
        | _ -> failwith "balance: the solution isn't unique"

class test_reaction start result =
object (self)
    inherit reaction start result

    method get_start = start
    method get_result = result
    method is_balanced = self#check_balance

    method balance = 
        let all_molecules = List.map fst start @ List.map fst result in (*get rid of old coefs*)
        let num_mol_start = List.length start in (*count how much molecules (without coefs) we have in start list*)
        let coefs = solve all_molecules num_mol_start in (*resolve the coefs problem*)
        let (s, r) = split num_mol_start (List.combine all_molecules coefs) in (*divide the combined list of pairs molecule+coef according to the number of molecules in the start list*)
        if self#is_balanced then
            (self :> reaction)
        else
            (new test_reaction s r :> reaction)
end
