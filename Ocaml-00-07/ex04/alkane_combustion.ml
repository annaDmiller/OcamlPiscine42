exception Not_balanced of string

let gcd a b =
    let rec aux a b = if b = 0 then a else aux b (a mod b)
    in
    aux (abs a) (abs b)

class alkane_combustion (alkanes: Alkane.alkane list) =
    let rec dedup lst accum = 
        match lst with
            | [] -> List.rev accum
            | a :: rest ->
                if List.exists (fun alk -> alk#equals (a :> Molecule.molecule)) accum then
                    dedup rest accum
                else
                    dedup rest (a :: accum)
    in

    let dedup_lst = dedup alkanes []
    in

    let single_balance (alk : Alkane.alkane) : (int * int * int * int) =
        let n = alk#size in
        if (3 * n + 1) mod 2 = 0 then
            (1, (3 * n + 1) / 2, n, n + 1)
        else
            (2, 3 * n + 1, 2 * n, 2 * (n + 1))
    in

    let combined_all = List.map (fun alk -> (alk, single_balance alk)) dedup_lst
    in

    let total_o2 = List.fold_left (fun acc (_, (_, coef_o2, _, _)) -> acc + coef_o2) 0 combined_all in
    let total_co2 = List.fold_left (fun acc (_, (_, _, coef_co2, _)) -> acc + coef_co2) 0 combined_all in
    let total_h2o = List.fold_left (fun acc (_, (_, _, _, coef_h2o)) -> acc + coef_h2o) 0 combined_all in

    let overall_gcd =
        List.fold_left
            (fun acc (_, (a, _, _, _)) -> gcd acc a)
            (gcd total_o2 (gcd total_co2 total_h2o))
            combined_all
    in
    let g = if overall_gcd = 0 then 1 else overall_gcd in
    
    let start_list = 
        List.map (fun (alk, (coef_alk, _, _, _)) -> ((alk :> Molecule.molecule), coef_alk)) combined_all
        @ [ (new Molecule.dioxygen(), total_o2 / g) ]
    in

    let result_list =
        [ new Molecule.carbon_dioxide(), total_co2 / g;
        new Molecule.water(), total_h2o / g]
    in
object (self)
    inherit Reaction.reaction start_list result_list

    method is_balanced = self#check_balance

    method get_start =
        if not self#is_balanced then
            raise (Not_balanced "alkane_combustion: reaction isn't balanced")
        else
            List.sort (fun (m1, _) (m2, _) -> compare m1#formula m2#formula) start_list

    method get_result =
        if not self#is_balanced then
            raise (Not_balanced "alkane_combustion: reaction isn't balanced")
        else
            List.sort (fun (m1, _) (m2, _) -> compare m1#formula m2#formula) result_list
    
    method balance = (new alkane_combustion dedup_lst :> Reaction.reaction)
end