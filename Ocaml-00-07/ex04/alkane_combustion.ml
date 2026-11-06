exception Reaction_not_balanced of string

let rec gcd a b =
    if b = 0 then 
        abs a
    else 
        gcd b (a mod b)

let group_alkanes (alkanes : Alkane.alkane list) =
    let grouped =
        List.fold_left
            (fun acc alkane ->
                let rec add_or_increment lst =
                    match lst with
                        | [] -> [(alkane, 1)]
                        | (existing, count) :: tail ->
                            if existing#size = alkane#size then
                                (existing, count + 1) :: tail
                            else
                                (existing, count) :: add_or_increment tail
                in
                add_or_increment acc)
            []
            alkanes
    in
    List.sort
        (fun (a, _) (b, _) -> compare a#size b#size)
        grouped


let combustion_coefficients grouped =
    let initial_coefficients =
        List.map
            (fun (alkane, _) -> (alkane, 1))
            grouped
    in

    let oxygen_atoms =
        List.fold_left
            (fun acc (alkane, coefficient) ->
                acc + coefficient * (3 * alkane#size + 1))
            0
            initial_coefficients
    in

    let alkane_coefficients =
        if oxygen_atoms mod 2 = 0 then
            initial_coefficients
        else
            let rec increment_smallest_even = function
                | [] ->
                    failwith "No even-sized alkane found"

                | (alkane, coefficient) :: tail ->
                    if alkane#size mod 2 = 0 then
                        (alkane, coefficient + 1) :: tail
                    else
                        (alkane, coefficient) ::
                        increment_smallest_even tail
            in
            increment_smallest_even initial_coefficients
    in

    let (oxygen_atoms, carbon_atoms, water_molecules) =
        List.fold_left
            (fun (oa, ca, wm) (alkane, coefficient) ->
                let n = alkane#size 
                in
                (
                    oa + coefficient * (3 * n + 1),
                    ca + coefficient * n,
                    wm + coefficient * (n + 1)
                )
            )
            (0, 0, 0)
            alkane_coefficients
    in

    let oxygen = oxygen_atoms / 2 in
    let carbon_dioxide = carbon_atoms in
    let water = water_molecules in

    (alkane_coefficients, oxygen, carbon_dioxide, water)


class alkane_combustion ?(balanced_data = None) (alkanes : Alkane.alkane list) =
    let (starts, results) =
        if alkanes = [] then
            invalid_arg "At least one alkane is required"
        else (
            match balanced_data with
                | None -> 
                    (List.map
                        (fun alkane -> ((alkane :> Molecule.molecule), 1))
                        alkanes
                    @ [ ((new Molecule.dioxygen () :> Molecule.molecule), 1) ],
    
                    [ ((new Molecule.carbon_dioxide () :> Molecule.molecule), 1);
                    ((new Molecule.water () :> Molecule.molecule), 1) ]
                )
    
                | Some (alkane_coefficients, oxygen, co2, water) ->
                    (List.map
                        (fun (alkane, coefficient) -> ((alkane :> Molecule.molecule), coefficient))
                        alkane_coefficients
                    @ [ ((new Molecule.dioxygen () :> Molecule.molecule), oxygen) ],
    
                    [ ((new Molecule.carbon_dioxide () :> Molecule.molecule), co2);
                    ((new Molecule.water () :> Molecule.molecule), water) ]
                )
        )
    in
object (self)
    inherit Reaction.reaction starts results

    method get_start : (Molecule.molecule * int) list =
        if self#check_balance then 
            starts
        else 
            raise (Reaction_not_balanced "The reaction not balanced. Can't give you start molecules.")

    method get_result : (Molecule.molecule * int) list =
        if self#check_balance then 
            results
        else 
            raise (Reaction_not_balanced "The reaction not balanced. Can't give you result molecules.")

    method is_balanced : bool = self#check_balance

    method balance : alkane_combustion =
        let grouped = group alkanes
        in
        let coefficients = combustion_coefficients grouped
        in

        new alkane_combustion
            ~balanced_data:(Some coefficients)
            alkanes
end