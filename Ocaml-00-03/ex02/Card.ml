module Color = 
struct
    type t = Spade | Heart | Diamond | Club
    
    let all = [ Spade;  Heart; Diamond; Club ]
    
    let toString (value : t) : string =
        match value with
            | Spade -> "S"
            | Heart -> "H"
            | Diamond -> "D"
            | Club -> "C"
    
    let toStringVerbose (value : t) : string =
        match value with
            | Spade -> "Spade"
            | Heart -> "Heart"
            | Diamond -> "Diadmond"
            | Club -> "Club"
end

module Value = 
struct
    type t = T2 | T3 | T4 | T5 | T6 | T7 | T8 | T9 | T10 | Jack | Queen | King | As

    let all = [ T2; T3; T4; T5; T6; T7; T8; T9; T10; Jack; Queen; King; As ]

    let toInt (value : t) : int =
        match value with
            | T2 -> 1
            | T3 -> 2
            | T4 -> 3
            | T5 -> 4
            | T6 -> 5
            | T7 -> 6
            | T8 -> 7
            | T9 -> 8
            | T10 -> 9
            | Jack -> 10
            | Queen -> 11
            | King -> 12
            | As -> 13
    
    let toString (value : t) : string =
        match value with
            | T2 -> "2"
            | T3 -> "3"
            | T4 -> "4"
            | T5 -> "5"
            | T6 -> "6"
            | T7 -> "7"
            | T8 -> "8"
            | T9 -> "9"
            | T10 -> "10"
            | Jack -> "J"
            | Queen -> "Q"
            | King -> "K"
            | As -> "A"
    
    let toStringVerbose (value : t) : string =
        match value with
            | T2 -> "2"
            | T3 -> "3"
            | T4 -> "4"
            | T5 -> "5"
            | T6 -> "6"
            | T7 -> "7"
            | T8 -> "8"
            | T9 -> "9"
            | T10 -> "10"
            | Jack -> "Jack"
            | Queen -> "Queen"
            | King -> "King"
            | As -> "As"
    
    let next (curr_value : t) : t =
        match curr_value with
            | T2 -> T3
            | T3 -> T4
            | T4 -> T5
            | T5 -> T6
            | T6 -> T7
            | T7 -> T8
            | T8 -> T9
            | T9 -> T10
            | T10 -> Jack
            | Jack -> Queen
            | Queen -> King
            | King -> As
            | As -> invalid_arg "Value.next: As has no next value"
    
    let previous (curr_value : t) : t =
        match curr_value with
            | T2 -> invalid_arg "Value.previous: T2 has no previous value"
            | T3 -> T2
            | T4 -> T3
            | T5 -> T4
            | T6 -> T5
            | T7 -> T6
            | T8 -> T7
            | T9 -> T8
            | T10 -> T9
            | Jack -> T10
            | Queen -> Jack
            | King -> Queen
            | As -> King
end

type t = {
    cardValue : Value.t; 
    cardColor : Color.t
}

let newCard (value : Value.t) (color : Color.t) =
    {cardValue = value; cardColor = color}

let getValue (card : t) : Value.t = 
    card.cardValue

let getColor (card : t) : Color.t = 
    card.cardColor

let allOfColor (color : Color.t) : t list =
    List.map (fun v -> newCard v color) Value.all
let allSpades = allOfColor Color.Spade
let allHearts = allOfColor Color.Heart
let allDiamonds = allOfColor Color.Diamond
let allClubs = allOfColor Color.Club

let all = allSpades @ allHearts @ allDiamonds @ allClubs

let toString (card : t) : string =
    Printf.sprintf "%s %s" (Value.toString card.cardValue) (Color.toString card.cardColor)

let toStringVerbose (card : t) : string =
    Printf.sprintf "Card(%s, %s)"
        (Value.toStringVerbose card.cardValue)
        (Color.toStringVerbose card.cardColor)

let compare (c1 : t) (c2 : t) : int =
    if c1.cardValue = c2.cardValue then
        0
    else
        (Value.toInt c1.cardValue - Value.toInt c2.cardValue)

let max (c1 : t) (c2 : t) : t = 
    if compare c1 c2 < 0 then 
        c2 
    else 
        c1

let min (c1 : t) (c2 : t) : t =
    if compare c1 c2 > 0 then 
        c2 
    else 
        c1

let best (lst : t list) : t =
    match lst with
        | [] -> invalid_arg "Card.best: cannot get the best card of an empty list"
        | car :: rest -> List.fold_left (fun acc card -> max acc card) car rest

let isOf (card : t) (color : Color.t) : bool = 
    card.cardColor = color
let isSpade (card : t) : bool = isOf card Color.Spade
let isHeart (card : t) : bool = isOf card Color.Heart
let isDiamond (card : t) : bool = isOf card Color.Diamond
let isClub (card : t) : bool = isOf card Color.Club