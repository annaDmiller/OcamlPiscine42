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
