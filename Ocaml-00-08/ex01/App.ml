type project = string * string * int

let zero = ("", "", 0)

let combine (first : project) (second : project) : project =
    let (p1_name, _, p1_grade) = first in
    let (p2_name, _, p2_grade) = second in
    let av_grade = (p1_grade + p2_grade) / 2 in
    let status = if av_grade > 80 then "succeed" else "fail" in

    (p1_name ^ p2_name, status, av_grade)

let fail (src : project) : project =
    let (name, _, _) = src in

    (name, "fail", 0)

let success (src : project) : project =
    let (name, _, _) = src in

    (name, "succeed", 80)