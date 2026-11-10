module type MONOID =
sig
    type element
    val zero1 : element
    val zero2 : element
    val add : element -> element -> element
    val sub : element -> element -> element
    val mul : element -> element -> element
    val div : element -> element -> element
end 

module INT : MONOID with type element = int =
struct
    type element = int

    let zero1 = 0
    let zero2 = 1

    let add a b = a + b
    let sub a b = a - b
    let mul a b = a * b
    let div a b = a / b
end

module FLOAT : MONOID with type element = float =
struct
    type element = float

    let zero1 = 0.0
    let zero2 = 1.0

    let add a b = a +. b
    let sub a b = a -. b
    let mul a b = a *. b
    let div a b  = a /. b
end

module Calc : functor (M : MONOID) ->
sig
    val add : M.element -> M.element -> M.element
    val sub : M.element -> M.element -> M.element
    val mul : M.element -> M.element -> M.element
    val div : M.element -> M.element -> M.element
    val power : M.element -> int -> M.element
    val fact : M.element -> M.element
end = functor (M : MONOID) ->
struct
    let add a b = M.add a b
    let sub a b = M.sub a b
    let mul a b = M.mul a b
    let div a b = M.div a b
    
    let power el p = 
        let rec aux acc p_temp =
            if p_temp <= 0 then
                acc
            else
                aux (M.mul acc el) (p_temp - 1)
        in

        aux M.zero2 p

    let fact el = 
        let rec aux acc n =
            if n < M.zero2 then
                acc
            else
                aux (M.mul acc n) (M.sub n M.zero2)
        in

        aux M.zero2 el
end

module Calc_int = Calc(INT)
module Calc_float = Calc(FLOAT)

let check (label : string) (b : bool) : unit =
	  print_endline ((if b then "[OK] " else "[KO] ") ^ label)

let () = 
	print_endline (string_of_int (Calc_int.power 3 3));
	print_endline (string_of_float (Calc_float.power 3.0 3));
	print_endline (string_of_int (Calc_int.mul (Calc_int.add 20 1) 2));
	print_endline (string_of_float (Calc_float.mul (Calc_float.add 20.0 1.0) 2.0));
	print_endline (string_of_int (Calc_int.sub 10 4));
	print_endline (string_of_float (Calc_float.sub 10.0 4.0));
	print_endline (string_of_int (Calc_int.div 20 4));
	print_endline (string_of_float (Calc_float.div 20.0 4.0));
	print_endline (string_of_int (Calc_int.fact 5));
	print_endline (string_of_float (Calc_float.fact 5.0));
	print_endline (string_of_int (Calc_int.fact 0));
	print_endline (string_of_int (Calc_int.power 2 10));
	check "int power x 0 = 1" (Calc_int.power 7 0 = 1);
	check "int power 2 10 = 1024" (Calc_int.power 2 10 = 1024);
	check "float power 2.0 3 = 8.0" (Calc_float.power 2.0 3 = 8.0);
	check "int fact 0 = 1" (Calc_int.fact 0 = 1);
	check "int fact 1 = 1" (Calc_int.fact 1 = 1);
	check "int fact 10 = 3628800" (Calc_int.fact 10 = 3628800);
	check "float fact 5.0 = 120.0" (Calc_float.fact 5.0 = 120.0);
	check "int add/sub/mul/div" (Calc_int.add 2 3 = 5 && Calc_int.sub 2 3 = -1
		&& Calc_int.mul 2 3 = 6 && Calc_int.div 7 2 = 3);
	check "float add/sub/mul/div" (Calc_float.add 2.0 3.0 = 5.0 && Calc_float.sub 2.0 3.0 = -1.0
		&& Calc_float.mul 2.0 3.0 = 6.0 && Calc_float.div 7.0 2.0 = 3.5);
	check "zero1 is neutral for add (int, float)"
		(Calc_int.add INT.zero1 9 = 9 && Calc_float.add FLOAT.zero1 9.0 = 9.0);
	check "zero2 is neutral for mul (int, float)"
		(Calc_int.mul INT.zero2 9 = 9 && Calc_float.mul FLOAT.zero2 9.0 = 9.0);
    
    print_endline "=== Check of the assotiativity for the Calc_int ===";
    check "int add assitiativity" (Calc_int.add (Calc_int.add 2 3) 4 = Calc_int.add 2 (Calc_int.add 3 4));
    check "int mul assitiativity" (Calc_int.mul (Calc_int.mul 2 3) 4 = Calc_int.mul 2 (Calc_int.mul 3 4))
