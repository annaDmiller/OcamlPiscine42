module type FRACTIONNAL_BITS = 
sig
    val bits : int
end

module type FIXED = 
sig
    type t
    val of_float  : float -> t
    val of_int    : int   -> t
    val to_float  : t     -> float
    val to_int    : t     -> int
    val to_string : t     -> string
    val zero : t
    val one  : t
    val succ : t -> t
    val pred : t -> t
    val min : t -> t -> t
    val max : t -> t -> t
    val gth : t -> t -> bool
    val lth : t -> t -> bool
    val gte : t -> t -> bool
    val lte : t -> t -> bool
    val eqp : t -> t -> bool (** physical equality  *)
    val eqs : t -> t -> bool (** structural equality *)
    val add : t -> t -> t
    val sub : t -> t -> t
    val mul : t -> t -> t
    val div : t -> t -> t
    val foreach : t -> t -> (t -> unit) -> unit
end

module type MAKE = functor (B : FRACTIONNAL_BITS) -> FIXED

module Make : MAKE =
    functor (B : FRACTIONNAL_BITS) ->
    struct
      (* A fixed point number is stored as a plain int : the value multiplied
         by 2^bits ("scale"). The B.bits lowest bits represent the fractional
         part. *)
      type t = int

      let scale = 1 lsl B.bits

      let of_float f =
          int_of_float (f *. float_of_int scale)
      let of_int i = 
          i * scale
      let to_float t = 
          float_of_int t /. float_of_int scale
      let to_int t = 
          t / scale
      let to_string t = 
          string_of_float (to_float t)

      let zero = 0
      let one = scale

      let succ t =
          t + one
      let pred t = 
          t - one

      let min a b =
          if a < b then
              a 
          else
              b
      let max a b = 
          if a > b then
              a
          else 
              b

      let gth a b = a > b
      let lth a b = a < b
      let gte a b = a >= b
      let lte a b = a <= b

      let eqp a b = a == b
      let eqs a b = a = b

      let add a b = a + b
      let sub a b = a - b
      let mul a b = (a * b) / scale
      let div a b = (a * scale) / b

      (* Iterates raw-value by raw-value from `start` to `stop`, inclusive. *)
      let foreach start stop func =
          let rec loop cur =
              if cur <= stop then (
                  func cur;
                  loop (cur + 1)
              )
              else
                  ()
        in
        loop start
  end

module Fixed4 : FIXED = Make (struct let bits = 4 end)
module Fixed8 : FIXED = Make (struct let bits = 8 end)

let () =
    let x8 = Fixed8.of_float 21.10 in
    let y8 = Fixed8.of_float 21.32 in
    let r8 = Fixed8.add x8 y8
    in
    print_endline (Fixed8.to_string r8);
    Fixed4.foreach (Fixed4.zero) (Fixed4.one) (fun f -> print_endline (Fixed4.to_string f));

    print_endline "==== FIXED tests (Fixed8) ====";
    let a = Fixed8.of_int 3 in
    let b = Fixed8.of_float 1.5 in
    Printf.printf "of_int a = 3          = %s\n" (Fixed8.to_string a);
    Printf.printf "of_float a = 1.5      = %s\n" (Fixed8.to_string b);
    Printf.printf "to_int a          = %d\n" (Fixed8.to_int a);
    Printf.printf "to_float a        = %f\n" (Fixed8.to_float a);
    Printf.printf "zero              = %s\n" (Fixed8.to_string Fixed8.zero);
    Printf.printf "one               = %s\n" (Fixed8.to_string Fixed8.one);
    Printf.printf "succ a            = %s\n" (Fixed8.to_string (Fixed8.succ a));
    Printf.printf "pred a            = %s\n" (Fixed8.to_string (Fixed8.pred a));
    Printf.printf "min a b           = %s\n" (Fixed8.to_string (Fixed8.min a b));
    Printf.printf "max a b           = %s\n" (Fixed8.to_string (Fixed8.max a b));
    Printf.printf "gth a b           = %b\n" (Fixed8.gth a b);
    Printf.printf "lth a b           = %b\n" (Fixed8.lth a b);
    Printf.printf "gte a a           = %b\n" (Fixed8.gte a a);
    Printf.printf "lte a a           = %b\n" (Fixed8.lte a a);
    Printf.printf "eqp a a           = %b\n" (Fixed8.eqp a a);
    Printf.printf "eqs a (of_int 3)  = %b\n" (Fixed8.eqs a (Fixed8.of_int 3));
    Printf.printf "add a b           = %s\n" (Fixed8.to_string (Fixed8.add a b));
    Printf.printf "sub a b           = %s\n" (Fixed8.to_string (Fixed8.sub a b));
    Printf.printf "mul a b           = %s\n" (Fixed8.to_string (Fixed8.mul a b));
    Printf.printf "div a b           = %s\n" (Fixed8.to_string (Fixed8.div a b))