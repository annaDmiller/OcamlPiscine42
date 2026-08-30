(*Input module*)
module type VAL =
sig
    type t

    val add : t -> t -> t

    val mul : t -> t -> t
end

(*Output module*)
module type EVALEXPR =
sig
    type t

    type expr =
        | Value of t
        | Add of expr * expr
        | Mul of expr * expr

    val eval : expr -> t
end

(* Destructive substitution on the functor's own signature: the result
   module never exposes a separate "type t", it is folded directly into
   "expr" and "eval" as V.t. *)
module type MAKEEVALEXPR = functor (V : VAL) -> EVALEXPR with type t := V.t

module MakeEvalExpr : MAKEEVALEXPR =
    functor (V : VAL) ->
    struct
        type t = V.t

        type expr = 
            | Value of t
            | Add of expr * expr
            | Mul of expr * expr

        let rec eval (e : expr) : t =
            match e with
                | Value v -> v
                | Add (a, b) -> V.add (eval a) (eval b)
                | Mul (a, b) -> V.mul (eval a) (eval b)
    end

(*As long as signature of VAL is abstract, it means that we don't inform the outside
  structures that our IntVal.t = int. To verify that this information is given
  outside we have to declare 'with type t = int'. Despite 'with type t := int' it doesn't
  replace type t with int value as in the functor we need to have the VAL input module with
  VAL.t type and add and mul functions which operates with VAL.t type as well so we can't
  replace VAL.t type with int but we can inform the outside structures that VAL.t has the
  type of int.*)
module IntVal : VAL with type t = int =
struct
    type t = int

    let add = ( + )

    let mul = ( * )
end

module FloatVal : VAL with type t = float =
struct
    type t = float

    let add = ( +. )

    let mul = ( *. )
end

module StringVal : VAL with type t = string =
struct
    type t = string
  
    let add s1 s2 = if (String.length s1) > (String.length s2) then s1 else s2
    
    let mul = ( ^ )
end

(*Destructive substitution of EVALEXPR the type t passes the type int instead of
  type t to the module IntEvalExpr. It means that IntEvalExpr.t = int. Then we need to
  verify that IntVal.t = int as well*)
module IntEvalExpr : EVALEXPR with type t := int = MakeEvalExpr (IntVal)

module FloatEvalExpr : EVALEXPR with type t := float = MakeEvalExpr (FloatVal)

module StringEvalExpr : EVALEXPR with type t := string = MakeEvalExpr (StringVal)

let ie = IntEvalExpr.Add (IntEvalExpr.Value 40, IntEvalExpr.Value 2)

let fe = FloatEvalExpr.Add (FloatEvalExpr.Value 41.5, FloatEvalExpr.Value 0.92)

let se = StringEvalExpr.Mul (StringEvalExpr.Value "very ",
                             (StringEvalExpr.Add (StringEvalExpr.Value "very long",
                                                  StringEvalExpr.Value "short")))

let () = Printf.printf "Res = %d\n" (IntEvalExpr.eval ie)
let () = Printf.printf "Res = %f\n" (FloatEvalExpr.eval fe)
let () = Printf.printf "Res = %s\n" (StringEvalExpr.eval se)