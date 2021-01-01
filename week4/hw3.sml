exception NoAnswer

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(* 1. string list -> string list *)
fun only_capitals xs =
    List.filter (fn x => Char.isUpper(String.sub(x, 0))) xs

(* 2 - longest_string1 string list -> string  *)
fun longest_string1 xs = 
    List.foldl (fn (x, init) => if String.size x > String.size init then x else init) "" xs

(* 3 - foldl f init [x1, x2, .., xn] -> f(xn,...,f(x2, f(x1, init))...) *)
fun longest_string2 xs =
    List.foldl (fn (x, init) => if String.size init <= String.size x then x else init) "" xs

(* 4 *)
fun longest_string_helper f xs =
    List.foldl (fn (x, init) => if f(String.size x, String.size init) then x else init) "" xs

val longest_string3 = longest_string_helper (fn (x, y) => if x > y then true else false)

val longest_string4 = longest_string_helper (fn (x, y) => if y <= x then true else false)

(* 5 *)
val longest_capitalized = longest_string1 o only_capitals

(* 6 *)
val rev_string = String.implode o List.rev o String.explode

(* 7 *)
fun first_answer f xs = 
    case xs of
    [] => raise NoAnswer
    | x::xs' => case f x of
                SOME v => v
                |NONE => first_answer f xs'
                                 
(* 8 *)
fun all_answers f xs =
    let fun helper(acc, ys) = 
        case ys of 
        [] => acc
        | y::ys' => case f y of
                    NONE => NONE
                    | SOME y => helper(SOME(y@(valOf acc)), ys')
    in
        helper(SOME [], xs) 
    end


datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(* 9a *)
val count_wildcards = g (fn () => 1) (fn x => 0)
(* 9b *)
val count_wild_and_variable_lengths = g (fn () => 1) (fn s => String.size s)
(* 9c *)
fun count_some_var (s, p) = g (fn () => 0) (fn x => if s = x then 1 else 0) p
(* 10 *)
fun check_pat p = 
    let fun f p =
        case p of
          Variable x        => [x]
        | TupleP ps         => List.foldl (fn (p,acc) => (f p) @ acc) [] ps
        | ConstructorP(_,p) => f p
        | _                 => []
        
        fun distinct xs = 
            case xs of
             []      => false
            |[_]     => true
            | x::xs' => case List.exists (fn y => x = y) xs' of
                           true => false
                        | false => distinct xs'
    in
        distinct (f p)
    end

(* 11 *)
fun match(v, p) =
    case (p, v) of
    (Wildcard, _) => SOME ([])
    | (Variable s, v) => SOME([(s, v)])
    | (UnitP, Unit) => SOME([])
    | (ConstP n, Const i) => if n = i then SOME([]) else NONE
    | (TupleP ps, Tuple vs) => if length ps = length vs 
                               then all_answers match (ListPair.zip(vs, ps)) 
                               else NONE
    | (ConstructorP(s1,p), Constructor(s2,v)) => if s1 = s2 then match(v, p) else NONE 
    | _ => NONE

(* 12 *)
fun first_match v ps = 
    let val mapped = List.map (fn x => (v, x)) ps
    in
        SOME (first_answer match mapped) handle NoAnswer => NONE
    end
