(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
fun all_except_option(s, ss) =
    let fun aux(ms) = 
        case ms of 
            [] => []
            | m::ms' => if same_string(m, s)
                        then aux(ms')
                        else m::aux(ms')
        val res = aux(ss)
    in
        if res = ss then NONE else SOME res
    end

fun get_substitutions1 (sslist, s) =
    case sslist of
        [] => [] 
        | sl::sslist' => (case all_except_option(s, sl) of
             NONE => []
            | SOME ls => ls) @ get_substitutions1 (sslist', s)


fun get_substitutions2 (xss, y) = 
    let fun f (xss,acc) =
            case xss of
                [] => acc
              | x :: xss' => case all_except_option(y, x) of
                               NONE => f(xss', acc)
                             | SOME z => f(xss', acc@z)
    in
        f(xss,[])
    end

fun similar_names(xss, {first=f, middle=m, last=l}) = 
    let fun helper (subs) = 
        case subs of
        [] => []
        | x::xs' => {first=x, middle=m, last=l} :: helper(xs')
    in
        let val subs = get_substitutions2(xss, f)
        in 
            {first=f, middle=m, last=l}::helper(subs)
        end
    end 

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

fun card_color (suit, rank) = 
    case suit of
    Clubs => Black
    | Diamonds => Red
    | Hearts => Red
    | Spades => Black

fun card_value (suit, rank) = 
    case rank of
    Num(i) => i
    | Ace => 11
    | _ => 10 

fun remove_card(cs, c, e) =
    let fun helper(cds) = 
        case cds of
        [] => []
        | cd::cds' => if cd = c
                     then cds'
                     else cd::helper(cds')
        val res = helper(cs)
    in 
        if res = cs then raise e else res
    end

fun all_same_color cs = 
    case cs of
    [] => true
    | x::[] => true
    | head::neck::rest => card_color(head) = card_color(neck) andalso all_same_color(neck::rest)

fun sum_cards cs = 
    let fun aux(cs,acc) =
        case cs of
        [] => acc
        | c::cs' => aux(cs', card_value(c) + acc)
    in 
        aux(cs,0)
    end

fun score (cards, goal) =
    let val sum = sum_cards(cards) 
        val prelim_score = if sum > goal
                           then 3 * (sum - goal)
                           else (goal - sum)
    in
        if all_same_color(cards) 
        then prelim_score div 2 
        else prelim_score
    end

fun officiate(card_list, move_list, goal) =
    let fun game(card_list, move_list, held_cards) =
        if sum_cards(held_cards) > goal
        then score(held_cards, goal)
        else 
            case (card_list, move_list) of
              ([], _) => score(held_cards, goal)
            | (_, []) => score(held_cards, goal)
            | (card::card_list', move::move_list') => 
                case move of
                    Draw => game(card_list', move_list', card::held_cards)
                    | Discard(i) => let 
                                        val remain = remove_card(held_cards, i, IllegalMove)
                                    in
                                        game(card::card_list', move_list', remain)
                                    end
    in
        game(card_list, move_list, [])
    end