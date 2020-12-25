(* Homework2 Simple Test *)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

val test1 = all_except_option ("string", ["string"]) = SOME []

val test2 = get_substitutions1 ([["foo"],["there"]], "foo") = []
val test2a = get_substitutions1 ([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], "Fred") = ["Fredrick","Freddie","F"]

val test3 = get_substitutions2 ([["foo"],["there"]], "foo") = []
val test3a = get_substitutions2 ([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], "Fred") = ["Fredrick","Freddie","F"]

val test4 = similar_names ([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Fred", middle="W", last="Smith"}) =
	    [{first="Fred", last="Smith", middle="W"}, {first="Fredrick", last="Smith", middle="W"},
	     {first="Freddie", last="Smith", middle="W"}, {first="F", last="Smith", middle="W"}]

val test5 = card_color (Clubs, Num 2) = Black
val test5a = card_color (Diamonds, Num 2) = Red
val test5b = card_color (Hearts, Num 2) = Red
val test5c = card_color (Spades, Num 2) = Black

val test6 = card_value (Clubs, Num 2) = 2
val test6a = card_value (Diamonds, Ace) = 11
val test6b = card_value (Hearts, King) = 10
val test6c = card_value (Spades, Queen) = 10
val test6d = card_value (Clubs, Jack) = 10

val test7 = remove_card ([(Hearts, Ace)], (Hearts, Ace), IllegalMove) = []
val test7a = (remove_card ([(Spades, Ace)], (Hearts, Ace), IllegalMove) handle IllegalMove => []) = []
val test7b = remove_card ([(Hearts, Ace),(Spades,Ace)], (Hearts, Ace), IllegalMove) = [(Spades,Ace)]
val test7c = remove_card ([(Hearts, Ace),(Hearts, Ace), (Spades, Ace)], (Hearts, Ace), IllegalMove) = [(Hearts, Ace), (Spades, Ace)]

val test8 = all_same_color [(Hearts, Ace), (Hearts, Ace)] = true
val test8a = all_same_color [(Hearts, Ace), (Hearts, Ace), (Diamonds,Jack),(Diamonds, Queen),(Hearts, King)] = true
val test8b = all_same_color [(Hearts, Ace), (Hearts, Ace), (Diamonds,Jack),(Diamonds, Queen),(Hearts, King)] = true
val test8c = all_same_color [(Hearts, Ace), (Spades, King)] = false
val test8d = all_same_color [(Hearts, Ace), (Spades, Ace), (Diamonds,Jack),(Clubs, Queen),(Hearts, King)] = false

val test9 = sum_cards [(Clubs, Num 2),(Clubs, Num 2)] = 4
val test9a = sum_cards [(Clubs, Ace),(Diamonds, Num 4)] = 15
val test9b = sum_cards [(Clubs, Num 2),(Clubs, Num 3),(Hearts, Jack),(Spades, King)] = 25
val test9c = sum_cards [(Diamonds, Ace),(Hearts, King),(Spades,Queen),(Clubs,Jack)] = 41

val test10 = score ([(Hearts, Num 2),(Clubs, Num 4)],10) = 4
val test10a = score ([(Hearts, Num 2),(Diamonds, Num 4)],10) = 2
val test10b = score ([(Hearts, Num 2),(Clubs, Num 4)],4) = 6
val test10c = score ([(Hearts, Num 2),(Clubs, Num 2)],5) = 1
val test10d = score ([(Hearts, Num 2),(Diamonds, Num 2)],5) = 0
val test10e = score ([(Hearts, Num 2),(Diamonds, Num 3)],5) = 0

val test11 = officiate ([(Hearts, Num 2),(Clubs, Num 4)],[Draw], 15) = 6

val test12 = officiate ([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],[Draw,Draw,Draw,Draw,Draw],42) = 3

val test13 = ((officiate([(Clubs,Jack),(Spades,Num(8))],[Draw,Discard(Hearts,Jack)],42);false)
            handle IllegalMove => true)
             
