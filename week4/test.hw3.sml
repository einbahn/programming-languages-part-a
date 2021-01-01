use "hw3.sml";

val test1 = only_capitals ["Hello","there","This","butnotthis"] = ["Hello","This"]
val test1a = only_capitals ["Hello"] = ["Hello"]
val test1b = only_capitals ["there","girl"] = []
val test1c = only_capitals ["Hello","There","Girl","Gold"] = ["Hello","There","Girl","Gold"]

val test2 = longest_string1 ["Hello","this","ist","an","b"] = "Hello"
val test2b = longest_string1 ["abc1","abc2","ab"] = "abc1"


val test3 = longest_string2 ["abc1","abc2","ab"] = "abc2"
val test3b = longest_string2 ["abcd","abcde","ab"] = "abcde"

val test4a = longest_string3 ["abc1","abc2","ab"] = "abc1"
val test4b = longest_string4 ["abc1","abc2","ab"] = "abc2"

val test5 = longest_capitalized ["Hello","There","HelloThere","Hellother1","thisshouldnotbeincluded","northisReally"] = "HelloThere"

val test6 = rev_string "abc" = "cba"

val test7 = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] = 4
val test7a = (first_answer (fn x => if x > 5 then SOME x else NONE) [1,2,3,4,5] handle NoAnswer => 0) = 0

val test8 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE

val test9a = count_wildcards(Wildcard) = 1
val test9aa = count_wildcards(UnitP) = 0
val test9ab = count_wildcards(TupleP [Wildcard, UnitP, Variable "x"]) = 1
val test9ac = count_wildcards(TupleP [Wildcard, Wildcard, Variable "xy"]) = 2

val test9 = count_wild_and_variable_lengths(Variable("a")) = 1
val test9ba = count_wild_and_variable_lengths(UnitP) = 0
val test9bb = count_wild_and_variable_lengths(TupleP [Wildcard, UnitP, Variable "x"]) = 2
val test9bc = count_wild_and_variable_lengths(TupleP [Wildcard, Wildcard, Variable "xy"]) = 4

val test9c = count_some_var ("x", Variable("x")) = 1
val test9ca = count_some_var("test", TupleP [TupleP [Wildcard, UnitP, Variable "test",Variable "test"]]) = 2
val test9cb = count_some_var("test",TupleP [TupleP [Wildcard, UnitP,Variable "te"]]) = 0
val test9cd = count_some_var("test", TupleP [TupleP [Wildcard, UnitP, Variable "test"]]) = 1  

val test10 = check_pat (Variable("x")) = true
val test10a = check_pat (TupleP [Wildcard,Variable "cat",
                                Variable "pp",TupleP[Variable "tt"],
                                Wildcard,ConstP 3,ConstructorP("cony",Variable "pp")]) = false
val test10b = check_pat (TupleP [Wildcard,Variable "cat",
                            Variable "pp",TupleP[Variable "tt"],
                            Wildcard,ConstP 3,
                            ConstructorP("cony",Variable "test")]) = true

                            val test11 = match(Unit, UnitP) = SOME []
val test11a = match(Unit, Variable "cat") = SOME [("cat", Unit)]
val test11c = match(Tuple [Unit], Variable "cat") = SOME [("cat", Tuple [Unit])]
val test11b = match(Unit, ConstP 3) = NONE
val test11d = match(Tuple [Unit, Const 3], TupleP [Variable "cat", ConstP 3]) = SOME [("cat", Unit)]
val test11e = match(Tuple [Unit, Const 8], TupleP [Variable "cat", ConstP 3]) = NONE
val test11f = match(Tuple [Unit, Const 8], TupleP [Variable "cat", Variable "dog"]) = SOME [("dog", Const 8), ("cat", Unit)]
val test11g = match(Tuple [Unit, Tuple [Unit, Unit]], TupleP [Variable "cat", TupleP [Variable "dog", Variable "rat"]]) = SOME [("rat", Unit), ("dog", Unit), ("cat", Unit)]
val test11h = match(Constructor ("mat", Unit), ConstructorP ("hat", Variable "cat")) = NONE
val test11i = match(Constructor ("dog", Unit), ConstructorP ("dog", Variable "cat")) = SOME [("cat", Unit)]
