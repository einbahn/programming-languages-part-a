(* Write a function is_older that takes two dates and evaluates to true or false. It evaluates to true if
the first argument is a date that comes before the second argument. (If the two dates are the same,
the result is false.)
val test1 = is_older ((1,2,3),(2,3,4)) = true

 *)

fun is_older(date1: (int * int * int), date2: (int * int * int)) =
    if #1 date1 <> #1 date2
    then #1 date1 < #1 date2
    else if #2 date1 <> #2 date2
    then #2 date1 < #2 date2
    else if #3 date1 <> #3 date2
    then #3 date1 < #3 date2
    else false

(* Write a function number_in_month that takes a list of dates and a month (i.e., an int) and returns
how many dates in the list are in the given month.

val test2 = number_in_month ([(2012,2,28),(2013,12,1)],2) = 1
*)

fun number_in_month ( ms: (int * int * int) list, m: int) = 
    if null ms
    then 0
    else
        let val tn = number_in_month(tl ms, m)
        in 
            if #2 (hd ms) = m
            then 1 + tn
            else tn 
        end


(* Write a function number_in_months that takes a list of dates and a list of months (i.e., an int list)
and returns the number of dates in the list of dates that are in any of the months in the list of months.
Assume the list of months has no number repeated. Hint: Use your answer to the previous problem. *)

fun number_in_months( dates: (int * int * int) list, months: int list) = 
    if null months
    then 0
    else number_in_month(dates, hd months) + number_in_months(dates, tl months)


(* 4. Write a function dates_in_month that takes a list of dates and a month (i.e., an int) and returns a
list holding the dates from the argument list of dates that are in the month. The returned list should
contain dates in the order they were originally given.

val test4 = dates_in_month ([(2012,2,28),(2013,12,1)],2) = [(2012,2,28)]

 *)

 fun dates_in_month(dates: (int * int * int) list, month: int) = 
    if null dates
    then []
    else
        let val tl_ans = dates_in_month(tl dates, month)
        in 
            if #2 (hd dates) = month
            then (hd dates)::tl_ans
            else tl_ans
        end

(* 5. Write a function dates_in_months that takes a list of dates and a list of months (i.e., an int list)
and returns a list holding the dates from the argument list of dates that are in any of the months in
the list of months. Assume the list of months has no number repeated. Hint: Use your answer to the
previous problem and SML’s list-append operator (@). 

val test5 = dates_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = [(2012,2,28),(2011,3,31),(2011,4,28)]

*)

fun dates_in_months(dates: (int * int * int) list, months: int list) = 
    if null months
    then []
    else
        dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)


(* 6. Write a function get_nth that takes a list of strings and an int n and returns the n
th element of the
list where the head of the list is 1st. Do not worry about the case where the list has too few elements:
your function may apply hd or tl to the empty list in this case, which is okay.

val test6 = get_nth (["hi", "there", "how", "are", "you"], 2) = "there"

 *)

fun get_nth(strs: string list, idx: int) = 
    if idx = 1
    then hd strs
    else get_nth(tl strs, idx - 1)

(* 7. Write a function date_to_string that takes a date and returns a string of the form January 20, 2013
(for example). Use the operator ^ for concatenating strings and the library function Int.toString
for converting an int to a string. For producing the month part, do not use a bunch of conditionals.
Instead, use a list holding 12 strings and your answer to the previous problem. For consistency, put a
comma following the day and use capitalized English month names: January, February, March, April,
May, June, July, August, September, October, November, December.

val test7 = date_to_string (2013, 6, 1) = "June 1, 2013"

 *)

fun date_to_string(year: int, month: int, day: int) = 
    let val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
    in get_nth(months, month) ^ " " ^ Int.toString(day) ^ ", " ^ Int.toString(year)
    end

(* 8. Write a function number_before_reaching_sum that takes an int called sum, which you can assume
is positive, and an int list, which you can assume contains all positive numbers, and returns an int.
You should return an int n such that the first n elements of the list add to less than sum, but the first
n + 1 elements of the list add to sum or more. Assume the entire list sums to more than the passed in
value; it is okay for an exception to occur if this is not the case.

val test8 = number_before_reaching_sum (10, [1,2,3,4,5]) = 3
 *)

 fun number_before_reaching_sum(sum: int, ints: int list) = 
    let val interim_sum = hd ints + hd (tl ints)
        val rest_of_sum = sum - interim_sum
    in 
        if rest_of_sum < 0
        then hd ints
        else number_before_reaching_sum(rest_of_sum, tl ints)
    end 

(* 9. Write a function what_month that takes a day of year (i.e., an int between 1 and 365) and returns
what month that day is in (1 for January, 2 for February, etc.). Use a list holding 12 integers and your
answer to the previous problem.

val test9 = what_month 70 = 3

 *)

fun what_month (day: int) = 
    let val days_in_month = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
        fun count_month(days:int, month: int) = 
        if days <= 0
        then month
        else
            count_month((days - number_before_reaching_sum(days, days_in_month)), month+1)
    in
        count_month(day, 0)        
    end

(* Write a function month_range that takes two days of the year day1 and day2 and returns an int list
[m1,m2,...,mn] where m1 is the month of day1, m2 is the month of day1+1, ..., and mn is the month
of day day2. Note the result will have length day2 - day1 + 1 or length 0 if day1>day2.

val test10 = month_range (31, 34) = [1,2,2,2]

 *)

fun month_range(day1: int, day2: int) =
    if day1 > day2
    then []
    else what_month(day1)::month_range(day1+1, day2)


(*  Write a function oldest that takes a list of dates and evaluates to an (int*int*int) option. It
evaluates to NONE if the list has no dates and SOME d if the date d is the oldest date in the list.

val test11 = oldest([(2012,2,28),(2011,3,31),(2011,4,28)]) = SOME (2011,3,31)
 *)

fun oldest(dates: (int * int * int) list) = 
    if null dates
    then NONE
    else if null (tl dates)
    then SOME (hd dates)
    else
        let val tl_ans = oldest(tl dates)
        in 
            if is_older(hd dates, valOf(tl_ans))
            then SOME (hd dates)
            else tl_ans
        end
