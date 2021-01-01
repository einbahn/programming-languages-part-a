(* 1 *)
fun is_older(date1: (int * int * int), date2: (int * int * int)) =
    if #1 date1 <> #1 date2
    then #1 date1 < #1 date2
    else if #2 date1 <> #2 date2
    then #2 date1 < #2 date2
    else if #3 date1 <> #3 date2
    then #3 date1 < #3 date2
    else false

(* 2 *)
fun number_in_month(lst : (int * int * int) list, mon : int) =
  if null lst
  then 0
  else if #2 (hd lst) = mon
  then number_in_month(tl lst,mon) + 1
  else number_in_month(tl lst,mon)


(* 3 *)
fun number_in_months( dates: (int * int * int) list, months: int list) = 
    if null months
    then 0
    else number_in_month(dates, hd months) + number_in_months(dates, tl months)


(* 4. val test4 = dates_in_month ([(2012,2,28),(2013,12,1)],2) = [(2012,2,28)] *)
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

(* 5. val test5 = dates_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = [(2012,2,28),(2011,3,31),(2011,4,28)] *)
fun dates_in_months(dates: (int * int * int) list, months: int list) = 
    if null months
    then []
    else
        dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)

(* 6 val test6 = get_nth (["hi", "there", "how", "are", "you"], 2) = "there" *)
fun get_nth(xs: string list, n: int) = 
    if n = 1
    then hd xs
    else get_nth(tl xs, n - 1)

(* 7. val test7 = date_to_string (2013, 6, 1) = "June 1, 2013" *)
fun date_to_string(year: int, month: int, day: int) = 
    let val month_strs = ["January", "February", 
                          "March", "April", 
                          "May", "June", 
                          "July", "August", 
                          "September", "October", 
                          "November", "December"]
    in get_nth(month_strs, month) ^ " " ^ Int.toString(day) ^ ", " ^ Int.toString(year)
    end

(* 8 val test8 = number_before_reaching_sum (10, [1,2,3,4,5]) = 3 *)
fun number_before_reaching_sum(sum: int, ints: int list) = 
     if hd ints >= sum 
     then 0 
     else 1 + number_before_reaching_sum(sum - hd ints, tl ints)

(* 9 *)
fun what_month (day: int) = 
    let val days_in_month = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in
        1 + number_before_reaching_sum(day, days_in_month)
    end

(* 10 *)
fun month_range(day1: int, day2: int) =
    if day1 > day2
    then []
    else what_month(day1)::month_range(day1+1, day2)


(* 11 *)
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
