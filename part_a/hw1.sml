val month_lengths = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
val month_strings = [ "January", "February", "March", "April",
                      "May", "June", "July", "August",
                      "September", "October", "November", "December"]

fun year (date : int*int*int) = #1 date

fun month (date : int*int*int) = #2 date

fun day (date : int*int*int) = #3 date

fun is_older (date1 : int*int*int, date2 : int*int*int) =
    if year date1 <> year date2
    then year date1 < year date2
    else
        if month date1 <> month date2
        then month date1 < month date2
        else day date1 < day date2

fun number_in_month (dates : (int*int*int) list, the_month : int) =
    if null dates
    then 0
    else if the_month = month (hd dates)
    then 1 + number_in_month ((tl dates), the_month)
    else number_in_month ((tl dates), the_month)

fun number_in_months (dates : (int*int*int) list, months : int list) =
    if null months
    then 0
    else number_in_month (dates, (hd months))
         + number_in_months (dates, (tl months))

fun dates_in_month (dates : (int*int*int) list, the_month : int) =
    if null dates
    then []
    else if the_month = month (hd dates)
    then (hd dates) :: dates_in_month ((tl dates), the_month)
    else dates_in_month ((tl dates), the_month)

fun dates_in_months (dates : (int*int*int) list, months : int list) =
    if null months
    then []
    else dates_in_month (dates, (hd months)) @ dates_in_months (dates, (tl months))

fun get_nth (lst : string list, n : int) =
    if n = 1
    then hd lst
    else get_nth ((tl lst), n - 1)

fun date_to_string (the_year : int, the_month : int, the_day : int) =
    get_nth (month_strings, the_month) ^ " "
    ^ Int.toString (the_day) ^ ", " ^ Int.toString(the_year)

fun number_before_reaching_sum (sum : int, lst : int list) =
    if sum <= (hd lst)
    then 0
    else 1 + number_before_reaching_sum (sum - (hd lst), (tl lst))

fun what_month (the_day) =
    number_before_reaching_sum (the_day, month_lengths) + 1

fun month_range (start_day, end_day) =
    if start_day > end_day
    then []
    else what_month (start_day) :: month_range (start_day + 1, end_day)

fun oldest (lst : (int*int*int) list) =
    if null lst
    then NONE
    else let
        fun oldest_nonempty (lst : (int*int*int) list) =
                if null (tl lst)
                then hd lst
                else let val oldest_tl = oldest_nonempty (tl lst)
                     in if is_older ((hd lst), oldest_tl)
                        then hd lst
                        else oldest_tl
                     end
    in
        SOME (oldest_nonempty lst)
    end
