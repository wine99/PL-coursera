fun is_older(a: int*int*int, b: int*int*int) =
    let fun f(a: int list, b: int list) = 
        if null a
        then false
        else
            if hd a = hd b
            then f(tl a, tl b)
            else hd a < hd b
    in
        f((#1 a)::(#2 a)::(#3 a)::[], (#1 b)::(#2 b)::(#3 b)::[])
    end

fun number_in_month(l: (int*int*int) list, month: int) =
    if null l
    then 0
    else (if #2 (hd l) = month then 1 else 0) + number_in_month(tl l, month)

fun number_in_months(l: (int*int*int) list, months: int list) =
    if null months
    then 0
    else number_in_month(l, hd months) + number_in_months(l, tl months)

fun dates_in_month(l: (int*int*int) list, month: int) =
    if null l
    then []
    else
        if #2 (hd l) = month
        then (hd l)::dates_in_month(tl l, month)
        else dates_in_month(tl l, month)

fun dates_in_months(l: (int*int*int) list, months: int list) =
    if null months
    then []
    else dates_in_month(l, hd months) @ dates_in_months(l, tl months)

fun get_nth(l: string list, n: int) =
    if n = 1
    then hd l
    else get_nth(tl l, n-1)

fun date_to_string(date: int*int*int) = 
    get_nth(["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"], #2 date)
    ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)

fun number_before_reaching_sum(sum: int, l: int list) =
    if hd l >= sum
    then 0
    else number_before_reaching_sum(sum - hd l, tl l) + 1

fun what_month(d: int) =
    number_before_reaching_sum(d, [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]) + 1

fun month_range(day1: int, day2: int) =
    if day1 > day2
    then []
    else what_month(day1)::month_range(day1+1, day2)

fun oldest(l: (int*int*int) list) =
    if null l
    then NONE
    else
        let val tmp = oldest(tl l)
        in
            if isSome tmp andalso is_older(valOf tmp, hd l)
            then tmp
            else SOME (hd l)
        end

(* Challenge Problem 12*)

fun deduplicate(l: int list) =
    if null l
    then []
    else
        let
            fun is_in(x: int, l: int list) =
                if null l
                then false
                else if hd l = x then true else is_in(x, tl l)
            val tmp = deduplicate(tl l)
        in 
            if is_in(hd l, tmp)
            then tmp
            else (hd l)::tmp
        end

fun number_in_months_challenge(l: (int*int*int) list, months: int list) =
    number_in_months(l, deduplicate(months))

fun dates_in_months_challenge(l: (int*int*int) list, months: int list) =
    dates_in_months(l, deduplicate(months))

(* Challenge Problem 13 *)

fun get_feb_days(year: int) = 
    if year mod 400 = 0 orelse (year mod 4 = 0 andalso year mod 100 <> 0)
    then 29 (* leap year *)
    else 28

fun get_nth_intlist(l: int list, n: int) =
    if n = 1
    then hd l
    else get_nth_intlist(tl l, n-1)

fun reasonable_day(year: int, month: int, day: int) = 
    if day < 1
    then false
    else
        if month <> 2
        then day <= get_nth_intlist([31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31], month)
        else day <= get_feb_days(year)

fun reasonable_date(date: int*int*int) =
    #1 date > 0 andalso #2 date >= 1 andalso #2 date <= 12 andalso reasonable_day(#1 date, #2 date, #3 date)