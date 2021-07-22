fun is_older (first: (int*int*int), second: (int*int*int)) =
    if (#1 first) < (#2 second)
    then true
    else
	if (#1 first) > (#1 second)
	then false
	else
	    if (#2 first) < (#2 second)
	    then true
	    else
		if (#2 first) > (#2 second)
		then false
		else
		    (#3 first) < (#3 second);

fun number_in_month (dates: (int*int*int) list, month: int) =
    if null dates
    then 0
    else
	if (#2 (hd dates)) = month
	then 1 + number_in_month((tl dates), month)
	else number_in_month((tl dates), month);

fun number_in_months (dates: (int*int*int) list, months: int list) =
    if null months
    then 0
    else
	number_in_month(dates, (hd months)) + number_in_months(dates, (tl months));

fun dates_in_month (dates: (int*int*int) list, month: int) =
    if null dates
    then []
    else
	if (#2 (hd dates)) = month
	then (hd dates)::dates_in_month((tl dates), month)
	else dates_in_month((tl dates), month);

fun dates_in_months (dates: (int*int*int) list, months: int list) =
    if null months
    then []
    else
	dates_in_month(dates, (hd months))@dates_in_months(dates, (tl months));

fun get_nth (strs: string list, n: int) =
   let fun get_nth (strs: string list, n: int, i: int) =
	   if n = i
	   then hd strs
	   else get_nth((tl strs), n, i+1)
   in
       get_nth(strs, n, 1)
   end

fun date_to_string (year: int, month: int, day: int) =
    let val month_strings = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
    in
	get_nth(month_strings, month) ^ " " ^ Int.toString(day) ^ ", " ^ Int.toString(year)
    end

fun number_before_reaching_sum (sum: int, nums: int list) =
    let fun number_before_reaching_sum (sum: int, nums: int list, i: int, tmp: int) =
	   if tmp + (hd nums) >= sum
	   then i
	   else number_before_reaching_sum(sum, (tl nums), i+1, tmp+(hd nums))
   in
       number_before_reaching_sum(sum, nums, 0, 0)
   end

fun what_month (doy: int) =
    let val days = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in
	number_before_reaching_sum(doy, days) + 1
    end
    
fun month_range (from: int, to: int) =
    if from > to
    then []
    else
	what_month(from)::month_range(from+1, to);

fun oldest (dates: (int*int*int) list) =
    if null dates
    then NONE
    else
	let fun oldest_nonempty(dates: (int*int*int) list) =
		if null (tl dates)
		then hd dates
		else
		    let val tl_ans = oldest_nonempty((tl dates))
		    in
			if is_older((hd dates), tl_ans)
			then hd dates
			else tl_ans
		    end
	in
	    SOME (oldest_nonempty dates)
	end
