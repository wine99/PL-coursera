fun is_older(date1: int*int*int, date2: int*int*int)=
    if #1 date1 < #1 date2
    then true
    else if #1 date1 > #1 date2
    then false
    else if #2 date1 < #2 date2
    then true
    else if #2 date1 > #2 date2
    then false
    else if #3 date1 <  #3 date2
    then true
    else false
	    
	    
fun number_in_month(list_dates: (int*int*int) list, month: int)=
    if null list_dates
    then 0
    else
	let
	    fun num_new(list_dates: (int*int*int) list)=
                if null list_dates
                then 0
                else
		    let val tl_ans = num_new(tl list_dates)
		    in
			if #2(hd list_dates) = month
			then 1 + tl_ans
			else 0 + tl_ans
		    end
	in
	    num_new list_dates
	end
	    
	    
fun number_in_months(list_dates: (int*int*int) list, months: int list)=
    if null list_dates orelse null months
    then 0
    else
	let
	    fun new_list(months: int list)=
		if null months
 		then 0
		else number_in_month(list_dates, hd months) + new_list(tl months)
	in
	    new_list months
	end
	    
	    
fun dates_in_month(list_dates: (int*int*int) list, month: int) =
    if null list_dates
    then []
    else
        let
            fun dates_in_month_nonempty(list_dates: (int*int*int) list) =
                if null list_dates
                then []
                else
                let val tl_ans = dates_in_month_nonempty(tl list_dates)
                in
                    if #2(hd list_dates) = month
                    then hd list_dates :: tl_ans
                    else tl_ans
                end
        in
            dates_in_month_nonempty list_dates
        end
	    

fun dates_in_months(list_dates: (int*int*int) list, list_months: int list) =
    if null list_dates orelse null list_months
    then []
    else
        let
            fun dates_in_months_nonempty(list_months: int list) =
                if null list_months
                then []
                else dates_in_month(list_dates, hd list_months) @
                    dates_in_months_nonempty(tl list_months)
        in
            dates_in_months_nonempty list_months
        end
	    
			    
fun get_nth(list_strings: string list, n: int) =
    if null list_strings
    then ""
    else if n = 1
    then hd list_strings
    else get_nth(tl list_strings, n-1)

fun date_to_string(dates: int*int*int) =
    let val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
    in get_nth(months, #2 dates) ^" "^
       Int.toString(#3 dates) ^", "^ Int.toString(#1 dates)
    end
	
fun number_before_reaching_sum(num: int, list: int list)=
    if num - hd list > 0
    then 1 + number_before_reaching_sum(num - hd list, tl list)
    else 0
				  
fun what_month(day: int) =
    let val val_list = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in
	1 + number_before_reaching_sum(day, val_list)
    end
	

fun month_range(day1: int, day2: int) =
    if day2 >= day1
    then what_month(day1) :: month_range(day1+1, day2)
    else []
	    
fun oldest(list_dates: (int*int*int) list)=
    if null list_dates
    then NONE
    else if null (tl list_dates)
    then SOME(hd list_dates)
    else
	let val ans = oldest(tl list_dates)
	in
	    if is_older(hd list_dates, valOf ans)
	    then SOME(hd list_dates)
	    else ans
	end
	    
