fun is_older (d1 : (int * int * int), d2 : (int * int * int)) =
    (#1 d1) < (#1 d2) orelse
    (#1 d1) = (#1 d2) andalso
    (#2 d1) < (#2 d2) orelse
    (#1 d1) = (#1 d2) andalso
    (#2 d1) = (#2 d2) andalso
    (#3 d1) < (#3 d2)

fun number_in_month (ds : (int * int * int) list, m : int) =
    if null ds
    then 0
    else
	if (#2 (hd ds)) = m
	then 1 + number_in_month(tl ds, m)
	else number_in_month(tl ds, m)

fun number_in_months (ds : (int * int * int) list, ms : int list) =
    if null ms
    then 0
    else
	let val count = number_in_month(ds, hd ms)
	in
	    if count > 0
	    then 1 + number_in_months(ds, tl ms)
	    else number_in_months(ds, tl ms)
	end

fun dates_in_month (ds : (int * int * int) list, m : int) =
    if null ds
    then []
    else
	if (#2 (hd ds)) = m
	then hd ds::dates_in_month(tl ds, m)
	else dates_in_month(tl ds, m)

fun dates_in_months (ds : (int * int * int) list, ms : int list) =
    if null ms
    then []
    else
	let val dates = dates_in_month(ds, hd ms)
	in
	    if null dates
	    then dates_in_months(ds, tl ms)
	    else dates @ dates_in_months(ds, tl ms)
	end

fun get_nth (xs : string list, n : int) =
    if n=1
    then hd xs
    else get_nth (tl xs, n-1)

fun date_to_string (date : (int * int * int)) =
    let val months = [
	"January", "February", "March", "April",
	"May", "June", "July", "August",
	"September", "October", "November", "December"
	]
    in
	get_nth (months, (#2 date)) ^ " " ^ Int.toString((#3 date)) ^ ", " ^ Int.toString((#1 date))
    end
	
fun number_before_reaching_sum (sum : int, xs: int list) =
    if null xs
    then 0
    else
	if sum - (hd xs) <= 0
	then 0
	else 1 + number_before_reaching_sum(sum - hd xs, tl xs)

fun what_month (day: int) =
    let
	val days = [31,28,31,30,31,30,31,31,30,31,30,31]
     in
	1 + number_before_reaching_sum(day, days)
     end

fun month_range (day1 : int, day2 : int) =
    if day1 = day2
    then what_month(day1)::[]
    else what_month(day1)::month_range(day1+1, day2)

fun oldest (ds: (int * int * int) list) =
    if null ds
    then NONE
    else
	let fun helper (ds: (int * int * int) list) =
		if null (tl ds)
		then hd ds
		else
		    let val tl_ans = helper (tl ds)
		    in
			if is_older(hd ds, tl_ans)
			then hd ds
			else tl_ans 
		    end
	in
	    SOME (helper ds)
	end
	    
fun number_in_months_challenge (ds : (int * int * int) list, ms : (int * int * int) list) =
    