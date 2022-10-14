fun is_older (d1 : int * int * int, d2 : int * int * int) =
    (#1 d1) <= (#1 d2)
    andalso (#2 d1) <= (#2 d2)
    andalso (#3 d1) < (#3 d2)
			 
    
fun number_in_month (ds : (int * int * int) list, month : int) =
    if null ds
    then 0
    else
	if (#2 (hd ds)) = month
	then 1 + number_in_month(tl ds, month)
	else number_in_month(tl ds, month)
			     

fun number_in_months (ds: (int * int * int) list, months : int list) =
    if null months
    then 0
    else
	if number_in_month(ds, hd months) > 0
	then 1 + number_in_months(ds, tl months)
	else number_in_months(ds, tl months)

			     
fun dates_in_month (ds : (int * int * int) list, month : int) =
    if null ds
    then []
    else
	if (#2 (hd ds)) = month
	then (hd ds)::dates_in_month(tl ds, month)
	else dates_in_month(tl ds, month)

			   
fun dates_in_months (ds : (int * int * int) list, months: int list) =
    if null months
    then []
    else
	let val dates = dates_in_month(ds, hd months)
	in
	    if null dates
	    then dates_in_months(ds, tl months)
	    else dates@dates_in_months(ds, tl months)
	end
	   
			    
 fun get_nth (xs : string list, n : int) =
    if n=1
    then hd xs
    else get_nth (tl xs, n-1)


fun date_to_string (date : int * int * int) =
    let
	val months = ["January","February","March","April","May","June","July","August","September","October","November","December"]
    in
	get_nth(months, (#2 date)) ^ " " ^ Int.toString (#3 date) ^ ", " ^ Int.toString (#1 date) 
    end
	

fun number_before_reaching_sum (sum : int, nums : int list) =
    if sum <= 0
    then 0
    else 1 + number_before_reaching_sum(sum - (hd nums), tl nums)


fun what_month (n : int) =
    let
	val days = [31,28,31,30,31,30,31,31,30,31,30,31]
    in
	number_before_reaching_sum(n, days)
    end

			  
fun month_range (day1 : int, day2 : int) =
    if day1 > day2
    then []
    else what_month(day1)::month_range(day1+1, day2)

				      
fun oldest (ds : (int * int * int) list) =
    if null ds
    then NONE
    else
	let
	    fun helper (dates : (int * int * int) list) =
		if null (tl dates)
		then hd dates
		else
		    let ans = helper(tl dates)
		    in
			if is_older(hd dates, ans)
			then hd dates
			else ans
		    end
	in
	    SOME helper(ds)
	end

	    
