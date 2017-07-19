(* Programming Languages Coursera/UW HW1 Solution *)
(* 
	Expected bindings:
	val is_older = fn : (int * int * int) * (int * int * int) -> bool
	val number_in_month = fn : (int * int * int) list * int -> int
	val number_in_months = fn : (int * int * int) list * int list -> int
	val dates_in_month = fn : (int * int * int) list * int -> (int * int * int) list
	val dates_in_months = fn : (int * int * int) list * int list -> (int * int * int) list
	val get_nth = fn : string list * int -> string
	val date_to_string = fn : int * int * int -> string
	val number_before_reaching_sum = fn : int * int list -> int
	val what_month = fn : int -> int
	val month_range = fn : int * int -> int list
	val oldest = fn : (int * int * int) list -> (int * int * int) option
*)


(* 
	1. Takes two dates and evaluates to true or false
	True if the first argument is a date that comes before the second argument.
	False if the two dates are the same
*)
fun is_older(date1: int * int * int, date2: int * int * int) =
	(#1 date1) < (#1 date2) 
	orelse (#1 date1 = #1 date2 andalso #2 date1 < #2 date2) 
	orelse (#1 date1 = #1 date2 andalso #2 date1 = #2 date2 andalso #3 date1 < #3 date2)

(* 
	2.  Takes a list of dates and a month (i.e., an int) and returns number of dates in the list 
	 	that are in the given month
*)
fun number_in_month(dates: (int * int * int) list, month: int) =
	if null dates
	then 0
	else if (#2 (hd dates) = month) then 1 + number_in_month(tl dates, month)
	else number_in_month(tl dates, month)

(*
	3.  Takes a list of dates and a list of months (i.e., an int list) and returns the number of dates
	in the list of dates that are in any of the months in the list of months.
*)
fun number_in_months(dates: (int * int * int) list, months: int list) =
	if null dates orelse null months
	then 0
	else number_in_month(dates, hd months) + number_in_months(dates, tl months)

(*
	4.  Takes a list of dates and a month (i.e., an int) and returns a list holding the dates from the 
	argument list of dates that are in the month
*)
fun dates_in_month(dates: (int * int * int) list, month: int) =
	if null dates
	then []
	else
		let val recursive_case = dates_in_month(tl dates, month)
		in
			if (#2 (hd dates) = month)
			then (hd dates) :: recursive_case
			else recursive_case
		end

(*
	5.  Takes a list of dates and a list of months (i.e., an int list) and returns a list holding the dates 
	from the argument list of dates that are in any of the months in the list of months. 
	Assuming no repeated value in months list
*)
fun dates_in_months(dates: (int * int * int) list, months: int list) =
	if null dates orelse null months
	then []
	else dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)

(*
	6. Takes a list of strings and an int n and returns the nth element of the list where the head of the list is 1st
*)
fun get_nth(strs: string list, index: int) =
	if index = 1
	then hd strs (* Disregarding empty list case for now *)
	else get_nth(tl strs, index -1)

(* 
	7. Taks a date and returns a string of form January 20, 2013 (for example)
*)
fun date_to_string(date: (int * int * int)) =
	let val months = ["January", "February", "March", "April", "May", "June", "July", 
						"August", "September", "October", "November", "December"]
	in
		get_nth(months, #2 date) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
	end

(*
	8. Takes a positive int called sum, and an int list, which contains only positive numbers,
	and returns an int n such that the first n elements of the list add to less than sum,
	but the first n + 1 elements of the list add to sum or more
*)
fun number_before_reaching_sum(sum: int, numbers: int list) =
	if sum <= hd numbers
	then 0
	else 1 + number_before_reaching_sum(sum - (hd numbers), tl numbers)

(*
	9. Takes a day of year (i.e., an int between 1 and 365) and returns what month that day 
	is in (1 for January, 2 for February, etc.)
*)
fun what_month (day: int) =
	let val days_in_month = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
	in
		(* number_before_reaching_sum always returns 1 month less in this case *)
		number_before_reaching_sum(day, days_in_month) + 1
	end

(*
	10. Takes two days of the year day1 and day2 and returns an int list [m1,m2,...,mn] 
	where m1 is the month of day1, m2 is the month of day1+1, ..., and mn is the month of day day2
*)
fun month_range(day1: int, day2: int) =
	if day1 > day2
	then []
	else what_month(day1) :: month_range(day1 + 1, day2)

(*
	11.  Takes a list of dates and evaluates to an (int*int*int) option. Evaluates to NONE if the list 
	has no dates and SOME d if the date d is the oldest date in the list
*)
fun oldest(dates: (int * int * int) list) =
	if null dates
	then NONE
	else
		let val recursive_case = oldest(tl dates)
		in
			if isSome recursive_case andalso is_older((valOf recursive_case), (hd dates))
			then recursive_case
			else SOME(hd dates)
			end