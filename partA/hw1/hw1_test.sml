use "hw1_yijieqiu.sml";

val test1 = is_older ((1,2,3),(2,3,4))

val test2 = number_in_month ([(2012,2,28),(2013,12,1)],2)

val test3 = number_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4])

val test4 = dates_in_month ([(2012,2,28),(2013,12,1)],2)

val test5 = dates_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4])

val test6 = get_nth (["hi", "there", "how", "are", "you"], 2)

val test7 = date_to_string (2013, 6, 1)

val test8 = number_before_reaching_sum (10, [1,2,3,4,5])

val test9 = what_month 70

val test10 = month_range (31, 34)

val test11 = oldest([(2012,2,28),(2011,3,31),(2011,4,28)])