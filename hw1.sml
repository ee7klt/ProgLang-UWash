fun isolder (date1: int*int*int, date2: int*int*int) =
   ( (#1 date1) < (#1 date2)) orelse (((#1 date1) = (#1 date2)) andalso ( (#2 date1) < (#2 date2))) orelse (((#1 date1) = (#1 date2)) andalso ( (#2 date1) = (#2 date2)) andalso ((#3 date1) < (#3 date2)))


fun number_in_month (dates: int*int*int list, month: int) =
   if null dates
   then 0
   else #2 (hd dates) + number_in_month ( #2 (tl dates))
