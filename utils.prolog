:- module(utils, [
	      get_first_item/2,
	      get_rest/2,
	      lists_to_string/2,
	      list_to_string/2,
	      replace_nulls/3,
	      get_todays_date/1,
	      date_string_to_stamp/2,
	      difference_in_days/3
	  ]).

:- use_module(library(system)).

% Get first item of a list
get_first_item([H|_], H).

get_rest([_| R], R).

% Convert a list of lists to a string
lists_to_string(Lists, Result) :-
    maplist(list_to_string, Lists, InnerStrings),
    atomic_list_concat(InnerStrings, ', ', Result).

% Convert a list to a string
list_to_string(List, Result) :-
    atomic_list_concat(List, ', ', Result).

% Replace null values in a list with the previous non-null value
replace_nulls([], _, []).
replace_nulls([null | Rest], Prev, [Prev | TransformedRest]) :-
    replace_nulls(Rest, Prev, TransformedRest).
replace_nulls([X | Rest], _, [X | TransformedRest]) :-
    X \= null,
    replace_nulls(Rest, X, TransformedRest).

% Get today's date formatted like YYYY-mm-dd
get_todays_date(FormattedDate) :-
    get_time(Stamp),
    format_time(atom(FormattedDate), '%Y-%m-%d', Stamp).

% Convert a date string to a Unix timestamp
date_string_to_stamp(DateString, Stamp) :-
    atomic_list_concat([SYear, SMonth, SDay], '-', DateString),
    atom_number(SYear, Year),
    atom_number(SMonth, Month),
    atom_number(SDay, Day),
    date_time_stamp(date(Year, Month, Day, 0, 0, 0, 0, -, -), Stamp).

% Compute the difference in days between two dates
difference_in_days(DateString1, DateString2, Difference) :-
    date_string_to_stamp(DateString1, Stamp1),
    date_string_to_stamp(DateString2, Stamp2),
    Difference is abs(Stamp2 - Stamp1) / (24 * 60 * 60).
