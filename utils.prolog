:- module(utils, [
	      get_first_item/2,
	      get_rest/2,
	      lists_to_string/2,
	      list_to_string/2
	  ]).

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
