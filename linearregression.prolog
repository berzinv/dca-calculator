:- module(linearregression, [linear_regression/3]).

% Compute linear regression parameters (slope and intercept)
linear_regression(Data, Slope, Intercept) :-
    length(Data, N),
    sum_xy(Data, SumXY),
    sum_x_squared(Data, SumXSquared),
    sum_x(Data, SumX),
    sum_y(Data, SumY),
    
    Slope is (N * SumXY - SumX * SumY) / (N * SumXSquared - SumX * SumX),
    Intercept is (SumY - Slope * SumX) / N.

% Helper predicate: sum of x*y for each data point
sum_xy([], 0).
sum_xy([Point|Rest], Sum) :-
    sum_xy(Rest, RestSum),
    nth0(0, Point, X),
    nth0(1, Point, Y),
    Sum is RestSum + X * Y.

% Helper predicate: sum of x^2 for each data point
sum_x_squared([], 0).
sum_x_squared([Point|Rest], Sum) :-
    sum_x_squared(Rest, RestSum),
    nth0(0, Point, X),
    Sum is RestSum + X * X.

% Helper predicate: sum of x for each data point
sum_x(Data, Sum) :-
    findall(X, member([X, _], Data), XList),
    sum_list(XList, Sum).

% Helper predicate: sum of y for each data point
sum_y(Data, Sum) :-
    findall(Y, member([_, Y], Data), YList),
    sum_list(YList, Sum).
