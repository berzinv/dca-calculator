:- module(dca, [dollar_cost_averaging/5]).

% Compute the value of a dollar-cost averaging investment
dollar_cost_averaging(InitialInvestment, MonthlyInvestment, MonthlyReturnRate, NumMonths, FinalValue) :-
    compute_final_value(InitialInvestment, MonthlyInvestment, MonthlyReturnRate, NumMonths, 0, FinalValue).

compute_final_value(_, _, _, 0, Value, Value).
compute_final_value(InitialInvestment, MonthlyInvestment, MonthlyReturnRate, NumMonths, CurrentValue, FinalValue) :-
    NewValue is CurrentValue + InitialInvestment,
    NewNumMonths is NumMonths - 1,
    NewMonthlyInvestment is MonthlyInvestment * (1 + MonthlyReturnRate),
    compute_final_value(NewMonthlyInvestment, MonthlyInvestment, MonthlyReturnRate, NewNumMonths, NewValue, FinalValue).
