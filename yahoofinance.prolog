:- module(yahoofinance, [yahoo_finance/5]).
:- use_module(library(http/http_client)).
:- use_module(library(csv)).
:- use_module(library(apply)).

url_pattern('https://query1.finance.yahoo.com/v7/finance/download/~w?period1=~w&period2=~w&interval=~w&events=history&includeAdjustedClose=true').

%% build_url/5
%% Ticker : string. Example: 'BTC-USD'
%% Period1 & Period2 : string. Example: '2023-11-01', format must be 'YYY-mm-dd'
%% Interval: string. Example: '1d', '1w', '1m'
build_url(Ticker, Period1, Period2, Interval, Url) :-
    url_pattern(Pattern),
    parse_time(Period1, iso_8601, P1tsf), P1ts is floor(P1tsf),
    parse_time(Period2, iso_8601, P2tsf), P2ts is floor(P2tsf),
    format(atom(Url), Pattern, [Ticker, P1ts, P2ts, Interval]).

%% get_prices/2 
%% This will get only the date and the adjusted close price.
%% Url: string. 
get_prices(Url, Prices) :-
    http_get(Url, Data, []),
    open_string(Data, S),
    csv_read_stream(S, Rows, [skip_header('Date')]),
    maplist([row(Date, _, _, _, _, ClosePrice, _), [Date, ClosePrice]] >> true, Rows, Prices).

%% yahoo_finance/5 
%%
%% Ticker: string
%% StartDate, EndDate: string. Example: '2023-10-20'
%% Interval: '1d', '1w', '1m'
yahoo_finance(Ticker, StartDate, EndDate, Interval, Data) :-
    build_url(Ticker, StartDate, EndDate, Interval, Url),
    get_prices(Url, Data).
