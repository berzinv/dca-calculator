:- use_module(library(http/http_client)).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_error)).

:- use_module(yahoofinance).
:- use_module(dca).

%%%%% web part %%%%%
server(Port) :-
    http_server(http_dispatch, [port(Port)]).

:- http_handler(/, say_hi, []).

say_hi(_Request) :-
    reply_html_page(
	\headers,
	[h1('DCA-calculator.com'),
	 
         p(['This example demonstrates generating HTML ',
            'messages from Prolog'
           ]),
	 form([method='post', action='/landing'], [
		  input([type='text', name='initialinvestment', placeholder='Initial investment']),br(''),
		  input([type='text', name='monthlyinvestment', placeholder='Monthly investment']), br(''),
		  input([type='text', name='monthlyreturnrate', placeholder='Monthly return rate']), br(''),
		  input([type='text', name='nummonths', type='number', placeholder='Number of months']), br(''),
		  input([type='submit', value="Go"])
	      ])
	]).

:- http_handler('/landing', landing_pad, []).

landing_pad(Request) :-
        member(method(post), Request), !,
        http_read_data(Request, Data, []),
	atom_number(Data.initialinvestment, II),
	atom_number(Data.monthlyinvestment, MI),
	atom_number(Data.monthlyreturnrate, MRR),
	atom_number(Data.nummonths, NM),
	dollar_cost_averaging(II, MI, MRR, NM, Finalvalue),
        reply_html_page(
	    \headers,
	    [
		p([Finalvalue])
	    ]),
	portray_clause(Data).

headers --> html([
		       title('DCA-calculator.com'),
		       meta([name='viewport', content='width=device-width, initial-scale=1'])
		   ]).
