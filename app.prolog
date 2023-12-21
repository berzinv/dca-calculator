:- module(app, [ server/1]).

:- use_module(library(http/http_client)).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_error)).
:- use_module(library(http/http_unix_daemon)).
:- use_module(library(http/http_log)).
:- use_module(library(http/http_dyn_workers)).

:- use_module(yahoofinance).
:- use_module(dca).

%%%%% web part %%%%%
server(Port) :-
    http_server(http_dispatch, [port(Port), workers(16)]).

:- http_handler(/, say_hi, []).

say_hi(_Request) :-
    reply_html_page(
	\headers,
	[
	    div(class='container', [
		    h1([align='center'], 'DCA-calculator.com'),

		    section([id='calculator'], [
				form([method='post', action='/landing'], [
					 label([for='initialinvestment'], ['Investissement initial :']),
					 input([type='number', name='initialinvestment', placeholder='Exemple: 1000']),br(''),
					 label([for='monthlyinvestment'], ['Investissement mensuel :']),
					 input([type='number', name='monthlyinvestment', placeholder='Exemple: 1000']), br(''),
					 label([for='monthlyreturnrate'], ['Rendement mensuel :']),
					 input([type='number', name='monthlyreturnrate', placeholder='Exemple: 0.5']), br(''),
					 label([for='nummonths'], ['Nombre de mois :']),
					 input([type='number', name='nummonths', type='number', placeholder='Exemple: 24']), br(''),
					 input([type='submit', value="Calculer"])
				     ])
			    ])
		]),
	    style([], [
		      ':root {
  		      	     --color-primary: #da1d50; /* brand color */
  			     --grid-maxWidth: 50rem; /* max container width 1080px */
			}'
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
	    ]).

headers --> html([
		       title('DCA-calculator.com'),
		       meta([name='viewport', content='width=device-width, initial-scale=1']),
		       link([rel='stylesheet', href='https://unpkg.com/chota@latest'])
		   ]).
