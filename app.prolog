:- module(app, [ server/1]).

:- use_module(library(http/http_client)).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_error)).
:- use_module(library(http/http_log)).
:- use_module(library(http/http_dyn_workers)).
:- use_module(library(http/js_write)).

:- use_module(yahoofinance).
:- use_module(dca).
:- use_module(utils).

server(Port) :-
    http_server(http_dispatch, [port(Port), workers(16)]).

:- http_handler(/, home_page, []).

home_page(_Request) :-
    reply_html_page(
	\headers,
	[
	    div(class='container', [
		    h1([align='center'], 'DCA-calculator.com'),

		    section([id='calculator'], [
				form([method='post', action='/results'], [
					 label([for='ticker'], ['Symbole :']),
					 input([type='text', name='ticker', placeholder='Exemple: PSP5.PA']), br(''),
					 label([for='initialinvestment'], ['Investissement initial :']),
					 input([type='number', name='initialinvestment', placeholder='Exemple: 1000']), br(''),
					 label([for='monthlyinvestment'], ['Investissement mensuel :']),
					 input([type='number', name='monthlyinvestment', placeholder='Exemple: 1000']), br(''),
					 label([for='monthlyreturnrate'], ['Rendement mensuel :']),
					 input([type='number', name='monthlyreturnrate', step='0.1', placeholder='Exemple: 0.5']), br(''),
					 label([for='nummonths'], ['Nombre de mois :']),
					 input([type='number', name='nummonths', placeholder='Exemple: 24']), br(''),
					 label([for='startdate'], ['Date de départ :']),
					 input([type='date', name='startdate'], []), br(''),
					 label([for='enddate'], ['Date de fin :']),
					 input([type='date', name='enddate'], []), br(''),
					 input([type='submit', value="Calculer"])
				     ])
			    ])
		]),
	    style([], [
		      ':root {
  		      	     --color-primary: #da1d50; /* brand color */
  			     --grid-maxWidth: 50rem; /* max container width 1080px */
			}

			h1 { color: #da1d50; font-size: 2vw; font-weight: 800; line-height: 80px; margin: 0 0 24px; text-align: center; text-transform: uppercase; }'
		  ])
	]).

:- http_handler('/results', results_page, []).

results_page(Request) :-
        member(method(post), Request), !,
        http_read_data(Request, Data, []),
	atom_number(Data.initialinvestment, II),
	atom_number(Data.monthlyinvestment, MI),
	atom_number(Data.monthlyreturnrate, MRR),
	atom_number(Data.nummonths, NM),
	yahoo_finance(Data.ticker, Data.startdate, Data.enddate, '1d', YFData),
	maplist(get_first_item, YFData, Dates),
	maplist(get_rest, YFData, Prices),
	replace_nulls(Prices, _, PricesNonNull),
	lists_to_string(PricesNonNull, PricesStr),
	dollar_cost_averaging(II, MI, MRR, NM, Finalvalue),
        reply_html_page(
	    \headers,
	    [
		p([Finalvalue]),
		p([Dates]),
		p([PricesStr]),
		div([], [
			canvas([id='myChart'], [])
		    ]),
		\js_script({| javascript(Dates, Prices)  ||
			      const ctx = document.getElementById('myChart');

				new Chart(ctx, {
					 type: 'line',
					 data: {
					     labels: Dates,
					     datasets: [{
							       label: 'Prix',
							       data: Prices,
							       fill: false,
							       borderColor: 'rgb(75, 192, 192)',
							       tension: 0.1
							   }]
					 },
					 options: {
					     scales: {
						 y: {
						     beginAtZero: true
						 }
					     }
					 }
				     });			    
			    |})
		]),
	portray_clause(Data).

headers --> html([
		       title('DCA-calculator.com'),
		       meta([name='viewport', content='width=device-width, initial-scale=1']),
		       link([rel='stylesheet', href='https://unpkg.com/chota@latest']),
		       script([src='https://cdn.jsdelivr.net/npm/chart.js'], [])
		       ]).
