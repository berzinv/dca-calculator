:- module(app, [ server/1]).

:- use_module(library(http/http_client)).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_error)).
:- use_module(library(http/http_log)).
:- use_module(library(http/http_dyn_workers)).
:- use_module(library(http/js_write)).
:- use_module(library(lists)).

:- use_module(yahoofinance).
:- use_module(dca).
:- use_module(utils).

server(Port) :-
    http_server(http_dispatch, [port(Port), workers(16)]).

:- http_handler(/, home_page, []).

home_page(_Request) :-
    get_todays_date(TodaysDate),
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
					 label([for='startdate'], ['Date de départ :']),
					 input([type='date', name='startdate', value=TodaysDate], []), br(''),
					 label([for='enddate'], ['Date de fin :']),
					 input([type='date', name='enddate', value=TodaysDate], []), br(''),
					 input([type='submit', value="Calculer"])
				     ])
			    ])
		])
	]).

:- http_handler('/results', results_page, []).

results_page(Request) :-
        member(method(post), Request), !,
        http_read_data(Request, Data, []),
	atom_number(Data.initialinvestment, II),
	atom_number(Data.monthlyinvestment, MI),
	atom_number(Data.monthlyreturnrate, MRR),
%	difference_in_days(Data.startdate, Data.enddate, Nummonths),
	yahoo_finance(Data.ticker, Data.startdate, Data.enddate, '1d', YFData),
	maplist(get_first_item, YFData, Dates),
	maplist(get_rest, YFData, Prices),
	replace_nulls(Prices, _, PricesNonNull),
	flatten(PricesNonNull, FinalPrices),
	dollar_cost_averaging(II, MI, MRR, 24, Finalvalue),
        reply_html_page(
	    \headers,
	    [
		div(class='container', [
			h1([align='center'], 'DCA-calculator.com'),

		p(['Valeur:', Data.ticker]),
		p(['Capital initial:', Data.initialinvestment, '€']),
		p(['Capital final: ', Finalvalue, '€']),
		p(['Somme investie mensuellement:', Data.monthlyinvestment, '€']),
		p(['Période d\'investissement: de ', Data.startdate, ' à ', Data.enddate, ' soit ', '123', ' mois']),
		div([], [
			canvas([id='myChart'], [])
		    ]),
		\js_script({| javascript(Dates, FinalPrices)  ||
			      const ctx = document.getElementById('myChart');

				new Chart(ctx, {
					 type: 'line',
					 data: {
					     labels: Dates,
					     datasets: [{
							       label: 'Prix',
							       data: FinalPrices,
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
			])
		]),
	portray_clause(Data).

headers --> html([
			title('DCA-calculator.com'),
			meta([name='viewport', content='width=device-width, initial-scale=1']),
			link([rel='stylesheet', href='https://unpkg.com/chota@latest']),
			script([src='https://cdn.jsdelivr.net/npm/chart.js'], []),
			style([], [
				  ':root {
  		      	     --color-primary: #da1d50; /* brand color */
  			     --grid-maxWidth: 60rem; /* max container width 1080px */
			}

			h1 { color: #da1d50; font-size: 2vw; font-weight: 800; line-height: 80px; margin: 0 0 24px; text-align: center; text-transform: uppercase; }'
			      ])
		    ]).
