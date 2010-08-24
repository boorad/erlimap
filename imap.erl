-module(imap).

-include("imap.hrl").

-behaviour(gen_server).

-compile(export_all). % FIXME

%%%-----------------
%%% Client functions
%%%-----------------

open_account(ConnType, Host, Port, User, Pass) ->
	gen_server:start_link(?MODULE, {ConnType, Host, Port, User, Pass}, []).

close_account(Account) ->
	gen_server:call(Account, close_account).

%%%-------------------
%%% Callback functions
%%%-------------------

init({ConnType, Host, Port, User, Pass}) ->
	case ConnType of
		% FIXME: comprobar is host errorneo
		tcp -> {ok, Conn} = imap_fsm:connect(Host, Port);
		ssl -> {ok, Conn} = imap_fsm:connect_ssl(Host, Port)
	end,
	% FIXME: comprobar is logeo errorneo
	ok = imap_fsm:login(Conn, User, Pass),
	{ok, Conn}.

handle_call(close_account, From, Conn) ->
	ok = imap_fsm:logout(Conn),
	ok = imap_fsm:disconnect(Conn),
	{stop, normal, ok, Conn}.

terminate(normal, State) ->
	ok.

%%%-----------
%%% Unit tests
%%%-----------

other_modules_test() ->
	ok = eunit:test([imap_fsm, imap_util, imap_re, imap_resp, imap_cmd]).

test_account(ConnType, Host, Port, User, Pass) ->
	{ok, Account} = open_account(ConnType, Host, Port, User, Pass),
	ok = close_account(Account).

account_test_() ->
	{ok, AccountsConf} = file:consult("test_account.conf"),
	GenTest = fun(AccountConf) ->
			{ConnType, Host, Port, User, Pass} = AccountConf,
			fun() -> test_account(ConnType, Host, Port, User, Pass) end
	end,
	lists:map(GenTest, AccountsConf).
