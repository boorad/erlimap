-module(imap).

-include("imap.hrl").

-behaviour(gen_server).

-export([open_account/5, close_account/1]).

-export([init/1, handle_call/3, terminate/2]).

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
	try
		{ok, Conn} = case ConnType of
			tcp -> imap_fsm:connect(Host, Port);
			ssl -> imap_fsm:connect_ssl(Host, Port)
		end,
		ok = imap_fsm:login(Conn, User, Pass),
		{ok, Conn}
	catch
		error:{badmatch, {error, Reason}} -> {stop, Reason}
	end.

handle_call(close_account, _From, Conn) ->
	try
		ok = imap_fsm:logout(Conn),
		ok = imap_fsm:disconnect(Conn),
		{stop, normal, ok, Conn}
	catch
		error:{badmatch, {error, Reason}} -> {stop, Reason, {error, Reason}, Conn}
	end.

terminate(normal, _State) ->
	ok;
terminate(Reason, _State) ->
	{error, Reason}.

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
