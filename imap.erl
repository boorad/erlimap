-module(imap).

-include("imap.hrl").

-behaviour(gen_server).

-compile(export_all). % FIXME

open_account(Host, Port, User, Pass) ->
	gen_server:start_link(?MODULE, {Host, Port, User, Pass}, []).

close_account(Account) ->
	gen_server:call(Account, close_account).

init({Host, Port, User, Pass}) ->
	% FIXME: comprobar is host errorneo
	{ok, Conn} = imap_fsm:connect(Host, Port),
	timer:sleep(1000), %FIXME
	% FIXME: comprobar is logeo errorneo
	ok = imap_fsm:login(Conn, User, Pass),
	{ok, Conn}.

handle_call(close_account, From, Conn) ->
	ok = imap_fsm:logout(Conn),
	ok = imap_fsm:disconnect(Conn),
	{stop, normal, ok, Conn}.

terminate(normal, State) ->
	ok.
