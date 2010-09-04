-module(imap_util).

-include("imap.hrl").

-export([identity_fun/1, catch_first_error/1, extract_dict_element/2, clean_line/1,
		start_ssl/0, sock_connect/4, sock_send/3, sock_close/2, gen_tag/0]).

%%%------------------
%%% Utility functions
%%%------------------

identity_fun(X) -> X.

catch_first_error(Fun) ->
	try Fun()
	catch
		error:{badmatch, {error, Reason}} -> {error, Reason}
	end.

extract_dict_element(Key, Dict) ->
	{ok, Val} = dict:find(Key, Dict),
	{ok, Val, dict:erase(Key, Dict)}.

clean_line(Line) ->
	Line2 = string:strip(Line, right, $\n),
	string:strip(Line2, right, $\r).

start_ssl() ->
	case ssl:start() of
		ok -> ok;
		{error, {already_started, ssl}} -> ok
	end.

sock_connect(tcp, Host, Port, Opts) ->
	gen_tcp:connect(Host, Port, Opts);
sock_connect(ssl, Host, Port, Opts) ->
	ok = start_ssl(),
	ssl:connect(Host, Port, Opts).

sock_send(tcp, Sock, Data) ->
	gen_tcp:send(Sock, Data);
sock_send(ssl, Sock, Data) ->
	ssl:send(Sock, Data).

sock_close(tcp, Sock) ->
	gen_tcp:close(Sock);
sock_close(ssl, Sock) ->
	ssl:close(Sock).

gen_tag() ->
	Tag = case get(last_tag) of
		undefined -> 0;
		LastTag -> LastTag + 1
	end,
	put(last_tag, Tag),
	string:right(integer_to_list(Tag), 3, $0).

%%%-----------
%%% Unit tests
%%%-----------

catch_first_error_test() ->
	{error, foobar} = catch_first_error(fun force_badmatch/0).

force_badmatch() ->
	ok = ok,
	ok = return_error(foobar),
	ok = ok.

return_error(Reason) ->
	{error, Reason}.
