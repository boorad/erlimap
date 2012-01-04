-module(imap_util).

-include("imap.hrl").

-export([identity_fun/1, catch_first_error/1, extract_dict_element/2,
         clean_line/1, start_ssl/0, sock_connect/4, sock_send/3, sock_close/2,
         gen_tag/0, quote_mbox/1, to_key/1]).
-export([to_binary/1, to_int/1, to_list/1, to_float/1, to_atom/1]).

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
  ssl:send(Sock, to_binary(Data)).

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

quote_mbox(Orig) ->
  lists:flatten([34,Orig,34]).

to_key(L) when is_list(L) ->
  to_key(L, []).

to_key([], Acc) -> Acc;
to_key([H|T], Acc) ->
  Key = string:to_upper(to_list(H)),
  to_key(T, [Key | Acc]).

to_binary(undefined)            -> undefined;
to_binary(V) when is_integer(V) -> to_binary(?i2l(V));
to_binary(V) when is_list(V)    -> to_binary(?l2b(V));
to_binary(V) when is_float(V)   -> to_binary(float_to_list(V));
to_binary(V) when is_binary(V)  -> V.

to_int(undefined)            -> undefined;
to_int(V) when is_float(V)   -> round(V);
to_int(V) when is_integer(V) -> V;
to_int(V) when is_list(V)    -> ?l2i(V);
to_int(V) when is_binary(V)  -> to_int(?b2l(V)).

to_list(undefined)            -> undefined;
to_list(V) when is_integer(V) -> integer_to_list(V);
to_list(V) when is_list(V)    -> V;
to_list(V) when is_binary(V)  -> ?b2l(V);
to_list(V) when is_atom(V)    -> ?a2l(V).

to_float(undefined)            -> undefined;
to_float(V) when is_integer(V) -> V + 0.0;
to_float(V) when is_list(V)    -> list_to_float(V);
to_float(V) when is_binary(V)  -> to_float(?b2l(V)).

to_atom(undefined)         -> undefined;
to_atom(V) when is_atom(V) -> V;
to_atom(V) when is_list(V) -> list_to_atom(V);
to_atom(V)                 -> to_atom(to_list(V)).

%%%-----------
%%% internal
%%%-----------


%%%-----------
%%% tests
%%%-----------

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

catch_first_error_test() ->
    {error, foobar} = catch_first_error(fun force_badmatch/0).

force_badmatch() ->
  ok = ok,
  ok = return_error(foobar),
  ok = ok.

return_error(Reason) ->
  {error, Reason}.

-endif.