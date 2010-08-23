-module(imap_resp).

-include("imap.hrl").

-export([parse_response/1, analyze_response/4]).

%%%----------------------------
%%% Responses parsing functions
%%%----------------------------

parse_response(Line) ->
	Result = case string:to_upper(lists:nth(2, string:tokens(Line, " "))) of
		"OK" -> imap_re:match_ok_response(Line);
		"NO" -> imap_re:match_no_response(Line);
		"BAD" -> imap_re:match_bad_response(Line);
		"BYE" -> imap_re:match_bye_response(Line)
	end,
	case Result of
		{match, Response} -> {ok, parse_tag(Response)};
		nomatch -> {error, nomatch}
	end.

parse_tag({response, "+", Response, Args}) ->
	{response, tag_continue, Response, Args};
parse_tag({response, "*", Response, Args}) ->
	{response, untagged, Response, Args};
parse_tag({response, _Tag, _, _} = ResponseTuple) ->
	ResponseTuple.

% TODO TODO TODO
analyze_response(not_authenticated, Responses, {command, login, {_, _}}, From) ->
	send_client_response_result(Responses, From),
	authenticated;
analyze_response(StateName, Responses, {command, logout, {}}, From) ->
	CheckHasBye = fun(Responses) -> check_response_has(Responses, "BYE") end,
	case send_client_if_invalid_response(Responses, From, CheckHasBye) of
		false ->
			send_client_response_result(Responses, From),
			logout;
		true ->
			StateName
	end;
analyze_response(StateName, Responses, {command, noop, {}}, From) ->
	send_client_response_result(Responses, From),
	StateName.
% TODO TODO TODO

send_client_response_result(Responses, From) ->
	case check_response_result(Responses) of
		{result, ok} -> gen_fsm:reply(From, ok);
		{result, no} -> gen_fsm:reply(From, {error, no_response});
		{result, bad} -> gen_fsm:reply(From, {error, bad_response})
	end.

send_client_if_invalid_response(Responses, From, CheckFun) ->
	case CheckFun(Responses) of
		true -> false;
		false -> gen_fsm:reply(From, {error, invalid_response})
	end.

check_response_result(Responses) ->
	case lists:last(Responses) of
		{response, Tag, "OK", _} when not is_atom(Tag) -> {result, ok};
		{response, Tag, "NO", _} when not is_atom(Tag) -> {result, no};
		{response, Tag, "BAD", _} when not is_atom(Tag) -> {result, bad}
	end.

check_response_has(Responses, What) ->
	case lists:keysearch(What, 3, Responses) of
		{value, _} -> true;
		false -> false
	end.

%%%-----------
%%% Unit tests
%%%-----------

parse_response_test() ->
	{ok, {response, untagged, "OK", []}} = parse_response("* OK"),
	{ok, {response, "a01", "OK", ["IMAP4rev1", "foo"]}} =
			parse_response("a01 ok [capability IMAP4rev1 foo] Hey you."),
	{ok, {response, "1234", "NO", "bad boy"}} = parse_response("1234 no bad boy"),
	{ok, {response, untagged, "NO", ""}} = parse_response("* NO"),
	{ok, {response, "XyZ", "BAD", ""}} = parse_response("XyZ bad"),
	{ok, {response, untagged, "BAD", "go to hell"}} = parse_response("* BAD go to hell"),
	{ok, {response, untagged, "BYE", ""}} = parse_response("* BYE see you soon"),
	{error, nomatch} = parse_response("01 BYE").
