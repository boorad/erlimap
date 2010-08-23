-module(imap_re).

-include("imap.hrl").

-export([match_ok_response/1, match_no_response/1, match_bad_response/1, match_bye_response/1]).

-define(OK_REGEXP, "^(?<TAG>[\\*[:alnum:]]+) (?i)(?<KEYWORD>OK)( \\[CAPABILITY (?<CAPABILITY>.*)\\])?(?-i)(?: .*)??$").
-define(NO_REGEXP, "^(?<TAG>[\\*[:alnum:]]+) (?i)(?<KEYWORD>NO)(?-i)(?: (?<REASON>.*))??$").
-define(BAD_REGEXP, "^(?<TAG>[\\*[:alnum:]]+) (?i)(?<KEYWORD>BAD)(?-i)(?: (?<REASON>.*))??$").
-define(BYE_REGEXP, "^(?<TAG>\\*) (?i)(?<KEYWORD>BYE)(?-i)(?: .*)??$").

%%%-------------------
%%% Matching functions
%%%-------------------

match_response(Str, Regexp, Fields) ->
	match_response(Str, Regexp, Fields, fun imap_util:identity_fun/1).

match_response(Str, Regexp, Fields, TransformFun) ->
	case re:run(Str, Regexp, [{capture, Fields, list}]) of
		nomatch ->
			nomatch;
		{match, Matches} ->
			Response = list_to_tuple([response] ++ Matches),
			Keyword = string:to_upper(element(3, Response)),
			Response2 = setelement(3, Response, Keyword),
			Response3 = TransformFun(Response2),
			{match, Response3}
	end.

match_ok_response(Str) ->
	TransformFun = fun(Response) ->
			Tokens = string:tokens(element(4, Response), " "),
			setelement(4, Response, Tokens)
	end,
	match_response(Str, ?OK_REGEXP, ["TAG", "KEYWORD", "CAPABILITY"], TransformFun).

match_no_response(Str) ->
	match_response(Str, ?NO_REGEXP, ["TAG", "KEYWORD", "REASON"]).

match_bad_response(Str) ->
	match_response(Str, ?BAD_REGEXP, ["TAG", "KEYWORD", "REASON"]).

match_bye_response(Str) ->
	TransformFun = fun(Response) -> erlang:append_element(Response, "") end,
	match_response(Str, ?BYE_REGEXP, ["TAG", "KEYWORD"], TransformFun).

%%%-----------
%%% Unit tests
%%%-----------

match_ok_response_test() ->
	{match, {response, "*", "OK", []}} = match_ok_response("* OK"),
	{match, {response, "abc", "OK", []}} = match_ok_response("abc OK"),
	{match, {response, "001", "OK", []}} = match_ok_response("001 ok"),
	{match, {response, "001", "OK", []}} = match_ok_response("001 OK Hey you"),
	{match, {response, "*", "OK", ["IMAP4rev1", "LITERAL+", "SASL-IR", "AUTH=PLAIN"]}} =
			match_ok_response("* OK [CAPABILITY IMAP4rev1 LITERAL+ SASL-IR AUTH=PLAIN] Ready"),
	{match, {response, "*", "OK", ["IMAP4rev1"]}} = match_ok_response("* ok [capability IMAP4rev1]"),
	{match, {response, "*", "OK", []}} = match_ok_response("* OK [FOO BAR]"),
	{match, {response, "*", "OK", []}} = match_ok_response("* OK [FOO BAR] Hey you"),
	nomatch = match_ok_response("OK"),
	nomatch = match_ok_response("*OK"),
	nomatch = match_ok_response("*  OK"),
	nomatch = match_ok_response("* 001 OK").

match_no_response_test() ->
	{match, {response, "*", "NO", ""}} = match_no_response("* NO"),
	{match, {response, "001", "NO", ""}} = match_no_response("001 no"),
	{match, {response, "001", "NO", "Bad boy"}} = match_no_response("001 NO Bad boy"),
	nomatch = match_no_response("*  NO").

match_bad_response_test() ->
	{match, {response, "*", "BAD", ""}} = match_bad_response("* BAD"),
	{match, {response, "001", "BAD", ""}} = match_bad_response("001 BAD"),
	{match, {response, "001", "BAD", "Bad boy"}} = match_bad_response("001 bad Bad boy"),
	nomatch = match_bad_response("*  BAD").

match_bye_response_test() ->
	{match, {response, "*", "BYE", ""}} = match_bye_response("* BYE"),
	{match, {response, "*", "BYE", ""}} = match_bye_response("* bye Sayonara baby"),
	nomatch = match_bye_response("001 BYE"),
	nomatch = match_bye_response("*  BYE").
