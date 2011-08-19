-module(imap_resp).

-include("imap.hrl").

-export([parse_response/1, analyze_response/4]).

%%%----------------------------
%%% Responses parsing functions
%%%----------------------------

parse_response(Line) ->
  Resp = string:to_upper(lists:nth(2, string:tokens(Line, " "))),
  case int_parse_response(Resp, Line) of
    {match, Response} -> {ok, parse_tag(Response)};
    nomatch -> {error, nomatch}
  end.

%% TODO TODO TODO
%% LOGIN
analyze_response(not_authenticated, Responses, {command, login, {_, _}}, From) ->
  case get_response_result(Responses) of
    {result, ok} ->
      send_client_response_result(ok, From),
      authenticated;
    {result, Other} ->
      send_client_response_result(Other, From),
      not_authenticated
  end;
%% LOGOUT
analyze_response(StateName, Responses, {command, logout, {}}, From) ->
  HasBye = check_response_has(Responses, "BYE"),
  {result, Result} = get_response_result(Responses),
  case {HasBye, Result} of
    {true, ok} ->
      send_client_response_result(ok, From),
      logout;
    {_, Result} ->
      send_client_response_result(Result, From),
      StateName
  end;
%% EXAMINE
analyze_response(authenticated, Responses, {command, examine, _}, From) ->
  send_client_response_result({ok, Responses}, From),
  authenticated;
%% NOOP
analyze_response(StateName, Responses, {command, noop, {}}, From) ->
  {result, Result} = get_response_result(Responses),
  send_client_response_result(Result, From),
  StateName.

%%%-----------
%%% internal
%%%-----------

parse_tag({response, "+", Response, Args}) ->
  {response, tag_continue, Response, Args};
parse_tag({response, "*", Response, Args}) ->
  {response, untagged, Response, Args};
parse_tag({response, _Tag, _, _} = ResponseTuple) ->
  ResponseTuple.

%% TODO TODO TODO

send_client_response_result(ok, From) ->
  gen_fsm:reply(From, ok);
send_client_response_result({ok, Reply}, From) ->
  gen_fsm:reply(From, {ok, Reply});
send_client_response_result(no, From) ->
  gen_fsm:reply(From, {error, no_response});
send_client_response_result(bad, From) ->
  gen_fsm:reply(From, {error, bad_response}).

get_response_result(Responses) ->
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

int_parse_response("CAPABILITY", Line) ->
  imap_re:match_capability_response(Line);
int_parse_response("OK", Line) ->
  imap_re:match_ok_response(Line);
int_parse_response("NO", Line) ->
  imap_re:match_no_response(Line);
int_parse_response("BAD", Line) ->
  imap_re:match_bad_response(Line);
int_parse_response("BYE", Line) ->
  imap_re:match_bye_response(Line);
int_parse_response("FLAGS", Line) ->
  imap_re:match_flags_response(Line);
int_parse_response(Resp, Line) ->
  try
    Third = string:to_upper(lists:nth(3, string:tokens(Line, " "))),
    case try_third_term(Third, Line) of
      {match, Response} -> {match, Response};
      _ ->
        ?LOG_ERROR(int_parse_response, "Unhandled response: ~p~n~p~n",
                   [Resp,Line]),
        nomatch
    end
  catch
    _:Err ->
      ?LOG_DEBUG("try_third_term error: ~p", [Err]),
      nomatch
  end.

try_third_term("EXISTS", Line) ->
  imap_re:match_exists_response(Line);
try_third_term("RECENT", Line) ->
  imap_re:match_recent_response(Line);
try_third_term(_,_) ->
  nomatch.

%%%-----------
%%% tests
%%%-----------

parse_response_test() ->
  {ok, {response, untagged, "OK", []}} = parse_response("* OK"),
  {ok, {response, "a01", "OK", ["IMAP4rev1", "foo"]}} =
    parse_response("a01 ok [capability IMAP4rev1 foo] Hey you."),
  {ok, {response, "1234", "NO", "bad boy"}} = parse_response("1234 no bad boy"),
  {ok, {response, untagged, "NO", ""}} = parse_response("* NO"),
  {ok, {response, "XyZ", "BAD", ""}} = parse_response("XyZ bad"),
  {ok, {response, untagged, "BAD", "go to hell"}} =
    parse_response("* BAD go to hell"),
  {ok, {response, untagged, "BYE", ""}} = parse_response("* BYE see you soon"),
  {ok, {response, untagged, "CAPABILITY", "IMAP4rev1 UNSELECT IDLE NAMESPACE "
        "QUOTA ID XLIST CHILDREN X-GM-EXT-1 UIDPLUS COMPRESS=DEFLATE\r"}} =
    parse_response("* CAPABILITY IMAP4rev1 UNSELECT IDLE NAMESPACE QUOTA ID "
                   "XLIST CHILDREN X-GM-EXT-1 UIDPLUS COMPRESS=DEFLATE\r\n"),
  {error, nomatch} = parse_response("01 BYE").

int_parse_response_test() ->
  ?assertEqual({match, {response, "*", "EXISTS", "28"}},
               int_parse_response("28", "* 28 EXISTS")),
  ok.
