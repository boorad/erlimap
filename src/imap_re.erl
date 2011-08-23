-module(imap_re).

-include("imap.hrl").

-export([match_capability_response/1,
         match_ok_response/1,
         match_no_response/1,
         match_bad_response/1,
         match_bye_response/1,
         match_flags_response/1,
         match_exists_response/1,
         match_recent_response/1,
         match_search_response/1,
         match_fetch_response/1
        ]).

-define(CAPABILITY_REGEXP, "^(?<TAG>[\\*[:alnum:]]+) (?i)(?<KEYWORD>CAPABILITY) (?<CAPABILITY>.*)?(?-i)(?: .*)??$").
-define(OK_REGEXP, "^(?<TAG>[\\*[:alnum:]]+) (?i)(?<KEYWORD>OK)( \\[(?<BRACKETED>.*)\\])?(?-i)(?: (?<REST>.*))??$").
-define(NO_REGEXP, "^(?<TAG>[\\*[:alnum:]]+) (?i)(?<KEYWORD>NO)(?-i)(?: (?<REASON>.*))??$").
-define(BAD_REGEXP, "^(?<TAG>[\\*[:alnum:]]+) (?i)(?<KEYWORD>BAD)(?-i)(?: (?<REASON>.*))??$").
-define(BYE_REGEXP, "^(?<TAG>\\*) (?i)(?<KEYWORD>BYE)(?-i)(?: .*)??$").
-define(FLAGS_REGEXP, "^(?<TAG>[\\*[:alnum:]]+) (?i)(?<KEYWORD>FLAGS)(?-i)(?: (?<FLAG>.*))??$").
-define(EXISTS_REGEXP, "^(?<TAG>[\\*[:alnum:]]+) (?<EXISTS>.*) (?i)(?<KEYWORD>EXISTS)(?-i)$").
-define(RECENT_REGEXP, "^(?<TAG>[\\*[:alnum:]]+) (?<RECENT>.*) (?i)(?<KEYWORD>RECENT)(?-i)$").
-define(SEARCH_REGEXP, "^(?<TAG>[\\*[:alnum:]]+) (?i)(?<KEYWORD>SEARCH)( (?<SEARCH>.*))?(?-i)$").
-define(FETCH_REGEXP, "^(?<TAG>[\\*[:alnum:]]+) (?<ID>.*) (?i)(?<KEYWORD>FETCH)(?-i)(?: (?<REST>.*))??$").

%%%-------------------
%%% api
%%%-------------------

match_capability_response(Str) ->
  match_response(Str, ?CAPABILITY_REGEXP, ["TAG", "KEYWORD", "CAPABILITY"],
                fun tokenize/1).

match_ok_response(Str) ->
  match_response(Str, ?OK_REGEXP, ["TAG", "KEYWORD", "BRACKETED", "REST"],
                 fun tokenize_ok/1).

match_no_response(Str) ->
  match_response(Str, ?NO_REGEXP, ["TAG", "KEYWORD", "REASON"]).

match_bad_response(Str) ->
  match_response(Str, ?BAD_REGEXP, ["TAG", "KEYWORD", "REASON"]).

match_bye_response(Str) ->
  TransformFun = fun(Response) -> erlang:append_element(Response, "") end,
  match_response(Str, ?BYE_REGEXP, ["TAG", "KEYWORD"], TransformFun).

match_flags_response(Str) ->
  match_response(Str, ?FLAGS_REGEXP, ["TAG", "KEYWORD", "FLAG"]).

match_exists_response(Str) ->
  match_response(Str, ?EXISTS_REGEXP, ["TAG", "KEYWORD", "EXISTS"]).

match_recent_response(Str) ->
  match_response(Str, ?RECENT_REGEXP, ["TAG", "KEYWORD", "RECENT"]).

match_search_response(Str) ->
  match_response(Str, ?SEARCH_REGEXP, ["TAG", "KEYWORD", "SEARCH"],
                 fun tokenize/1).

match_fetch_response(Str) ->
  match_response(Str, ?FETCH_REGEXP, ["TAG", "KEYWORD", "ID", "REST"],
                 fun transform_fetch/1).

%%%-----------
%%% internal
%%%-----------

match_response(Str, Regexp, Fields) ->
  match_response(Str, Regexp, Fields, fun imap_util:identity_fun/1).

match_response(Str, Regexp, Fields, TransformFun) ->
  case re:run(trim_lineterms(Str), Regexp, [{capture, Fields, list}]) of
    nomatch ->
      nomatch;
    {match, Matches} ->
      Response = list_to_tuple([response] ++ Matches),
      Keyword = string:to_upper(element(3, Response)),
      Response2 = setelement(3, Response, Keyword),
      Response3 = TransformFun(Response2),
      {match, Response3}
  end.

tokenize_ok({response, Tag, "OK", Bracketed0, Rest0}) ->
  Bracketed1 = string:tokens(Bracketed0, " "),
  Bracketed = lists:map(fun(T) -> trim_whitespace(T) end, Bracketed1),
  Rest = trim_lineterms(Rest0),
  {response, Tag, "OK", {Bracketed, Rest}}.

tokenize(Response) ->
  Tokens0 = string:tokens(element(4, Response), " "),
  Tokens1 = lists:map(fun(T) -> trim_all(T) end, Tokens0),
  setelement(4, Response, Tokens1).

transform_fetch({response, Tag, "FETCH", Id, Info}) ->
  {response, Tag, "FETCH", {Id, Info}}.

trim_all(Input) ->
  trim_lineterms(trim_whitespace(Input)).

trim_whitespace(Input) ->
  re:replace(Input, "\\s+", "", [global, {return, list}]).

trim_lineterms(Input) ->
  re:replace(Input, "(\\r|\\n)", "", [global, {return, list}]).

%%%-----------
%%% tests
%%%-----------

match_capability_response_test() ->
  ?assertEqual({match, {response, "*", "CAPABILITY", ["IMAP4rev1","UNSELECT",
      "IDLE","NAMESPACE","QUOTA","ID","XLIST","CHILDREN","X-GM-EXT-1",
      "UIDPLUS","COMPRESS=DEFLATE"]}},
    match_capability_response("* CAPABILITY IMAP4rev1 UNSELECT IDLE NAMESPACE "
                              "QUOTA ID XLIST CHILDREN X-GM-EXT-1 UIDPLUS "
                              "COMPRESS=DEFLATE\r\n")).

match_ok_response_test() ->
  ?assertEqual({match, {response, "*", "OK", {[],
                        "Gimap ready for requests from IP blah"}}},
               match_ok_response("* OK Gimap ready for requests from IP blah")),
  ?assertEqual({match, {response, "*", "OK", {[], []}}},
               match_ok_response("* OK")),
  ?assertEqual({match, {response, "abc", "OK", {[], []}}} ,
               match_ok_response("abc OK")),
  ?assertEqual({match, {response, "001", "OK", {[], []}}},
               match_ok_response("001 ok")),
  ?assertEqual({match, {response, "001", "OK", {[], "Hey you"}}},
               match_ok_response("001 OK Hey you")),
  ?assertEqual({match, {response, "*", "OK",
    {["CAPABILITY", "IMAP4rev1", "LITERAL+", "SASL-IR", "AUTH=PLAIN"],
     "Ready"}}},
    match_ok_response("* OK [CAPABILITY IMAP4rev1 LITERAL+ SASL-IR "
                                 "AUTH=PLAIN] Ready")),
  ?assertEqual({match, {response, "*", "OK", {["capability", "IMAP4rev1"],[]}}},
               match_ok_response("* ok [capability IMAP4rev1]")),
  ?assertEqual({match, {response, "*", "OK", {["FOO","BAR"], []}}},
               match_ok_response("* OK [FOO BAR]")),
  ?assertEqual({match, {response, "*", "OK", {["FOO","BAR"], "Hey you"}}},
               match_ok_response("* OK [FOO BAR] Hey you")),
  ?assertEqual({match, {response, "002", "OK",
                        {[], "SEARCH completed (Success)"}}},
               match_ok_response("002 OK SEARCH completed (Success)\r\n")),
  ?assertEqual(nomatch, match_ok_response("OK")),
  ?assertEqual(nomatch, match_ok_response("*OK")),
  ?assertEqual(nomatch, match_ok_response("*  OK")),
  ?assertEqual(nomatch, match_ok_response("* 001 OK")),
  ok.

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

match_flags_response_test() ->
  {match, {response, "*", "FLAGS", ""}} = match_flags_response("* FLAGS"),
  {match, {response, "*", "FLAGS", "hey guys"}} = match_flags_response("* flags hey guys"),
  nomatch = match_flags_response("*  FLAGS").

match_exists_response_test() ->
  ?assertEqual({match, {response, "*", "EXISTS", "28"}},
    match_exists_response("* 28 EXISTS")),
  ?assertEqual(nomatch, match_exists_response("* 28 EXISTS extra terms")),
  ok.

match_recent_response_test() ->
  ?assertEqual({match, {response, "*", "RECENT", "8"}},
    match_recent_response("* 8 RECENT")),
  ?assertEqual(nomatch, match_recent_response("* 8 RECENT extra terms\r\n")),
  ok.

match_search_response_test() ->
  ?assertEqual({match, {response, "*", "SEARCH", ["1","2","3"]}},
    match_search_response("* SEARCH 1 2 3\r\n")),
  ?assertEqual({match,{response,"*","SEARCH",[]}},
    match_search_response("* SEARCH\r\n")),
  ok.

match_fetch_response_test() ->
  ?assertEqual({match, {response, "*", "FETCH", {"8", []}}},
    match_fetch_response("* 8 FETCH")),
  ?assertEqual({match, {response, "*", "FETCH", {"8", "extra terms"}}},
    match_fetch_response("* 8 FETCH extra terms\r\n")),
  ok.

trim_whitespace_test() ->
  ?assertEqual("3", trim_whitespace("3\r\n")),
  ?assertEqual("2", trim_whitespace("2")),
  ok.

trim_lineterms_test() ->
  ?assertEqual("a", trim_lineterms("a")),
  ?assertEqual("a", trim_lineterms("a\r")),
  ?assertEqual("a", trim_lineterms("a\r\n")),
  ok.
