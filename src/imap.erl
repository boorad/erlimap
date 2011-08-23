-module(imap).

-include("imap.hrl").

-behaviour(gen_server).

-export([open_account/5, close_account/1,
         examine/2, search/2, fetch/3
        ]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3,
         terminate/2]).

%%%-----------------
%%% Client functions
%%%-----------------

open_account(ConnType, Host, Port, User, Pass) ->
  gen_server:start_link(?MODULE, {ConnType, Host, Port, User, Pass}, []).

close_account(Account) ->
  gen_server:call(Account, close_account).

examine(Account, Mailbox) ->
  gen_server:call(Account, {examine, Mailbox}).

search(Account, SearchKeys) ->
  gen_server:call(Account, {search, SearchKeys}).

fetch(Account, SequenceSet, MsgDataItems) ->
  gen_server:call(Account, {fetch, SequenceSet, MsgDataItems}).

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
  end;
handle_call({examine, Mailbox}, _From, Conn) ->
  {reply, imap_fsm:examine(Conn, Mailbox), Conn};
handle_call({search, SearchKeys}, _From, Conn) ->
  {reply, imap_fsm:search(Conn, SearchKeys), Conn};
handle_call({fetch, SequenceSet, MsgDataItems}, _From, Conn) ->
  {reply, imap_fsm:fetch(Conn, SequenceSet, MsgDataItems), Conn};
handle_call(_, _From, Conn) ->
  {reply, ignored, Conn}.



handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(normal, _State) ->
  ok;
terminate(Reason, _State) ->
  {error, Reason}.

%%%-----------
%%% tests
%%%-----------

-ifdef(TEST).

%other_modules_test() ->
%  ok = eunit:test([imap_fsm, imap_util, imap_re, imap_resp, imap_cmd]).

test_account(ConnType, Host, Port, User, Pass) ->
  %% open
  {ok, Account} = open_account(ConnType, Host, Port, User, Pass),

  %% examine
  Examine = examine(Account, imap_util:quote_mbox("[Gmail]/All Mail")),
  ?LOG_DEBUG("Examine: ~p~n", [Examine]),

  %% search
  {ok, SearchResponses} = search(Account, [all]),
  Ids = get_search_ids(SearchResponses),
  TargetIds = last_x(Ids, 10),
  ?LOG_DEBUG("Search Target Ids: ~p~n", [TargetIds]),

  %% process msgs
  process_messages(Account, TargetIds, fun process_message/1),

  %% close
  ok = close_account(Account).

account_test_() ->
  {ok, AccountsConf} = file:consult("../priv/test_account.conf"),
  GenTest = fun(AccountConf) ->
                {ConnType, Host, Port, User, Pass} = AccountConf,
                fun() -> test_account(ConnType, Host, Port, User, Pass) end
            end,
  lists:map(GenTest, AccountsConf).

%% TODO: move these funs below to userland app
get_search_ids([]) -> undefined;
get_search_ids([{response, untagged, "SEARCH", Ids}|_]) ->
  Ids; %% assuming only one of these per SEARCH command :\
get_search_ids([_|Rest]) ->
  get_search_ids(Rest).

last_x(List, X) ->
  Length = length(List),
  Start = case (X >= Length) of
            true  -> 1;
            false -> Length - X + 1
          end,
  lists:sublist(List, Start, X).

process_messages(_, [], _) -> ok;
process_messages(Account, [Id|Rest], ProcessFun) ->
  Msg = fetch(Account, Id, "RFC822.HEADER"),
  ProcessFun(Msg),
  process_messages(Account, Rest, ProcessFun).

process_message(Msg) ->
  ?LOG_DEBUG("Msg: ~p~n", [Msg]).

-endif.
