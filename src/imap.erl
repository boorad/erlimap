-module(imap).

-include("imap.hrl").

-behaviour(gen_server).

-export([open_account/5, close_account/1,
         select/2, examine/2, search/2, fetch/3, store/4
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

select(Account, Mailbox) ->
  gen_server:call(Account, {select, Mailbox}).

examine(Account, Mailbox) ->
  gen_server:call(Account, {examine, Mailbox}).

search(Account, SearchKeys) ->
  gen_server:call(Account, {search, SearchKeys}).

fetch(Account, SequenceSet, MsgDataItems) ->
  gen_server:call(Account, {fetch, SequenceSet, MsgDataItems}).

store(Account, SequenceSet, Flags, Action) ->
  gen_server:call(Account, {store, SequenceSet, Flags, Action}).
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
  
handle_call({select, Mailbox}, _From, Conn) ->
  {reply, imap_fsm:select(Conn, Mailbox), Conn};
handle_call({examine, Mailbox}, _From, Conn) ->
  {reply, imap_fsm:examine(Conn, Mailbox), Conn};
handle_call({search, SearchKeys}, _From, Conn) ->
  {reply, imap_fsm:search(Conn, SearchKeys), Conn};
handle_call({fetch, SequenceSet, MsgDataItems}, _From, Conn) ->
  {reply, imap_fsm:fetch(Conn, SequenceSet, MsgDataItems), Conn};
handle_call({store, SequenceSet, Flags, Action}, _From, Conn) ->
  {reply, imap_fsm:store(Conn, SequenceSet, Flags, Action), Conn};
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
-include_lib("eunit/include/eunit.hrl").


-endif.
