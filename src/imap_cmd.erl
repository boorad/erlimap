-module(imap_cmd).

-include("imap.hrl").

-export([send_command/3]).

%%%------------------------
%%% Commands send functions
%%%------------------------

send_command(SockType, Sock, Command) ->
  case prepare_command(Command) of
    {Tag, Line} ->
      case imap_util:sock_send(SockType, Sock, Line ++ "\r\n") of
        ok ->
          ?LOG_DEBUG("line sent: ~s", [Line]),
          {ok, Tag};
        {error, Reason} ->
          {error, Reason}
      end;
    Line ->
      imap_util:sock_send(SockType, Sock, Line ++ "\r\n")
  end.

prepare_command({command, login, {User, Pass}}) ->
  prepare_tagged_command("LOGIN ~s ~s", [User, Pass]);
prepare_command({command, logout, {}}) ->
  prepare_tagged_command("LOGOUT", []);
prepare_command({command, examine, Mailbox}) ->
  prepare_tagged_command("EXAMINE ~s", [Mailbox]);
prepare_command({command, noop, {}}) ->
  prepare_tagged_command("NOOP", []).

prepare_tagged_command(Format, Args) ->
  Tag = imap_util:gen_tag(),
  Line = lists:flatten(io_lib:format("~s " ++ Format, [Tag | Args])),
  {Tag, Line}.

%%%-----------
%%% tests
%%%-----------

-ifdef(TEST).

examine_test() ->
  ?assertEqual({"000", "000 EXAMINE [Gmail]/All Mail"},
               prepare_command({command, examine, "[Gmail]/All Mail"})).

-endif.
