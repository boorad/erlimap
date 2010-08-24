-include_lib("eunit/include/eunit.hrl").

-record(state_data, {socket, socket_type, enqueued_commands = [], server_capabilities = [],
		commands_pending_response = dict:new(), untagged_responses_received = []}).

-define(DEBUG, true).

-define(LOG_ERROR(Fun, Format, Data), error_logger:error_msg("~p:~p(): " ++ Format ++ "~n", [?MODULE, Fun] ++ Data)).
-define(LOG_WARNING(Fun, Format, Data), error_logger:warning_msg("~p:~p(): " ++ Format ++ "~n", [?MODULE, Fun] ++ Data)).
-define(LOG_INFO(Format, Data), error_logger:info_msg(Format ++ "~n", Data)).

-ifdef(DEBUG).
-define(LOG_DEBUG(Format, Data), ?LOG_INFO("*** DEBUG: " ++ Format ++ " ***", Data)).
-else.
-define(LOG_DEBUG(Format, Data), false).
-endif.
