-module (erl_net).

-export([start/1]).
-export([handle_call/1]).

-define(PING, <<255>>).
-define(PONG, <<255>>).

start(Port) ->
	spawn(fun () -> erl_net_caller:start(?MODULE, Port) end).

handle_call(Stream) ->
	case Stream of
	?PING ->
		{reply, ?PONG};
	_ ->
		close
	end.