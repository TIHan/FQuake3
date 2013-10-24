-module(erl_net_socket).

-export([listen/1, accept/1, send/2, recv/1, close/1]).

listen(Port) ->
	gen_tcp:listen(Port, [binary, {packet, 0}, {active, false}, {nodelay, true}]).

accept(Socket) ->
	gen_tcp:accept(Socket).

send(Data, Socket) ->
    gen_tcp:send(Socket, Data).

recv(Socket) ->
	gen_tcp:recv(Socket, 0).

close(Socket) ->
	gen_tcp:close(Socket).