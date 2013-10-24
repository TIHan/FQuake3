-module(erl_net_socket).

listen(Port, AcceptFunc) ->
	gen_tcp:listen(Port, [binary, {packet, 0}, {active, false}, {nodelay, true}]).

accept(Socket) ->
	gen_tcp:accept(Socket).

send(Data, Socket) ->
    gen_tcp:send(Socket, Data).

recv(Socket) ->
	gen_tcp:recv(Socket, 0).