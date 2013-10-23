-module(fq3_net).

-export([create_ip_socket/2]).

create_ip_socket(NetInterface, Port) ->
	case NetInterface of
	undefined -> 
		io:format("Opening IP socket: localhost:~p~n", [Port]);
	X -> 
		io:format("Opening IP socket: ~p:~p~n", [X, Port])
	end,

	{ok, Address} = inet:parse_ipv4_address(NetInterface),
	{ok, Socket} = gen_udp:open(Port, [
		{ip, Address},
		{port, Port},
		inet,
		{broadcast, true}
	]),

	case Socket of
	{error, _} -> 0;
	_ -> prim_inet:getfd(Socket)
	end.

