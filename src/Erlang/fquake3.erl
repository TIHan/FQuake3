-module(fquake3).

-export([init/0]).

init() ->
	spawn(fun () -> listen(37950) end).

receive_loop(Socket) ->
    case gen_tcp:recv(Socket, 0) of
    {ok, Bytes} ->
    	case Bytes of
    	<<0>> ->
    		io:format("Ping Request~n"),
    		gen_tcp:send(Socket, <<"Pong">>);
    	_ ->
    		io:format("Bad Message~n")
    	end,
        receive_loop(Socket);
    {error, closed} ->
        io:format("Disconnected from FQuake3~n"),
        {ok, closed}
    end.

accept_loop(Listener) ->
    case gen_tcp:accept(Listener) of
    {ok, Socket} ->
    	io:format("Connected to FQuake3~n"),
        spawn(fun () -> receive_loop(Socket) end),
        accept_loop(Listener);
    _ ->
        accept_loop(Listener)
    end.

listen(Port) ->
    case gen_tcp:listen(Port, [binary, {packet, 0}, {active, false}]) of
    {ok, Listener} ->
        accept_loop(Listener);
    _ ->
        stop
    end.
