-module(fq3).

-export([start/0]).

start() ->
    spawn(fun () -> listen(37950) end).

%**********************************************************************************************************************
%**********************************************************************************************************************
%**********************************************************************************************************************

send(Res, Socket) ->
    gen_tcp:send(Socket, Res).

handle_byte_stream(ByteStream, Socket) ->
    case ByteStream of
    <<0>> ->
        io:format("Ping request.~n"),
        send(<<0>>, Socket);
    _ ->
        ok
    end.

receive_loop(Socket) ->
    case gen_tcp:recv(Socket, 0) of
    {ok, ByteStream} ->
        handle_byte_stream(ByteStream, Socket),
        receive_loop(Socket);
    {error, closed} ->
        io:format("Disconnected from FQuake3.~n"),
        {ok, closed}
    end.

accept_loop(Listener) ->
    case gen_tcp:accept(Listener) of
    {ok, Socket} ->
    	io:format("Connected to FQuake3.~n"),
        spawn(fun () -> receive_loop(Socket) end),
        accept_loop(Listener);
    _ ->
        accept_loop(Listener)
    end.

listen(Port) ->
    case gen_tcp:listen(Port, [binary, {packet, 0}, {active, false}, {nodelay, true}]) of
    {ok, Listener} ->
        accept_loop(Listener);
    _ ->
        stop
    end.
