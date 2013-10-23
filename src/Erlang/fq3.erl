-module(fq3).

-export([start/0]).

-define(CALL, 0).
-define(CAST, 1).

start() ->
    spawn(fun () -> listen(37950) end).

%**********************************************************************************************************************
%**********************************************************************************************************************
%**********************************************************************************************************************

handle_call(_ByteStream, _Socket) ->
    not_implemented.

handle_cast(ByteStream, Socket) ->
    case ByteStream of
    <<0, Rest/binary>> ->
        gen_tcp:send(Socket, <<?CAST, "Pong">>),
        Rest;
    _ ->
        bad_msg
    end.

handle_receive(ByteStream, Socket) ->
    case ByteStream of
    <<?CALL, Rest/binary>> ->
        handle_receive(handle_call(Rest, Socket), Socket);
    <<?CAST, Rest/binary>> ->
        handle_receive(handle_cast(Rest, Socket), Socket);
    _ ->
        ok
    end.

receive_loop(Socket) ->
    case gen_tcp:recv(Socket, 0) of
    {ok, ByteStream} ->
        handle_receive(ByteStream, Socket),
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
