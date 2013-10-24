-module(erl_net_caller).

-export([start/2]).

start(Module, Port) ->
    case erl_net_socket:listen(Port) of
    {ok, Listener} ->
        accept_loop(Module, Listener);
    _ ->
        error
    end.   

%**********************************************************************************************************************
%**********************************************************************************************************************
%**********************************************************************************************************************

receive_loop(Module, Socket) ->
    case erl_net_socket:recv(Socket) of
    {ok, Stream} ->
        case Module:handle_call(Stream) of
        {reply, Res} ->
            erl_net_socket:send(Res, Socket);
        close ->
            erl_net_socket:close(Socket);
        _ ->
            error
        end,
        receive_loop(Module, Socket);
    {error, closed} ->
        {ok, closed}
    end.

accept_loop(Module, Listener) ->
    case erl_net_socket:accept(Listener) of
    {ok, Socket} ->
        spawn(fun () -> receive_loop(Module, Socket) end);
    _ ->
        error
    end,
    accept_loop(Module, Listener).
