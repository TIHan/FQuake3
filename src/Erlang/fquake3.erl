-module(fquake3).

-export([init/0, add_bot/1, execute/1]).

init() ->
	spawn(fun () -> listen(37950) end),
	ok.

add_bot(BotName) ->
	Pid = whereis(fq3),
	Pid ! "addbot " ++ BotName,
	ok.

execute(Text) ->
	Pid = whereis(fq3),
	Pid ! Text,
	ok.

process_loop(Socket) ->
	receive
	"Stop" ->
		ok;
	Msg -> 
		gen_tcp:send(Socket, list_to_binary(Msg)),
		process_loop(Socket)
	end.

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
        Pid = whereis(fq3),
        Pid ! "Stop",
        {ok, closed}
    end.

accept_loop(Listener) ->
    case gen_tcp:accept(Listener) of
    {ok, Socket} ->
    	io:format("Connected to FQuake3~n"),
        spawn(fun () -> receive_loop(Socket) end),
        Pid = spawn(fun () -> process_loop(Socket) end),
        register(fq3, Pid),
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
