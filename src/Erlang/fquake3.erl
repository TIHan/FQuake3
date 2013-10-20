-module(fquake3).

-export([init/0, add_bot/1, execute/1]).
-export([move_forward/0, move_backward/0, turn_left/0, turn_right/0, stop_moving/0, attack/0, stop_attacking/0]).

init() ->
	spawn(fun () -> listen(37950) end),
	ok.

add_bot(BotName) ->
	Pid = whereis(fq3),
	Pid ! "addbot " ++ BotName,
	ok.

move_forward() ->
	Pid = whereis(fq3),
	Pid ! "+forward; -back",
	ok.

move_backward() ->
	Pid = whereis(fq3),
	Pid ! "+back; -forward",
	ok.

turn_left() ->
	Pid = whereis(fq3),
	Pid ! "+left; -right",
	ok.

turn_right() ->
	Pid = whereis(fq3),
	Pid ! "+right; -left",
	ok.

stop_moving() ->
	Pid = whereis(fq3),
	Pid ! "-forward; -back; -left; -right",
	ok.

attack() ->
	Pid = whereis(fq3),
	Pid ! "+attack",
	ok.

stop_attacking() ->
	Pid = whereis(fq3),
	Pid ! "-attack",
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
