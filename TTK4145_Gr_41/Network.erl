
-module(network).
-export([readMailbox/0]).
-export([initialize_connection/0]).

-define(SERVER_IP, 127.0.0.1).
-define(SERVER_NAME, server).

initialize_connection() ->
	R= io_lib:format("~p",[SERVER_IP]).
	io:format("Here: ~p~n",[R]).
	%net_adm:ping(SERVER_NAME@SERVER_IP).




readMailbox() ->
	receive
		{msg, Msg} -> io:format("Received message: ~p~n",[Msg]);
		{err, Err} -> io:format("Error message: ~p~n",[Err])
	end,
	readMailbox().