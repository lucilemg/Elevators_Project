-module(network).
-export([init_conn/2,send_message/2,init_listener/2,read_mailbox/0,broadcast/1]).


init_conn(_,5) ->
	io:format("No node is available for connection~n");
init_conn(ConnectionList, Element) ->
	Connection = lists:nth(Element,ConnectionList),
	io:format("Testing with connection: ~p ~n",[Connection]),
	case 	pong == net_adm:ping(Connection) of
			true ->	io:format("Successful connection~n");

			false -> io:format("Failure to connect~n"),
				init_conn(ConnectionList,Element+1)
	end.


read_mailbox() ->
	receive
		{msg, Msg} -> io:format("Received message: ~p~n",[Msg]);
		{err, Err} -> io:format("Error message: ~p~n",[Err]);
		{order, OrderMsg} -> io:format("Received order~n ~p",[OrderMsg]);
			%handle order in scheduler
			%implicit msg ack? if KnownReceivers < NumNodes : broadcast KnownReceivers
		other -> io:format("Some other message retreived~n")
	end,
	read_mailbox().


send_message(PID, Message) ->
	PID ! {msg, Message}.

broadcast(Message) ->
	AllRegisteredNames = global:registered_names(),
	lists:foreach(fun(N) -> 
			send_message(global:whereis_name(N), Message)
			end, AllRegisteredNames).



init_listener(Namelist, Element) ->
	io:format("Testing with PID name: ~p ~n",[lists:nth(Element,Namelist)]),
	case 	yes == global:register_name(lists:nth(Element,Namelist),spawn(network,read_mailbox,[])) of 
			true ->
				io:format("Successful registration ~n");
			false ->
				io:format("Name taken, attempting with different name~n"),
				init_listener(Namelist, Element+1)
	end.
