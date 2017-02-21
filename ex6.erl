-module(ex6).
-export([main/1,send_message/2,backupLoop/1,broadcast/1, count/1]).


main(CurrentCount) ->
	global:register_name('primary',self()),
	global:register_name('backup',spawn(main,backupLoop,[0])),
	count(CurrentCount).

send_message(PID, Message) ->
	PID ! {msg, Message}.

broadcast(Message) ->
	AllRegisteredNames = global:registered_names(),
	lists:foreach(fun(N) -> 
			NthPid = global:whereis_name(N),
			send_message(NthPid, Message)
			end, AllRegisteredNames).

count(Integer) ->
	case 200 == Integer of
		true -> io:format("Process is dead~n"),
			exit(self(),kill);
		false ->
			timer:sleep(1000),
			io:format("Count: ~p~n",[Integer]),
			broadcast(Integer),
			count(Integer + 1)
	end.

backupLoop(BackupCount) ->
	receive
		{msg, Msg} -> io:format("Received message: ~p~n",[Msg]),
					backupLoop(Msg)
	after 3000 -> 
		io:format("Primary is dead~n"),
		global:unregister_name('backup'),
		spawn(main,main,[BackupCount+1]),
		exit(self(),kill)
	end.
