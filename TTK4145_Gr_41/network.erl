-module(network).
-export([init_conn/1,send_message/2,init_listener/2,receive_network_messages/0,broadcast/1]).

-include("records.hrl").


init_conn(Num) ->
	
	Name = lists:nth(Num,?ElevatorNameList),
	Ip 	 = lists:nth(Num, ?ConnectionList),

	ConnectionName = atom_to_list(Name) ++ "@" ++ atom_to_list(Ip),

	net_kernel:start([list_to_atom(ConnectionName)]),
	erlang:set_cookie(node(),asd),

	net_adm:world_list(?ConnectionList,verbose),

	net_kernel:monitor_nodes(true).


receive_network_messages() ->

	receive

		{add_order, Order} ->
			?ORDERLIST_HANDLER_PID ! {add_order, Order, ?NETWORK};

		{remove_order, FSM_PID} ->
			?ORDERLIST_HANDLER_PID ! {remove_order, FSM_PID, ?NETWORK};

		{update_order, Order} ->
			?ORDERLIST_HANDLER_PID ! {update_order, Order, ?NETWORK};


		{update_direction, Direction, ElevPID} ->
			?STATUSLIST_HANDLER_PID ! {update_direction, Direction, ElevPID, ?NETWORK};

		{update_floor, Floor, ElevPID} ->
			?STATUSLIST_HANDLER_PID ! {update_floor, Floor, ElevPID, ?NETWORK};

		{update_state, State, ElevPID} ->
			?STATUSLIST_HANDLER_PID ! {update_state, State, ElevPID, ?NETWORK};



		{get_all_orders, CallerPID} ->
			Orders = scheduler:get_all_orders(),
			CallerPID ! {all_orders, Orders};


		{get_statuses, CallerPID} ->
			io:format("tring"),
			Statuses = scheduler:get_statuses(),
			io:format("asd"),
			CallerPID ! {all_statuses, Statuses};

		_ ->
			io:format("Something somtheing somthein~n")

	end,
	receive_network_messages().



send_message(PID, Message) ->
	PID ! Message.

broadcast(Message) ->
	AllRegisteredNames = global:registered_names(),
	lists:foreach(fun(N) -> 
			send_message(global:whereis_name(N), Message)
			end, AllRegisteredNames).


init_listener(Namelist, Element) ->
	timer:sleep(1000),
	io:format("Testing with PID name: ~p ~n",[lists:nth(Element,Namelist)]),
	io:format("Registered names: ~p~n", [global:registered_names()]),
	case global:whereis_name(lists:nth(Element,Namelist)) of
		undefined ->
			case global:register_name(lists:nth(Element,Namelist),spawn(network,receive_network_messages,[])) of
				yes -> io:format("Successful registration ~n");
				no -> io:format("Tried to register a process that seems to already be registered~n")
			end;
		_ ->
			io:format("Name taken, attempting with different name~n"),
			init_listener(Namelist, Element+1)
	end.
