-module(network).
-export([init_conn/1,broadcast/1]).

-include("records.hrl").


init_conn(ElevID) ->
	
	Name = lists:nth(ElevID,?ElevatorNameList),
	Ip 	 = lists:nth(ElevID, ?ConnectionList),

	ConnectionName = atom_to_list(Name) ++ "@" ++ atom_to_list(Ip),

	net_kernel:start([list_to_atom(ConnectionName)]),
	erlang:set_cookie(node(),asd),
	net_kernel:set_net_ticktime(4),

	net_kernel:monitor_nodes(true),

	Connections = net_adm:world_list(?ConnectionList,verbose),

	spawn (fun() -> connection_loop(Connections, ElevID) end),

	init_listener(?ElevatorNameList,ElevID).


connection_loop(OldConnections, ElevID) ->

	Connections = net_adm:world_list(?ConnectionList,verbose),

	io:format("OldConnections: ~p~n",[OldConnections]),
	io:format("Connections: ~p~n",[Connections]),


	lists:foreach(fun(Node) ->
		%io:format("~p removed?~n",[N]),
		case lists:member(Node, Connections) of
				true -> io:format("~p is member of ~p~n",[Node,Connections]);
				false -> 
					io:format("NODE ~p IS DISCONNECTED~n",[Node]),
					?NETWORK_MONITOR_PID ! {nodedown, Node}
		end 
	end, OldConnections),

	lists:foreach(fun(Node) ->
		%io:format("~p removed?~n",[N]),
		case lists:member(Node, OldConnections) of
				true -> ok;
				false -> 
					io:format("NODE ~p IS NEWLY CONNECTED~n",[Node]),
					NodeName = lists:sublist(atom_to_list(Node),5),
					OwnName = lists:nth(ElevID, ?ElevatorNameList),
					global:whereis_name(list_to_atom(NodeName)) ! {force_update_orderlist, atom_to_list(OwnName)};

					%?NETWORK_MONITOR_PID ! {nodeup, Node}
				_ -> io:format("?ñ~n")
		end 
	end, Connections),



	timer:sleep(5000),
	connection_loop(Connections, ElevID).



receive_network_messages(OwnElevID) ->

	receive

		{add_order, Order} ->
			io:format("add_order_networkñ~n"),
			?ORDERLIST_HANDLER_PID ! {add_order, Order, ?NETWORK};

		{remove_order, Floor, ElevID} ->
			io:format("NRORNORNORNRNONRO~n"),
			?ORDERLIST_HANDLER_PID ! {remove_order, Floor, ElevID, ?NETWORK};

		{update_order, Order} ->
			io:format("NUONUOUNUNUO~n"),
			?ORDERLIST_HANDLER_PID ! {update_order, Order, ?NETWORK};


		% {update_direction, Direction, ElevID} ->
		% 	io:format("NUNDDNDUNDUUDU~n"),
		% 	?STATUSLIST_HANDLER_PID ! {update_direction, Direction, ElevID, ?NETWORK};

		% {update_floor, Floor, ElevID} ->
		% 	io:format("NUNUNUNFUNFUFN~n"),
		% 	?STATUSLIST_HANDLER_PID ! {update_floor, Floor, ElevID, ?NETWORK};

		% {update_state, State, ElevID} ->
		% 	io:format("NUNUSNSUNUSNUS~n"),
		% 	?STATUSLIST_HANDLER_PID ! {update_state, State, ElevID, ?NETWORK};



		{get_all_orders, CallerPID} ->
			io:format("GAOGAOGOAGOO~n"),
			Orders = scheduler:get_all_orders(),
			CallerPID ! {all_orders, Orders};


		% {get_statuses, CallerPID} ->
		% 	io:format("tring~n"),
		% 	Statuses = scheduler:get_statuses(),
		% 	CallerPID ! {all_statuses, Statuses};


		{give_id, CallerPID} ->
			io:format("Giving ElevID as requested~n"),
			CallerPID ! {elev_id, OwnElevID};


		{force_update_orderlist, NodeName} ->
			main:update_orderlist_and_statuslist(NodeName);


		Something ->
			io:format("Something: ~p~n",[Something])

	end,
	receive_network_messages(OwnElevID).


send_message(PID, Message) ->
	PID ! Message.

broadcast(Message) ->
	AllRegisteredNames = global:registered_names(),
	lists:foreach(fun(N) -> 
			send_message(global:whereis_name(N), Message)
			end, AllRegisteredNames).


init_listener(NameList, ElevID) ->
	timer:sleep(500),
	io:format("Testing with PID name: ~p ~n",[lists:nth(ElevID,NameList)]),
	io:format("Registered names: ~p~n", [global:registered_names()]),
	case global:whereis_name(lists:nth(ElevID,NameList)) of
		undefined ->
			case global:register_name(lists:nth(ElevID,NameList),spawn (fun() -> receive_network_messages(ElevID) end)) of
				yes -> io:format("Successful registration ~n");
				no -> io:format("Tried to register a process that seems to already be registered~n")
			end;
		_ ->
			io:format("Name taken, unable to register on network~n")
	end.
