-module(network).
-export([init_connections/1,broadcast/1]).

-include("records.hrl").


init_connections(ElevID) ->
	
	Name = lists:nth(ElevID, ?ElevatorNameList),
	IP 	 = lists:nth(ElevID, ?IPList),

	ConnectionName = atom_to_list(Name) ++ "@" ++ atom_to_list(IP),

	net_kernel:start([list_to_atom(ConnectionName)]),
	erlang:set_cookie(node(),gateau),

	% Enables monitor to detect node connections and disconnections
	net_kernel:set_net_ticktime(4,0),
	net_kernel:monitor_nodes(true),

	% Attempts to establish connection to network, returns list of connections
	Connections = net_adm:world_list(?IPList,verbose),

	% Spawns and registers a listener for network messages
	init_listener(Name,ElevID),

	spawn (fun() -> connection_loop(Connections, ElevID) end).


init_listener(Name, ElevID) ->
	timer:sleep(500),
	io:format("Registered names: ~p~n", [global:registered_names()]),

	case global:whereis_name(Name) of
		undefined ->
			case global:register_name(Name,spawn (fun() -> receive_network_messages(ElevID) end)) of
				yes -> io:format("Successful registration ~n");
				no  -> io:format("Tried to register a process that seems to already be registered~n")
			end;
		_ ->
			io:format("Name taken, unable to register on network~n")
	end.


connection_loop(OldConnections, ElevID) ->

	net_adm:world_list(?IPList,verbose),

	% io:format("OldConnections: ~p~n",[OldConnections]),
	% io:format("Connections: ~p~n",[Connections]),


	% lists:foreach(fun(Node) ->
	% 	%io:format("~p removed?~n",[N]),
	% 	case lists:member(Node, Connections) of
	% 			true -> io:format("~p is member of ~p~n",[Node,Connections]);
	% 			false -> 
	% 				io:format("NODE ~p IS DISCONNECTED~n",[Node]),
	% 				?NETWORK_MONITOR_PID ! {nodedown, Node}
	% 	end 
	% end, OldConnections),

	% lists:foreach(fun(Node) ->
	% 	%io:format("~p removed?~n",[N]),
	% 	case lists:member(Node, OldConnections) of
	% 			true -> ok;
	% 			false -> 
	% 				io:format("NODE ~p IS NEWLY CONNECTED~n",[Node]),
	% 				NodeName = lists:sublist(atom_to_list(Node),5),
	% 				OwnName = lists:nth(ElevID, ?ElevatorNameList),
	% 				global:whereis_name(list_to_atom(NodeName)) ! {force_update_orderlist, atom_to_list(OwnName)};

	% 				%?NETWORK_MONITOR_PID ! {nodeup, Node}
	% 			_ -> io:format("?ñ~n")
	% 	end 
	% end, Connections),

	timer:sleep(5000),
	connection_loop(OldConnections, ElevID).


receive_network_messages(OwnElevID) ->

	receive

		{add_order, Order} ->
			io:format("add_order_network~n"),
			?ORDERLIST_HANDLER_PID ! {add_order, Order, ?NETWORK};

		{remove_order, Floor, ElevID} ->
			io:format("Received remove_order on network~n"),
			?ORDERLIST_HANDLER_PID ! {remove_order, Floor, ElevID, ?NETWORK};

		{reassign_order, Order} ->
			io:format("Received update_order on network~n"),
			?ORDERLIST_HANDLER_PID ! {reassign_order, Order, ?NETWORK};

		{increment_waiting_time} ->
			io:format("Received increment_waiting_time on network ~n"),
			?ORDERLIST_HANDLER_PID ! {increment_waiting_time};



		{remove_assignments, ElevID} ->
			io:format("removing assignemntesjntesjk~n"),
			?ORDERLIST_HANDLER_PID ! {remove_assignments, ElevID, ?NETWORK};

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


		{give_order_costs, CallerPID} ->
			Orders = scheduler:get_all_orders(),
			Statuses = scheduler:get_statuses(),
			Status = lists:keyfind(OwnElevID,5,Statuses),
			CalculatedOrders = scheduler:cost_function_loop(Orders, Status, length(Orders)),
			CallerPID ! {order_cost_list, CalculatedOrders};




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



broadcast(Message) ->

	RecipientsIPs = nodes(),
	io:format("Nodes: ~p~n",[RecipientsIPs]),

	lists:foreach(fun(Node) -> 

			NodeName = lists:sublist(atom_to_list(Node),5),
			io:format("Nodename: ~p~n",[NodeName]),
			(global:whereis_name(list_to_atom(NodeName))) ! Message
			end, RecipientsIPs).