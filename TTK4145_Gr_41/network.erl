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
	net_adm:world_list(?IPList,verbose),

	% Spawns and registers a listener for network messages
	init_listener(Name,ElevID),

	connection_loop().


init_listener(Name, ElevID) ->
	timer:sleep(500),
	io:format("Registered names: ~p~n", [global:registered_names()]),

	case global:whereis_name(Name) of
		undefined ->
			case global:register_name(Name,spawn_link (fun() -> receive_network_messages(ElevID) end)) of
				yes -> io:format("Successful registration ~n");
				no  -> io:format("Tried to register a process that seems to already be registered~n")
			end;
		_ ->
			io:format("Name taken, unable to register on network~n")
	end.


connection_loop() ->

	net_adm:world_list(?IPList,verbose),
	timer:sleep(5000),
	connection_loop().


receive_network_messages(OwnElevID) ->

	receive

		{add_order, Order} ->
			?ORDERLIST_HANDLER_PID ! {add_order, Order, ?NETWORK};


		{remove_order, Floor, ElevID} ->
			?ORDERLIST_HANDLER_PID ! {remove_order, Floor, ElevID, ?NETWORK};


		{reassign_order, Order} ->
			?ORDERLIST_HANDLER_PID ! {reassign_order, Order, ?NETWORK};


		{increment_waiting_time} ->
			?ORDERLIST_HANDLER_PID ! {increment_waiting_time};


		{remove_assignments, ElevID} ->
			?ORDERLIST_HANDLER_PID ! {remove_assignments, ElevID, ?NETWORK};


		{get_all_orders, CallerPID} ->
			Orders = scheduler:get_all_orders(),
			CallerPID ! {all_orders, Orders};


		{give_id, CallerPID} ->
			CallerPID ! {elev_id, OwnElevID};


		Something ->
			io:format("Something: ~p~n",[Something])


	end,
	receive_network_messages(OwnElevID).



broadcast(Message) ->

	RecipientsIPs = nodes(),

	lists:foreach(fun(Node) -> 

			NodeName = lists:sublist(atom_to_list(Node),5),
			(global:whereis_name(list_to_atom(NodeName))) ! Message
			end, RecipientsIPs).