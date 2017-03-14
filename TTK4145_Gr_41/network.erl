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
	%net_adm:world_list(?IPList,verbose),

	% Spawns and registers a listener for network messages
	init_listener(Name,ElevID),

	spawn(fun() -> connection_loop() end),

	network_monitor([]).


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
	timer:sleep(100),
	io:format("Checking connections...~n"),
	net_adm:world_list(?IPList),
	timer:sleep(5000),
	connection_loop().



% PairList contains pairs of IP for a Node with its corresponding ElevID
network_monitor(PairList) ->	

	receive 

		{nodeup, Node} -> 
			io:format("~p is up~n",[Node]),

			timer:sleep(5000),
			NodeName = lists:sublist(atom_to_list(Node),5),
			io:format("regd names: ~p~n",[global:registered_names()]),
			(global:whereis_name(list_to_atom(NodeName))) ! {give_id, self()},
			io:format("Waitinf in nodeup~n"),
			receive 
				{elev_id, NodeElevID} -> NewPair = {Node, NodeElevID}
			end,
			NewPairList = PairList ++ [NewPair],
			main:update_orderlist_and_statuslist(NodeName);


		{nodedown, Node} -> 
			io:format("~p is down~n",[Node]),

			PairTuple = lists:keyfind(Node,1,PairList),
			{_,ElevID} = PairTuple,
			?ORDERLIST_HANDLER_PID ! {remove_assignments, ElevID},
			NewPairList = lists:delete({Node,ElevID},PairList);


		A -> io:format("got thomesing elsek;::::: ~p~n",[A]),
			NewPairList = PairList

	end,
	io:format("Currently connected nodes, with nodes(), are: ~p~n",[nodes()]),
	network_monitor(NewPairList).


receive_network_messages(OwnElevID) ->
	io:format("looking for network messages~n"),
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
			io:format("give_id received~n"),
			CallerPID ! {elev_id, OwnElevID};


		Something ->
			io:format("Something: ~p~n",[Something])


	end,
	receive_network_messages(OwnElevID).



broadcast(Message) ->

	RecipientsIPs = nodes(),

	lists:foreach(fun(Node) -> 

			NodeName = lists:sublist(atom_to_list(Node),5),
			io:format("Broadcast: NodeName = ~p~n",[NodeName]),
			(global:whereis_name(list_to_atom(NodeName))) ! Message
			end, RecipientsIPs).