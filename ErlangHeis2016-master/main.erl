-module(main).

-export([main/1,update_orderlist_and_statuslist/1]).
-include("records.hrl").


main(ElevID) ->

	network:init_connections(ElevID),
	
	SCHEDULER_MANAGER_PID = spawn (fun() -> scheduler_manager_init(ElevID) end),
	DRIVER_MANAGER_PID	  =	spawn (fun() -> driver_manager(SCHEDULER_MANAGER_PID,ElevID) end),


	elev_driver:start(DRIVER_MANAGER_PID,elevator),
	scheduler:init_listhandlers(ElevID),

	spawn (fun() -> button_light_manager([],ElevID) end),

	register(?FSM_PID, spawn(fun() -> fsm:start(SCHEDULER_MANAGER_PID) end)),

	io:format("Currently connected nodes, with nodes(), are: ~p~n",[nodes()]),

	%update_orderlist_and_statuslist(ElevID),

	register (?NETWORK_MONITOR_PID, monitor_listener([])).


update_orderlist_and_statuslist(NodeName) ->
	try case global:whereis_name(list_to_atom(NodeName)) of
				undefined -> 
					%PID = undefined,
					throw(node_not_found);
				PID -> throw(PID)
	end
	catch
		throw:node_not_found -> io:format("ERROR: NodeName was not found~n");

		throw:Something ->
			Something ! {get_all_orders, self()},
			receive 
				{all_orders, Orders} ->
					?ORDERLIST_HANDLER_PID ! {add_all_orders, Orders}
			end,
			%Something ! {get_statuses, self()},
			%receive
			%	{all_statuses, Statuses} ->
			%		?STATUSLIST_HANDLER_PID ! {update_all_statuses, Statuses}
			%end,
			io:format("UPDATED LISTS~n")
	end.

% PairList contains pairs of IP for a Node with its corresponding ElevID
monitor_listener(PairList) ->	
	receive 

		{nodeup, Node} -> 
			io:format("~p is up~n",[Node]),

			timer:sleep(5000),
			NodeName = lists:sublist(atom_to_list(Node),5),
			io:format("NodeName: ~p~n",[NodeName]),
			io:format("regd name: ~p~n",[global:registered_names()]),
			(global:whereis_name(list_to_atom(NodeName))) ! {give_id, self()},
			receive 
				{elev_id, ElevID} -> NewPair = {Node, ElevID}
			end,
			NewPairList = PairList ++ [NewPair],

			update_orderlist_and_statuslist(NodeName);

		{nodedown, Node} -> 
			io:format("~p is down~n",[Node]),

			PairTuple = lists:keyfind(Node,1,PairList),
			{_,ElevID} = PairTuple,
			?ORDERLIST_HANDLER_PID ! {remove_assignments, ElevID},
			NewPairList = lists:delete({Node,ElevID},PairList);


		_ ->	io:format("got thomesing elsek~n"),
			NewPairList = PairList

	end,
	io:format("Currently connected nodes, with nodes(), are: ~p~n",[nodes()]),
	io:format("PairList: ~p~n",[NewPairList]),
	monitor_listener(NewPairList).


scheduler_manager_init(ElevID) ->
	receive
		{floor_reached, Floor} ->
			io:format("First floor reached!~n"),
			?FSM_PID ! {stop_at_floor},
			?STATUSLIST_HANDLER_PID ! {update_direction, 0, ElevID, ?LOCAL},
			?STATUSLIST_HANDLER_PID ! {update_floor, Floor, ElevID, ?LOCAL}
	end,
	scheduler_manager(ElevID).


scheduler_manager(ElevID) ->
	receive
		{floor_reached, Floor} ->
			% Check if the local elevator (FSM) should stop
			?STATUSLIST_HANDLER_PID ! {update_floor, Floor, ElevID, ?LOCAL},

			io:format("Checking if elevator should stop~n"),

			NextAction = scheduler:receive_action(ElevID),
			case NextAction of

				open_doors ->
					?FSM_PID ! {stop_at_floor},
					?ORDERLIST_HANDLER_PID	! {remove_order, Floor, ElevID, ?LOCAL},
					?STATUSLIST_HANDLER_PID ! {update_state, open_doors, ElevID, ?LOCAL};
				no_orders_available ->
					io:format("We are running when there are no more orders, lets stop.~n"),
					?FSM_PID ! {stop_at_floor},
					?STATUSLIST_HANDLER_PID ! {update_state, open_doors, ElevID, ?LOCAL};
				move_up ->
					?FSM_PID ! {next_direction, up};
				move_down ->
					?FSM_PID ! {next_direction, down}

			end;

		{awaiting_orders} ->
			?STATUSLIST_HANDLER_PID ! {update_state, idle, ElevID, ?LOCAL},
			%io:format("looking for action for idle elevator~n"),
			NextAction = scheduler:receive_action(ElevID),
			?FSM_PID ! {execute_action, NextAction},
			Statuses = scheduler:get_statuses(),
			Floor = (lists:keyfind(ElevID,5,Statuses))#elevatorStatus.lastFloor,
			case NextAction of 
				move_up ->
					?STATUSLIST_HANDLER_PID ! {update_direction, up, ElevID, ?LOCAL},
					?STATUSLIST_HANDLER_PID ! {update_state, running, ElevID, ?LOCAL};
				move_down ->
					?STATUSLIST_HANDLER_PID ! {update_direction, down, ElevID, ?LOCAL},
					?STATUSLIST_HANDLER_PID ! {update_state, running, ElevID, ?LOCAL};
				open_doors ->
					?ORDERLIST_HANDLER_PID ! {remove_order, Floor, ElevID, ?LOCAL},
					?STATUSLIST_HANDLER_PID ! {update_direction, 0, ElevID, ?LOCAL},
					?STATUSLIST_HANDLER_PID ! {update_state, doors_open, ElevID, ?LOCAL};
				no_orders_available ->
					ok
			end;

		{running_timeout} ->
			?ORDERLIST_HANDLER_PID ! {remove_assignments, ElevID, ?LOCAL},
			io:format("sending retry~n"),
			?FSM_PID ! {retry}


	end,
	scheduler_manager(ElevID).


driver_manager(SCHEDULER_MANAGER_PID, ElevID) ->

	receive 
		{floor_reached, Floor} ->
			elev_driver:set_floor_indicator(Floor),
			io:format("Passing floor: ~p~n",[Floor]),
			?ORDERLIST_HANDLER_PID ! {increment_waiting_time, ?LOCAL},
			SCHEDULER_MANAGER_PID ! {floor_reached, Floor};

		{new_order, Direction, Floor} ->

			io:format("New order received ~n"),
			case Direction of 
				command ->
					NewOrder = #orders{floor=Floor,direction=Direction, assignedElevID = ElevID, waitingTime = 0};
				_ ->
					NewOrder = #orders{floor=Floor,direction=Direction, waitingTime = 0}
			end,
			?ORDERLIST_HANDLER_PID ! {add_order, NewOrder, ?LOCAL}

	end,
	driver_manager(SCHEDULER_MANAGER_PID, ElevID).


button_light_manager(OldOrders, ElevID) ->

	Orders = scheduler:get_all_orders(),

	%io:format("Old orders: ~p~n",[OldOrders]),
	%io:format("New orders: ~p~n",[Orders]),

	%io:format("~n### REMOVE CHECK ###~n~n"),
	lists:foreach(fun(N) ->
		%io:format("~p removed?~n",[N]),
		case is_order_removed(N, Orders, ElevID) of
				true -> elev_driver:set_button_lamp(N#orders.floor, N#orders.direction, off);
					%io:format("Found removed order: ~p, turning off the lights for it.~n",[N]);
				false -> ok;
				_ ->	io:format("???!?!?!?!?~n")
		end 
	end, OldOrders),
	
	%io:format("~n### LIGHT ON CHECK ###~n~n"),

	lists:foreach(fun(N) ->
		case lists:member(N, OldOrders) of
				true -> ok;
				false -> 
					case N#orders.direction of
						command -> 	if 
										N#orders.assignedElevID == ElevID -> elev_driver:set_button_lamp(N#orders.floor, N#orders.direction, on);
										true -> ok
									end;
						_ -> elev_driver:set_button_lamp(N#orders.floor, N#orders.direction, on)
					end
		end
	end, Orders),

	timer:sleep(300),
	button_light_manager(Orders, ElevID).


is_order_removed(Order, OrderList, ElevID) ->
	%io:format("OrderList is : ~p~n",[OrderList]),
	try case length(OrderList) of
		0 -> throw(order_not_found);

		_ ->
			lists:foreach (fun(N) ->

			case N#orders.floor == Order#orders.floor of
				true ->
					case (N#orders.direction == command) of
						true -> 
							case ((N#orders.assignedElevID == ElevID) and (Order#orders.direction == command)) of
								true -> throw(order_exists);
								false -> ok
							end;
						false ->
							case (N#orders.direction == Order#orders.direction) of
								true ->	throw(order_exists);
								false -> ok
							end
					end;

				_ ->
					ok
			end

			end, OrderList),
			throw(order_not_found)
	end
	catch
			throw:order_exists -> %io:format("order exists~n"),
								false;
			throw:order_not_found -> %io:format("order not found~n"),
									true
	end.	



