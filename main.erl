-module(main).

-export([init/1, update_orderlist_and_statuslist/1]).
-include("records.hrl").


init(ElevID) ->
	io:format("Starting!~n"),
	timer:sleep(1000),

	process_flag(trap_exit,true),
	spawn_link(network,init_connections,[ElevID]),

	scheduler:init_listhandlers(),

	timer:sleep(2000),

	SCHEDULER_MANAGER_PID = spawn_link (fun() -> scheduler_manager_init(ElevID) end),
	DRIVER_MANAGER_PID	  =	spawn_link (fun() -> driver_manager(SCHEDULER_MANAGER_PID,ElevID) end),


	elev_driver:start(DRIVER_MANAGER_PID, elevator),

	register(?FSM_PID, spawn_link(fun() -> fsm:start(SCHEDULER_MANAGER_PID) end)),
	
	spawn_link (fun() -> button_light_manager([],ElevID) end),

	%register (?NETWORK_MONITOR_PID, spawn_link(fun() -> network_monitor([],ElevID) end)),

	process_supervisor(ElevID).



process_supervisor(ElevID) ->
	
	receive

		{'EXIT', Pid, Reason} ->
				io:format("ERROR, ~p HAS CRASHED WITH REASON: ~p~n!",[Pid,Reason]),
				io:format("Restarting.~n"),	
				spawn(fun() -> init(ElevID) end),
				exit(self(),kill)

	end.



update_orderlist_and_statuslist(NodeName) ->
	try 
		case global:whereis_name(list_to_atom(NodeName)) of
				undefined -> throw(node_not_found);
				NodePID   -> throw(NodePID)
	end
	catch
		throw:node_not_found -> 
			io:format("ERROR: NodeName was not found~n");

		throw:PID ->
			PID ! {get_all_orders, self()},
			receive 
				{all_orders, Orders} ->
					?ORDERLIST_HANDLER_PID ! {add_all_orders, Orders},
					io:format("Got Orders: ~p~n",[Orders])
			end,
			io:format("UPDATED LISTS~n")
	end.



scheduler_manager_init(ElevID) ->

	receive
		{floor_reached, Floor} ->
			io:format("asdasidaioasjioasi~n"),
			?FSM_PID ! {stop_at_floor},
			?STATUSLIST_HANDLER_PID ! {update_floor, Floor},
			?STATUSLIST_HANDLER_PID ! {update_direction, 0}

	end,
	scheduler_manager(ElevID).


scheduler_manager(ElevID) ->

	receive

		{floor_reached, Floor} ->

			?STATUSLIST_HANDLER_PID ! {update_floor, Floor},

			NextAction = scheduler:receive_action(ElevID),
			case NextAction of
				move_up ->
					?FSM_PID ! {next_direction, up};
				move_down ->
					?FSM_PID ! {next_direction, down};
				open_doors ->
					?FSM_PID ! {stop_at_floor},
					?ORDERLIST_HANDLER_PID	! {remove_order, Floor, ElevID, ?LOCAL},
					?STATUSLIST_HANDLER_PID ! {update_state, open_doors};
				no_orders_available ->
					io:format("We are running when there are no more orders, lets stop.~n"),
					?FSM_PID ! {stop_at_floor},
					?STATUSLIST_HANDLER_PID ! {update_state, open_doors}
			end;


		{awaiting_orders} ->

			?STATUSLIST_HANDLER_PID ! {update_state, idle},
			NextAction = scheduler:receive_action(ElevID),
			?FSM_PID ! {execute_action, NextAction},
			case NextAction of 
				move_up ->
					?STATUSLIST_HANDLER_PID ! {update_direction,  up},
					?STATUSLIST_HANDLER_PID ! {update_state, running};
				move_down ->
					?STATUSLIST_HANDLER_PID ! {update_direction, down},
					?STATUSLIST_HANDLER_PID ! {update_state,  running};
				open_doors ->
					Status = scheduler:get_status(),
					Floor  = Status#elevatorStatus.lastFloor,
					?ORDERLIST_HANDLER_PID !  {remove_order, Floor, ElevID, ?LOCAL},
					?STATUSLIST_HANDLER_PID ! {update_direction, 0},
					?STATUSLIST_HANDLER_PID ! {update_state, doors_open};
				no_orders_available ->
					ok
			end;


		{running_timeout} ->
			?ORDERLIST_HANDLER_PID ! {remove_assignments, ElevID, ?LOCAL},
			?FSM_PID ! {retry}

	end,
	scheduler_manager(ElevID).


driver_manager(SCHEDULER_MANAGER_PID, ElevID) ->

	receive 

		{floor_reached, Floor} ->

			elev_driver:set_floor_indicator(Floor),
			io:format("Passing floor: ~p~n",[Floor]),
			SCHEDULER_MANAGER_PID  ! {floor_reached, Floor},
			?ORDERLIST_HANDLER_PID ! {increment_waiting_time, ?LOCAL};	


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

	lists:foreach(fun(N) ->
		case is_order_removed(N, Orders, ElevID) of
				true  -> elev_driver:set_button_lamp(N#orders.floor, N#orders.direction, off);
				false -> ok
		end 
	end, OldOrders),

	lists:foreach(fun(N) ->
		case is_order_new(N, OldOrders, ElevID) of
				true  -> elev_driver:set_button_lamp(N#orders.floor, N#orders.direction, on);
				false -> ok
					
		end
	end, Orders),

	timer:sleep(300),
	button_light_manager(Orders, ElevID).


is_order_removed(Order, OrderList, ElevID) ->
	try 
		case length(OrderList) of
		0 -> throw(order_not_found);
		_ ->
			lists:foreach (fun(N) ->

				case N#orders.floor == Order#orders.floor of
					true ->
						case (N#orders.direction == command) of
							true -> 
								case ((N#orders.assignedElevID == ElevID) and (Order#orders.direction == command)) of
									true  -> throw(order_exists);
									false -> ok
								end;
							false ->
								case (N#orders.direction == Order#orders.direction) of
									true  -> throw(order_exists);
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
		throw:order_exists    -> false;
		throw:order_not_found -> true
	end.	


is_order_new(Order, OldOrders, ElevID) -> 
	try
		case lists:member(Order, OldOrders) of
			true  -> throw(order_exists);
			false ->
					case Order#orders.direction of
						command -> 	if 
										Order#orders.assignedElevID == ElevID -> throw(order_is_new);
										true -> throw(order_exists)
									end;
						_ -> throw(order_is_new)
					end
		end
	catch
		throw:order_exists -> false;
		throw:order_is_new -> true
	end.