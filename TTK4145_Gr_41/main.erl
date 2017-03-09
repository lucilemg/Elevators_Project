-module(main).

-export([main/0]).
-include("records.hrl").


main() ->


	network:init_conn(?ConnectionList,1),
	network:init_listener(?ElevatorNameList,1),

	%SCHEDULER_PID = scheduler:start(),
	SCHEDULER_MANAGER_PID = spawn( fun() -> scheduler_manager_init() end),
	LISTENER_PID = spawn (fun() -> driver_manager(SCHEDULER_MANAGER_PID) end),

	elev_driver:start(LISTENER_PID,elevator),

	register(?FSM_PID, spawn(fun() -> fsm:start(SCHEDULER_MANAGER_PID) end)),

	% Move initialization to scheduler module so export is not required?
	ElevStatus = #elevatorStatus{direction = -1, lastFloor = -1, state = init, fsm_PID = ?FSM_PID},
	register(?STATUSLIST_HANDLER_PID, spawn (fun() -> scheduler:statuslist_handler([ElevStatus]) end)),

	register(?ORDERLIST_HANDLER_PID, spawn (fun() -> scheduler:orderlist_handler([]) end)),

	spawn (fun() -> button_light_manager([]) end).


scheduler_manager_init() ->
	receive
		{floor_reached, Floor} ->
			io:format("First floor reached!~n"),
			?FSM_PID ! {floor_reached},
			%InitStatus = elevatorStatus#{direction = 0, lastFloor = Floor, state = idle, fsm_PID = ?FSM_PID},
			?STATUSLIST_HANDLER_PID ! {update_direction, 0, ?FSM_PID},
			?STATUSLIST_HANDLER_PID ! {update_floor, Floor, ?FSM_PID}
	end,
	scheduler_manager().


scheduler_manager() ->
	receive
		{floor_reached, Floor} ->
			% Check if the local elevator (FSM) should stop
			?STATUSLIST_HANDLER_PID ! {update_floor, Floor, ?FSM_PID},

			io:format("Checking if elevator should stop~n"),

			Result = scheduler:receive_action(?FSM_PID),
			case Result of
				open_doors ->
					?FSM_PID ! {destination_floor_reached},
					?ORDERLIST_HANDLER_PID	! {remove_order, ?FSM_PID},
					?STATUSLIST_HANDLER_PID ! {update_state, open_doors, ?FSM_PID};
				no_orders_available ->
					io:format("We are running when there are no more orders, lets stop.~n"),
					?FSM_PID ! {destination_floor_reached},
					?STATUSLIST_HANDLER_PID ! {update_state, open_doors, ?FSM_PID};
				_ ->
					ok
			end;
			
		{awaiting_orders} ->
			%io:format("Looking for action for idle elevator~n"),
			?STATUSLIST_HANDLER_PID ! {update_state, idle, ?FSM_PID},

			NextAction = scheduler:receive_action(?FSM_PID),
			?FSM_PID ! {execute_action, NextAction},
			case NextAction of 
				move_up ->
					?STATUSLIST_HANDLER_PID ! {update_direction, up, ?FSM_PID},
					?STATUSLIST_HANDLER_PID ! {update_state, running, ?FSM_PID};
				move_down ->
					?STATUSLIST_HANDLER_PID ! {update_direction, down, ?FSM_PID},
					?STATUSLIST_HANDLER_PID ! {update_state, running, ?FSM_PID};
				open_doors ->
					?ORDERLIST_HANDLER_PID ! {remove_order, ?FSM_PID},
					?STATUSLIST_HANDLER_PID ! {update_direction, 0, ?FSM_PID},
					?STATUSLIST_HANDLER_PID ! {update_state, doors_open, ?FSM_PID};
				no_orders_available ->
					ok
			end
	end,
	scheduler_manager().


driver_manager(SCHEDULER_MANAGER_PID) ->
	SCHEDULER_MANAGER_PID ! {hello, ok},

	receive 
		{floor_reached, Floor} ->
			elev_driver:set_floor_indicator(Floor),
			io:format("Passing floor: ~p~n",[Floor]),
			SCHEDULER_MANAGER_PID ! {floor_reached, Floor};

		{new_order, Direction, Floor} ->

			io:format("New order received ~n"),
			case Direction of 
				command ->
					NewOrder = #orders{floor=Floor,direction=Direction, elevatorPID = ?FSM_PID};
				_ ->
					NewOrder = #orders{floor=Floor,direction=Direction}
			end,
			?ORDERLIST_HANDLER_PID ! {add_order, NewOrder}

	end,
	driver_manager(SCHEDULER_MANAGER_PID).


button_light_manager(OldOrders) ->

	Orders = scheduler:get_all_orders(),

	lists:foreach(fun(N) ->
		case lists:member(N, Orders) of
				true -> ok;
				false -> elev_driver:set_button_lamp(N#orders.floor, N#orders.direction, off)
		end
	end, OldOrders),
	
	lists:foreach(fun(N) ->
		case lists:member(N, OldOrders) of
				true -> ok;
				false -> elev_driver:set_button_lamp(N#orders.floor, N#orders.direction, on)
		end
	end, Orders),

	timer:sleep(300),
	button_light_manager(Orders).




%network_manager() ->
%	ok.
	%Receives the statuses of the other elevators and calls statuslist_handler
	%Receives orders from the other elevators and calls add orders
	% also acknoledgments that the other elevators achieved their task ? then call remove orders
	