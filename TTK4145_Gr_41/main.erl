-module(main).

-export([main/0]).
-include("records.hrl").


main() ->

	Namelist = ['elev1','elev2','elev3','elev4'],
	ConnectionList = ['elev1@127.0.0.1','elev2@127.0.0.1','elev3@127.0.0.1','elev4@127.0.0.1'],

	%network:init_conn(ConnectionList,1),
	%network:init_listener(Namelist,1),

	SCHEDULER_PID = scheduler:start(),
	SCHEDULER_MANAGER_PID = spawn( fun() -> scheduler_manager_init() end),
	LISTENER_PID = spawn (fun() -> driver_manager(SCHEDULER_MANAGER_PID) end),

	elev_driver:start(LISTENER_PID,elevator),

	register(?FSM_PID, spawn(fun() -> fsm:start(SCHEDULER_MANAGER_PID) end)),

	% Initialized in scheduler module so export is not required?
	ElevStatus = #elevatorStatus{direction = -1, lastFloor = -1, state = init, fsm_PID = ?FSM_PID},
	register(?STATUSLIST_HANDLER_PID, spawn (fun() -> scheduler:statuslist_handler([ElevStatus]) end)),

	TestOrder = #orders{direction = command, floor = 2},
	register(?ORDERLIST_HANDLER_PID, spawn (fun() -> scheduler:orderlist_handler([TestOrder]) end)).
	%register(?FSM_PID, fsm:start(SCHEDULER_MANAGER_PID)).
	%register(?FSM_PID,self()).


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
			io:format("Checking if elevator should stop~n"),
			OrdersList = scheduler:get_sorted_orders(?FSM_PID),
			Result = destination_or_not(lists:nth(1,OrdersList), Floor),
			io:format("Result is: ~p~n",[Result]),
			if
				Result == destination -> 
					io:format("We wanna stop here for sure!~n"),
					?FSM_PID ! {destination_floor_reached},
					?ORDERLIST_HANDLER_PID	! {remove_order, Floor, ?FSM_PID},
					?STATUSLIST_HANDLER_PID ! {update_state, open_doors, ?FSM_PID};
				true ->
					ok
			end,
			io:format("herereer~n"),
			?STATUSLIST_HANDLER_PID ! {update_floor, Floor, ?FSM_PID};
			%scheduler:update_states(asdasd)
		{awaiting_orders} ->
			io:format("Looking for action for idle elevator~n"),
			?STATUSLIST_HANDLER_PID ! {update_state, idle, ?FSM_PID},
			NextAction = scheduler:receive_action(?FSM_PID),
			?FSM_PID ! {execute_action, NextAction}
	end,
	scheduler_manager().


driver_manager(SCHEDULER_MANAGER_PID) ->
	SCHEDULER_MANAGER_PID ! {hello, ok},

	%io:format("Current orders: ~p~n",[CurrentOrders]),
	receive 
		{floor_reached, Floor} ->
			io:format("Passing floor: ~p~n",[Floor]),
			SCHEDULER_MANAGER_PID ! {floor_reached, Floor};

		{new_order, Direction, Floor} ->
			io:format("New order received ~n"),
			NewOrder = #orders{floor=Floor,direction=Direction},
			?ORDERLIST_HANDLER_PID ! {add_order, NewOrder}

	end,
	driver_manager(SCHEDULER_MANAGER_PID).


network_manager() ->
	ok.
	%Receives the statuses of the other elevators and calls statuslist_handler
	%Receives orders from the other elevators and calls add orders
	% also acknoledgments that the other elevators achieved their task ? then call remove orders
	

destination_or_not(NextOrder, ReachedFloor) ->
	io:format("Reached floor is ~p, while order is at ~p~n",[ReachedFloor,NextOrder#orders.floor]),

	try if
		ReachedFloor == NextOrder#orders.floor ->
			throw(destination);
		true ->
			throw(continue)
	end

	catch
		throw:destination ->
			destination;
		throw:continue ->
			continue
	end.