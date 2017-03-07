-module(main).

-export([main/0, down/0]).
-include("records.hrl").

main() ->

	Namelist = ['elev1','elev2','elev3','elev4'],
	ConnectionList = ['elev1@127.0.0.1','elev2@127.0.0.1','elev3@127.0.0.1','elev4@127.0.0.1'],

	%network:init_conn(ConnectionList,1),
	%network:init_listener(Namelist,1),
	
	SCHEDULER_PID = scheduler:start(),
	FSM_PID = fsm:start(SCHEDULER_PID),

	LISTENER_PID = spawn (fun() -> driver_manager([],[],[FSM_PID]) end),

	elev_driver:start(LISTENER_PID,elevator).

down() ->
	elev_driver:set_motor_direction(down).

scheduler_manager() ->
	receive
		{floor_reached, Floor} ->
			% Check if the local elevator (FSM) should stop
			OrdersList = scheduler:get_orders(FSM_PID),
			Result = destination_or_not(lists:nth(1,OrdersList), Floor, FSM_PID),
			if
				Result == destination -> 
					FSM_PID ! {destination_floor_reached};
				_ ->
					ok
			end
			%scheudler:update_states(aosi)

	end.

%fsm_manager() ->
%	receive
%		{}


driver_manager(CurrentOrders, CurrentStates, SCHEDULER_PID) ->

	%io:format("Current orders: ~p~n",[CurrentOrders]),
	receive 
		{floor_reached, Floor} ->
			io:format("Passing floor: ~p~n",[Floor]),
			SCHEDULER_PID ! {floor_reached, Floor};

		{new_order, Direction, Floor} ->
			io:format("New order received ~n"), 
			NewOrder = #orders{floor=Floor,direction=Direction},
			NewOrders = CurrentOrders ++ [NewOrder],
			io:format("Current orders: ~p~n",[NewOrders]),
			general_listener(NewOrders, CurrentStates, FSM_PID)
			% fix this call, catch - throw?

	end,
	general_listener(CurrentOrders, CurrentStates,FSM_PID).

	
stop_or_not(NextOrder, ReachedFloor, FSM_PID) ->

	try if
		ReachedFloor == NextOrder#orders.floor ->
			throw(destination);
		_ ->
			throw(continue)

	end,

	catch
		throw:destination ->
			destination;
		throw:continue ->
			continue
	end.