-module(main).

-export([main/0]).
-include("records.hrl").

main() ->

	Namelist = ['elev1','elev2','elev3','elev4'],
	ConnectionList = ['elev1@127.0.0.1','elev2@127.0.0.1','elev3@127.0.0.1','elev4@127.0.0.1'],

	%network:init_conn(ConnectionList,1),
	%network:init_listener(Namelist,1),
	
	SCHEDULER_PID = scheduler:start(),
	FSM_PID = fsm:start(SCHEDULER_PID),

	LISTENER_PID = spawn (fun() -> general_listener([],[],[FSM_PID]) end),

	elev_driver:start(LISTENER_PID,elevator).


general_listener(CurrentOrders, CurrentStates, FSM_PID) ->
	receive 
		{floor_reached, Floor} ->
			io:format("Passing floor: ~p~n",[Floor]),
			FSM_PID ! floor_reached,
			%NewStates = CurrentStates,

			ok;

		{new_order, Direction, Floor} -> 
			NewOrder = #orders{floor=Floor,direction=Direction},
			NewOrders = CurrentOrders ++ [NewOrder],
			io:format("Current orders: ~p~n",[NewOrders]);

		{orders_wanted, SCHEDULER_PID} ->
			%cost_function(CurrentOrders, CurrentStates, SCHEDULER_PID);
			ok;
		{update_state, Dir, State, PID} ->
			ok
	end,
	general_listener(CurrentOrders, CurrentStates,FSM_PID).
