-module(main).

-export([main/0]).


main() ->

	Namelist = ['elev1','elev2','elev3','elev4'],
	ConnectionList = ['elev1@127.0.0.1','elev2@127.0.0.1','elev3@127.0.0.1','elev4@127.0.0.1'],

	network:init_conn(ConnectionList,1),
	network:init_listener(Namelist,1),
	
	LISTENER_PID = spawn (fun() -> general_listener([],[]) end),

	SCHEDULER_PID = scheduler:start(),
	FSM_PID = fsm:start(SCHEDULER_PID),
	elev_driver:start(SCHEDULER_PID,elevator).


general_listener(CurrentOrders, CurrentStates) ->
	% move to main?
	receive 
		{floor_reached, Floor} ->
			%NewStates = CurrentStates,
			ok,

		{new_order, Direction, Floor} -> 
			NewOrder = #orders{floor=Floor,direction=Direction},
			NewOrders = CurrentOrders ++ [NewOrder],
			io:format("Current orders: ~p~n",[NewOrders]),

		{orders_wanted, SCHEDULER_PID} ->
			cost_function(CurrentOrders, CurrentStates, SCHEDULER_PID),
		{update_state, Dir, State, PID} ->
			ok
	end.
	