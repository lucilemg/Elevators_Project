-module(scheduler).

-export([start/0]).

-record (orders, {direction, floor, elevatorPID}).
-record (elevatorStates, {direction, lastFloor, state, elevatorPID}).


start() ->
	SCHED_LISTENER_PID = spawn (fun() -> scheduler_listener([],[]) end),
	spawn(fun() -> scheduler(SCHED_LISTENER_PID) end).


scheduler_listener(CurrentOrders, CurrentStates) ->
	% move to main?
	receive 
		{floor_reached, Floor} ->
			NewStates = CurrentStates

		{new_order, Direction, Floor} -> 
			NewOrder = #orders{floor=Floor,direction=Direction},
			NewOrders = CurrentOrders ++ [NewOrder],
			io:format("Current orders: ~p~n",[NewOrders]),
		{orders_wanted, SCHEDULER_PID} ->
			cost_function(CurrentOrders, CurrentStates, SCHEDULER_PID),
		{update_state, Dir, State, PID} ->

	end.
	

cost_function(Orders, ElevatorStates, SCHEDULER_PID) ->
	% Sort list as cost function of time waited per order or something
	SCHEDULER_PID ! {sorted_orders, Orders}.


scheduler(SCHED_LISTENER_PID) ->
	receive
		{asd, Dir, Floor} ->
			SCHED_LISTENER_PID ! {new_order, Dir, Floor},
			scheduler(SCHED_LISTENER_PID);
		{awaiting_orders, FSM_PID} ->
			SCHED_LISTENER_PID ! {update_state, ,Idle,FSM_PID}
			SCHED_LISTENER_PID ! {orders_wanted, self()},
			receive
				{sorted_orders, SortedOrders} ->
					lists:foreach(fun(N) -> 
						case FSM_PID == N#orders.elevatorPID of
							true ->
								FSM_PID ! {N#orders.direction};
							false ->
								ok
						end,
						end, SortedOrders).
					io:format("Orders received: ~p~n",[SortedOrders])

			end
	end.