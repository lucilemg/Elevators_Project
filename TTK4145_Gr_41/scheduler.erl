-module(scheduler).

-export([start/0]).
-include("records.hrl").


start() ->
	%SCHED_LISTENER_PID = spawn (fun() -> scheduler_listener([],[]) end),
	spawn(fun() -> scheduler(SCHED_LISTENER_PID) end).



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