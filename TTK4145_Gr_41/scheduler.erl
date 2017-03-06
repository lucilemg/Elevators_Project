-module(scheduler).

-export([start/0]).
-include("records.hrl").


start() ->
	%SCHED_LISTENER_PID = spawn (fun() -> scheduler_listener([],[]) end),
	spawn(fun() -> scheduler() end).


%cost_function(Orders, ElevatorStates, Caller_PID) ->
	% Sort list as cost function of time waited per order or something
	%Caller_PID ! {sorted_orders, Orders}.

scheduler() ->
	io:format("Scheduler ready to receive~n"),
	receive
		{awaiting_orders, FSM_PID} ->
			io:format("Received awaiting_orders ~n"),
			%SCHED_LISTENER_PID ! {update_state, ,Idle,FSM_PID}
			%SCHED_LISTENER_PID ! {orders_wanted, self()},
			CurrentFloor = get_floor(FSM_PID),
			NewOrder = get_order(FSM_PID, CurrentFloor),
			FSM_PID ! {execute_order, NewOrder},
			io:format("Scheduler says - FSM, execute: ~p~n",[NewOrder]);
		_ ->
			io:format("received unknown ~n")
	end,
	scheduler().

get_order(FSM_PID, CurrentFloor) ->
	% rename to receive_action?
 	SortedOrders = get_sorted_orders(FSM_PID),
	%CurrentFloor = get_floor(FSM_PID),

	try lists:foreach(fun(N) -> 
		case FSM_PID == N#orders.elevatorPID of
			true ->
				if 
					CurrentFloor < N#orders.floor ->
						throw(order_up);
					CurrentFloor > N#orders.floor ->
						throw(order_down);
					CurrentFloor == N#orders.floor ->
						throw(open_doors)
				end;
			false ->
				ok
		end
		end, SortedOrders)

	catch
		throw:order_up ->
			up;

		throw:order_down ->
			down;

		throw:open_doors ->
			open
end.

get_floor(FSM_PID) ->
	2.

get_sorted_orders(TESTER) ->
	FirstOrder = #orders{direction=1,floor=1,elevatorPID = 1},
	SecondOrder= #orders{direction=2,floor=1,elevatorPID = TESTER},
	ThirdOrder= #orders{direction=1,floor=2,elevatorPID = 2},

	RecordList = [FirstOrder,SecondOrder,ThirdOrder].